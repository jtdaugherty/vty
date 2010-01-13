{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -funbox-strict-fields -O #-}
module Data.Terminfo.Parse ( module Data.Terminfo.Parse
                           , Text.ParserCombinators.Parsec.ParseError
                           )
    where

import Control.Monad ( liftM )
import Control.Parallel.Strategies

import Data.Array.Unboxed
import Data.Monoid
import Data.Word

import Text.ParserCombinators.Parsec

type BytesLength = Word8
type BytesOffset = Word8
type CapBytes = UArray Word8 Word8

data CapExpression = CapExpression
    { cap_ops :: !CapOps
    , cap_bytes :: !CapBytes
    , source_string :: !String
    , param_count :: !Word
    , param_ops :: !ParamOps
    }

instance NFData CapExpression where
    rnf (CapExpression ops !_bytes !_str !_c !_p_ops) 
        = rnf ops 

type CapParam = Word

type CapOps = [CapOp]
data CapOp = 
      Bytes !BytesOffset !BytesLength
    | DecOut | CharOut
    -- This stores a 0-based index to the parameter. However the operation that implies this op is
    -- 1-based
    | PushParam !Word | PushValue !Word
    -- The conditional parts are the sequence of (%t expression, %e expression) pairs.
    -- The %e expression may be NOP
    | Conditional 
      { conditional_expr :: !CapOps
      , conditional_parts :: ![(CapOps, CapOps)]
      }
    | BitwiseOr | BitwiseXOr | BitwiseAnd
    | ArithPlus | ArithMinus
    | CompareEq | CompareLt | CompareGt
    deriving ( Show )

instance NFData CapOp where
    rnf (Bytes offset c) = rnf offset >| rnf c
    rnf (PushParam !_pn) = ()
    rnf (PushValue !_v) = ()
    rnf (Conditional c_expr c_parts) = rnf c_expr >| rnf c_parts
    rnf _ = ()

type ParamOps = [ParamOp]
data ParamOp =
      IncFirstTwo
    deriving ( Show )

parse_cap_expression :: String -> Either ParseError CapExpression
parse_cap_expression cap_string = 
    let v = runParser cap_expression_parser
                           initial_build_state
                           "terminfo cap" 
                           cap_string 
    in case v of
        Left e -> Left e
        Right build_results -> 
            Right $! ( CapExpression
                        { cap_ops = out_cap_ops build_results
                        -- The cap bytes are the lower 8 bits of the input string's characters.
                        -- \todo Verify the input string actually contains an 8bit byte per character.
                        , cap_bytes = listArray (0, toEnum $ length cap_string - 1) 
                                                $ map (toEnum . fromEnum) cap_string
                        , source_string = cap_string
                        , param_count = out_param_count build_results
                        , param_ops = out_param_ops build_results
                        } 
                        `using` rdeepseq
                     )

type CapParser a = GenParser Char BuildState a 

cap_expression_parser :: CapParser BuildResults
cap_expression_parser = do
    rs <- many $ param_escape_parser <|> bytes_op_parser 
    return $ mconcat rs

param_escape_parser :: CapParser BuildResults
param_escape_parser = do
    _ <- char '%'
    inc_offset 1
    literal_percent_parser <|> param_op_parser 

literal_percent_parser :: CapParser BuildResults
literal_percent_parser = do
    _ <- char '%'
    start_offset <- getState >>= return . next_offset
    inc_offset 1
    return $ BuildResults 0 [Bytes start_offset 1] []

param_op_parser :: CapParser BuildResults
param_op_parser
    = increment_op_parser 
    <|> push_op_parser
    <|> dec_out_parser
    <|> char_out_parser
    <|> conditional_op_parser
    <|> bitwise_op_parser
    <|> arith_op_parser
    <|> literal_int_op_parser
    <|> compare_op_parser
    <|> char_const_parser

increment_op_parser :: CapParser BuildResults
increment_op_parser = do
    _ <- char 'i'
    inc_offset 1
    return $ BuildResults 0 [] [ IncFirstTwo ]

push_op_parser :: CapParser BuildResults
push_op_parser = do
    _ <- char 'p'
    param_n <- digit >>= return . (\d -> read [d])
    inc_offset 2
    return $ BuildResults param_n [ PushParam $ param_n - 1 ] []

dec_out_parser :: CapParser BuildResults
dec_out_parser = do
    _ <- char 'd'
    inc_offset 1
    return $ BuildResults 0 [ DecOut ] []

char_out_parser :: CapParser BuildResults
char_out_parser = do
    _ <- char 'c'
    inc_offset 1
    return $ BuildResults 0 [ CharOut ] []

conditional_op_parser :: CapParser BuildResults
conditional_op_parser = do
    _ <- char '?'
    inc_offset 1
    cond_part <- many_expr conditional_true_parser
    parts <- many_p 
                ( do
                    true_part <- many_expr $ choice [ try $ lookAhead conditional_end_parser
                                                    , conditional_false_parser 
                                                    ]
                    false_part <- many_expr $ choice [ try $ lookAhead conditional_end_parser
                                                     , conditional_true_parser
                                                     ]
                    return ( true_part, false_part )
                ) 
                conditional_end_parser

    let true_parts = map fst parts
        false_parts = map snd parts
        BuildResults n cond cond_param_ops = cond_part

    let n' = maximum $ n : map out_param_count true_parts
        n'' = maximum $ n' : map out_param_count false_parts

    let true_ops = map out_cap_ops true_parts
        false_ops = map out_cap_ops false_parts
        cond_parts = zip true_ops false_ops

    let true_param_ops = mconcat $ map out_param_ops true_parts
        false_param_ops = mconcat $ map out_param_ops false_parts
        p_ops = mconcat [cond_param_ops, true_param_ops, false_param_ops]

    return $ BuildResults n'' [ Conditional cond cond_parts ] p_ops

    where 
        many_p !p !end = choice 
            [ try end >> return []
            , do !v <- p 
                 !vs <- many_p p end
                 return $! v : vs
            ]
        many_expr end = liftM mconcat $ many_p ( param_escape_parser <|> bytes_op_parser ) end

conditional_true_parser :: CapParser ()
conditional_true_parser = do
    _ <- string "%t"
    inc_offset 2

conditional_false_parser :: CapParser ()
conditional_false_parser = do
    _ <- string "%e"
    inc_offset 2

conditional_end_parser :: CapParser ()
conditional_end_parser = do
    _ <- string "%;"
    inc_offset 2

bitwise_op_parser :: CapParser BuildResults
bitwise_op_parser 
    =   bitwise_or_parser
    <|> bitwise_and_parser
    <|> bitwise_xor_parser

bitwise_or_parser :: CapParser BuildResults
bitwise_or_parser = do
    _ <- char '|'
    inc_offset 1
    return $ BuildResults 0 [ BitwiseOr ] [ ]

bitwise_and_parser :: CapParser BuildResults
bitwise_and_parser = do
    _ <- char '&'
    inc_offset 1
    return $ BuildResults 0 [ BitwiseAnd ] [ ]

bitwise_xor_parser :: CapParser BuildResults
bitwise_xor_parser = do
    _ <- char '^'
    inc_offset 1
    return $ BuildResults 0 [ BitwiseXOr ] [ ]

arith_op_parser :: CapParser BuildResults
arith_op_parser 
    =   plus_op 
    <|> minus_op 
    where
        plus_op = do
            _ <- char '+'
            inc_offset 1
            return $ BuildResults 0 [ ArithPlus ] [ ]
        minus_op = do
            _ <- char '-'
            inc_offset 1
            return $ BuildResults 0 [ ArithMinus ] [ ]

literal_int_op_parser :: CapParser BuildResults
literal_int_op_parser = do
    _ <- char '{'
    inc_offset 1
    n_str <- many1 digit
    inc_offset $ toEnum $ length n_str
    let n :: Word = read n_str
    _ <- char '}'
    inc_offset 1
    return $ BuildResults 0 [ PushValue n ] [ ]

compare_op_parser :: CapParser BuildResults
compare_op_parser 
    =   compare_eq_op
    <|> compare_lt_op 
    <|> compare_gt_op 
    where
        compare_eq_op = do
            _ <- char '='
            inc_offset 1
            return $ BuildResults 0 [ CompareEq ] [ ]
        compare_lt_op = do
            _ <- char '<'
            inc_offset 1
            return $ BuildResults 0 [ CompareLt ] [ ]
        compare_gt_op = do
            _ <- char '>'
            inc_offset 1
            return $ BuildResults 0 [ CompareGt ] [ ]

bytes_op_parser :: CapParser BuildResults
bytes_op_parser = do
    bytes <- many1 $ satisfy (/= '%')
    start_offset <- getState >>= return . next_offset
    let !c = toEnum $ length bytes
    !s <- getState
    let s' = s { next_offset = start_offset + c }
    setState s'
    return $ BuildResults 0 [Bytes start_offset c] []

char_const_parser :: CapParser BuildResults
char_const_parser = do
    _ <- char '\''
    char_value <- liftM (toEnum . fromEnum) anyChar 
    _ <- char '\''
    inc_offset 3
    return $ BuildResults 0 [ PushValue char_value ] [ ]

data BuildState = BuildState 
    { next_offset :: Word8
    } 

inc_offset :: Word8 -> CapParser ()
inc_offset n = do
    s <- getState
    let s' = s { next_offset = next_offset s + n }
    setState s'

initial_build_state :: BuildState
initial_build_state = BuildState 0

data BuildResults = BuildResults
    { out_param_count :: !Word
    , out_cap_ops :: !CapOps
    , out_param_ops :: !ParamOps
    }

instance Monoid BuildResults where
    mempty = BuildResults 0 [] []
    v0 `mappend` v1 
        = BuildResults
        { out_param_count = (out_param_count v0) `max` (out_param_count v1)
        , out_cap_ops = (out_cap_ops v0) `mappend` (out_cap_ops v1)
        , out_param_ops = (out_param_ops v0) `mappend` (out_param_ops v1)
        }

