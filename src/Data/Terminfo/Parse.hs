{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NamedFieldPuns #-}
module Data.Terminfo.Parse ( module Data.Terminfo.Parse
                           , Text.Parsec.ParseError
                           )
    where

import Control.Monad ( liftM )
import Control.Monad.ST.Strict

import Data.Array.Unboxed
import Data.Array.ST
import Data.Monoid
import Data.Word

import Numeric

import Text.Parsec

type BytesLength = Word8
type BytesOffset = Word8
type CapBytes = UArray Word8 Word8

data CapExpression = CapExpression
    { cap_ops :: CapOps
    , cap_bytes :: CapBytes
    , source_string :: String
    , param_count :: Word
    , param_ops :: ParamOps
    }

type CapParam = Word

type CapOps = [CapOp]
data CapOp = 
      Bytes (BytesOffset, BytesLength)
    | DecOut
    -- This stores a 0-based index to the parameter. However the operation that implies this op is
    -- 1-based
    | PushParam !Word
    -- The conditional parts are the sequence of (%t expression, %e expression) pairs.
    -- The %e expression may be NOP
    | Conditional 
      { conditional_expr :: CapOps
      , conditional_parts :: [(CapOps, CapOps)]
      }
    | BitwiseOr
    | LiteralInt Int
    deriving ( Show )

type ParamOps = [ParamOp]
data ParamOp =
      IncFirstTwo
    deriving ( Show )

parse_cap_expression :: String -> Either ParseError CapExpression
parse_cap_expression cap_string = runST $ do
    v <- runParserT cap_expression_parser
                           initial_build_state
                           "terminfo cap" 
                           cap_string 
    case v of
        Left error -> return $ Left error
        Right build_results -> 
            return $ Right $ CapExpression
                { cap_ops = out_cap_ops build_results
                -- The cap bytes are the lower 8 bits of the input string's characters.
                -- \todo Verify the input string actually contains an 8bit byte per character.
                , cap_bytes = listArray (0, toEnum $ length cap_string - 1) 
                                        $ map (toEnum . fromEnum) cap_string
                , source_string = cap_string
                , param_count = out_param_count build_results
                , param_ops = out_param_ops build_results
                }

cap_expression_parser = do
    rs <- many $ param_escape_parser <|> bytes_op_parser 
    return $ mconcat rs

param_escape_parser = do
    char '%'
    inc_offset 1
    literal_percent_parser <|> param_op_parser 

literal_percent_parser = do
    char '%'
    start_offset <- getState >>= return . next_offset
    inc_offset 1
    return $ BuildResults 0 [Bytes (start_offset, 1)] []

param_op_parser
    = increment_op_parser 
    <|> push_op_parser
    <|> dec_out_parser
    <|> conditional_op_parser
    <|> bitwise_op_parser
    <|> literal_int_op_parser

increment_op_parser = do
    char 'i'
    inc_offset 1
    return $ BuildResults 0 [] [ IncFirstTwo ]

push_op_parser = do
    char 'p'
    param_n <- digit >>= return . (\d -> read [d])
    inc_offset 2
    return $ BuildResults param_n [ PushParam $ param_n - 1 ] []

dec_out_parser = do
    char 'd'
    inc_offset 1
    return $ BuildResults 0 [ DecOut ] []

conditional_op_parser = do
    char '?'
    inc_offset 1
    cond_part <- many_expr conditional_true_parser
    parts <- many_p 
                ( do
                    true_part <- many_expr $ choice [ try $ lookAhead conditional_end_parser
                                                    , conditional_false_parser 
                                                    ]
                    false_part <- many_expr $ lookAhead conditional_end_parser
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
        param_ops = mconcat [cond_param_ops, true_param_ops, false_param_ops]

    return $ BuildResults n'' [ Conditional cond cond_parts ] param_ops

    where 
        many_p p end = choice 
            [ try end >> return []
            , do v <- p 
                 vs <- many_p p end
                 return $ v : vs
            ]
        many_expr end = liftM mconcat $ many_p ( param_escape_parser <|> bytes_op_parser ) end

conditional_true_parser = do
    string "%t"
    inc_offset 2

conditional_false_parser = do
    string "%e"
    inc_offset 2

conditional_end_parser = do
    string "%;"
    inc_offset 2

bitwise_op_parser = do
    bitwise_or_parser

bitwise_or_parser = do
    char '|'
    inc_offset 1
    return $ BuildResults 0 [ BitwiseOr ] [ ]

literal_int_op_parser = do
    char '{'
    inc_offset 1
    n_str <- many1 digit
    inc_offset $ toEnum $ length n_str
    let n = read n_str
    char '}'
    inc_offset 1
    return $ BuildResults 0 [ LiteralInt n ] [ ]

bytes_op_parser = do
    bytes <- many1 $ satisfy (/= '%')
    start_offset <- getState >>= return . next_offset
    let !count = toEnum $ length bytes
    modifyState $ \s -> s { next_offset = start_offset + count }
    return $ BuildResults 0 [Bytes (start_offset, count)] []

data BuildState = BuildState 
    { next_offset :: Word8
    } 

inc_offset n = modifyState $ \s -> s { next_offset = next_offset s + n }

initial_build_state = BuildState 0

data BuildResults = BuildResults
    { out_param_count :: Word
    , out_cap_ops :: CapOps
    , out_param_ops :: ParamOps
    }

instance Monoid BuildResults where
    mempty = BuildResults 0 [] []
    v0 `mappend` v1 
        = BuildResults
        { out_param_count = (out_param_count v0) `max` (out_param_count v1)
        , out_cap_ops = (out_cap_ops v0) `mappend` (out_cap_ops v1)
        , out_param_ops = (out_param_ops v0) `mappend` (out_param_ops v1)
        }

