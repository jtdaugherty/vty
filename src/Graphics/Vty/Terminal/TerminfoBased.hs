-- Copyright 2009 Corey O'Connor
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Graphics.Vty.Terminal.TerminfoBased ( terminal_instance
                                           )
    where

import Data.Terminfo.Parse
import Data.Terminfo.Eval

import Graphics.Vty.Attributes
import Graphics.Vty.Terminal.Generic
import Graphics.Vty.DisplayRegion

import Data.Word

import Foreign.C.Types ( CLong )

import qualified System.Console.Terminfo as Terminfo
import System.IO

data Term = Term 
    { term_info_ID :: String
    , smcup :: CapExpression
    , rmcup :: CapExpression
    , cup :: CapExpression
    , cnorm :: CapExpression
    , civis :: CapExpression
    , set_fore_color :: CapExpression
    , set_back_color :: CapExpression
    , set_default_attr :: CapExpression
    , set_attr :: CapExpression
    }

marshall_cap_to_terminal :: Term -> (Term -> CapExpression) -> [CapParam] -> IO ()
marshall_cap_to_terminal t cap_selector cap_params = do
    marshall_to_terminal t (cap_expression_required_bytes (cap_selector t) cap_params)
                           (serialize_cap_expression (cap_selector t) cap_params)
    return ()

{- | Uses terminfo for all control codes. While this should provide the most compatible terminal
 - terminfo does not support some features that would increase efficiency and improve compatibility:
 -  * determine the character encoding supported by the terminal. Should this be taken from the LANG
 - environment variable?  
 -  * Provide independent string capabilities for all display attributes.
 -
 - 
 - todo: Some display attributes like underline and bold have independent string capabilities that
 - should be used instead of the generic "sgr" string capability.
 -}
terminal_instance :: String -> IO Term
terminal_instance in_ID = do
    term_info <- Terminfo.setupTerm in_ID
    let str_cap str = case Terminfo.getCapability term_info (Terminfo.tiGetStr str) of
                        Nothing -> fail $ "Terminal does not define required capability \"" ++ str ++ "\""
                        Just cap_str -> case parse_cap_expression cap_str of
                            Left e -> fail $ show e
                            Right cap -> return cap
    smcup_cap <- str_cap "smcup"
    rmcup_cap <- str_cap "rmcup"
    cup_cap <- str_cap "cup"
    cnorm_cap <- str_cap "cnorm"
    civis_cap <- str_cap "civis"
    setaf_cap <- str_cap "setaf"
    setab_cap <- str_cap "setab"
    sgr0_cap <- str_cap "sgr0"
    sgr_cap <- str_cap "sgr"
    return $ Term in_ID 
                smcup_cap 
                rmcup_cap 
                cup_cap 
                cnorm_cap 
                civis_cap
                setaf_cap
                setab_cap
                sgr0_cap
                sgr_cap

instance Terminal Term where
    terminal_ID t = term_info_ID t ++ " :: TerminfoBased"

    release_terminal _t = do 
        return ()

    reserve_display t = do
        marshall_cap_to_terminal t smcup []
        marshall_cap_to_terminal t cup [0, 0]
        return ()

    release_display t = do
        marshall_cap_to_terminal t rmcup []
        marshall_cap_to_terminal t cnorm []
        return ()

    display_terminal_instance t b c = do
        return $ c (DisplayContext b t)

    display_bounds _t = do
        raw_size <- get_window_size
        case raw_size of
            ( w, h )    | w < 0 || h < 0 -> fail $ "getwinsize returned < 0 : " ++ show raw_size
                        | otherwise      -> return $ DisplayRegion (toEnum w) (toEnum h)

    -- Output the byte buffer of the specified size to the terminal device.
    output_byte_buffer _t out_ptr out_byte_count = do
        hPutBuf stdout out_ptr (fromEnum out_byte_count) 
        hFlush stdout

foreign import ccall "gwinsz.h c_get_window_size" c_get_window_size :: IO CLong

get_window_size :: IO (Int,Int)
get_window_size = do 
    (a,b) <- (`divMod` 65536) `fmap` c_get_window_size
    return (fromIntegral b, fromIntegral a)

data DisplayContext = DisplayContext
    { bounds :: DisplayRegion
    , term :: Term
    }

instance DisplayTerminal DisplayContext where
    context_region d = bounds d

    move_cursor_required_bytes d x y 
        = cap_expression_required_bytes (cup $ term d) [y, x]
    serialize_move_cursor d x y out_ptr 
        = serialize_cap_expression (cup $ term d) [y, x] out_ptr

    show_cursor_required_bytes d 
        = cap_expression_required_bytes (cnorm $ term d) []
    serialize_show_cursor d out_ptr 
        = serialize_cap_expression (cnorm $ term d) [] out_ptr

    hide_cursor_required_bytes d 
        = cap_expression_required_bytes (civis $ term d) []
    serialize_hide_cursor d out_ptr 
        = serialize_cap_expression (civis $ term d) [] out_ptr

    --  0. set the style attributes. This resets the fore and back color.
    --  1, If a foreground color is to be set then set the foreground color
    --  2. likewise with the background color
    --
    -- 
    -- todo: This process is accurate but inefficient. Optimize later.
    attr_required_bytes d prev_attr req_attr = 
        let attr = fix_display_attr prev_attr req_attr
        in cap_expression_required_bytes ( set_attr $ term d )
                                         ( sgr_args_for_style (fixed_style attr) )
        +  case fixed_fore_color attr of
            Just c -> cap_expression_required_bytes ( set_fore_color $ term d ) 
                                                    [ ansi_color_index c ]
            Nothing -> 0
        +  case fixed_back_color attr of
            Just c -> cap_expression_required_bytes ( set_back_color $ term d ) 
                                                    [ ansi_color_index c ]
            Nothing -> 0

    serialize_set_attr d prev_attr req_attr out_ptr = do
        let attr = fix_display_attr prev_attr req_attr
        out_ptr' <- serialize_cap_expression ( set_attr $ term d ) 
                                             ( sgr_args_for_style (fixed_style attr) )
                                             out_ptr
        out_ptr'' <- case fixed_fore_color attr of
            Just c -> serialize_cap_expression ( set_fore_color $ term d ) 
                                               [ ansi_color_index c ]
                                               out_ptr'
            Nothing -> return out_ptr'
        out_ptr''' <- case fixed_back_color attr of
            Just c -> serialize_cap_expression ( set_back_color $ term d ) 
                                               [ ansi_color_index c ]
                                               out_ptr''
            Nothing -> return out_ptr''
        return out_ptr'''

    default_attr_required_bytes d 
        = cap_expression_required_bytes (set_default_attr $ term d) []
    serialize_default_attr d out_ptr = do
        out_ptr' <- serialize_cap_expression (set_default_attr $ term d) [] out_ptr
        return out_ptr'

ansi_color_index :: Color -> Word
ansi_color_index (ISOColor v) = toEnum $ fromEnum v
ansi_color_index (Color240 v) = 16 + ( toEnum $ fromEnum v )

sgr_args_for_style :: Style -> [CapParam]
sgr_args_for_style s = map ( \b -> if b then 1 else 0 )
    [ s `has_style` standout 
    , s `has_style` underline
    , s `has_style` reverse_video
    , s `has_style` blink
    , s `has_style` dim
    , s `has_style` bold
    , False -- invis
    , False -- protect
    , False -- alt char set
    ]

