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
import Graphics.Vty.DisplayAttributes
import Graphics.Vty.Terminal.Generic
import Graphics.Vty.DisplayRegion

import Control.Applicative 
import Control.Monad ( foldM )
import Control.Monad.Trans

import Data.Bits ( (.&.) )
import Data.Maybe ( fromJust )
import Data.Monoid 
import Data.Word

import Foreign.C.Types ( CLong )

import qualified System.Console.Terminfo as Terminfo
import System.IO

data Term = Term 
    { term_info_ID :: String
    , term_info :: Terminfo.Terminal
    , smcup :: Maybe CapExpression
    , rmcup :: Maybe CapExpression
    , cup :: CapExpression
    , cnorm :: CapExpression
    , civis :: CapExpression
    , set_fore_color :: CapExpression
    , set_back_color :: CapExpression
    , set_default_attr :: CapExpression
    , clear_screen :: CapExpression
    , display_attr_caps :: DisplayAttrCaps
    }

data DisplayAttrCaps = DisplayAttrCaps
    { set_attr_states :: Maybe CapExpression
    , enter_standout :: Maybe CapExpression
    , exit_standout :: Maybe CapExpression
    , enter_underline :: Maybe CapExpression
    , exit_underline :: Maybe CapExpression
    , enter_reverse_video :: Maybe CapExpression
    , enter_dim_mode :: Maybe CapExpression
    , enter_bold_mode :: Maybe CapExpression
    }
    
marshall_cap_to_terminal :: MonadIO m => Term -> (Term -> CapExpression) -> [CapParam] -> m ()
marshall_cap_to_terminal t cap_selector cap_params = do
    marshall_to_terminal t ( cap_expression_required_bytes (cap_selector t) cap_params )
                           ( serialize_cap_expression (cap_selector t) cap_params )
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
terminal_instance :: ( Applicative m, MonadIO m ) => String -> m Term
terminal_instance in_ID = do
    ti <- liftIO $ Terminfo.setupTerm in_ID
    let require_cap str 
            = case Terminfo.getCapability ti (Terminfo.tiGetStr str) of
                Nothing -> fail $ "Terminal does not define required capability \"" ++ str ++ "\""
                Just cap_str -> case parse_cap_expression cap_str of
                    Left e -> fail $ show e
                    Right cap -> return cap
        probe_cap cap_name 
            = case Terminfo.getCapability ti (Terminfo.tiGetStr cap_name) of
                Nothing -> return Nothing
                Just cap_str -> case parse_cap_expression cap_str of
                    Left e -> fail $ show e
                    Right cap -> return $ Just cap
    pure Term
        <*> pure in_ID
        <*> pure ti
        <*> probe_cap "smcup"
        <*> probe_cap "rmcup"
        <*> require_cap "cup"
        <*> require_cap "cnorm"
        <*> require_cap "civis"
        <*> require_cap "setaf"
        <*> require_cap "setab"
        <*> require_cap "sgr0"
        <*> require_cap "clear"
        <*> current_display_attr_caps ti

current_display_attr_caps :: ( Applicative m, MonadIO m ) 
                          => Terminfo.Terminal 
                          -> m DisplayAttrCaps
current_display_attr_caps ti 
    =   pure DisplayAttrCaps 
    <*> probe_cap "sgr"
    <*> probe_cap "smso"
    <*> probe_cap "rmso"
    <*> probe_cap "smul"
    <*> probe_cap "rmul"
    <*> probe_cap "rev"
    <*> probe_cap "dim"
    <*> probe_cap "bold"
    where probe_cap cap_name 
            = case Terminfo.getCapability ti (Terminfo.tiGetStr cap_name) of
                Nothing -> return Nothing
                Just cap_str -> case parse_cap_expression cap_str of
                    Left e -> fail $ show e
                    Right cap -> return $ Just cap

instance Terminal Term where
    terminal_ID t = term_info_ID t ++ " :: TerminfoBased"

    release_terminal _t = do 
        return ()

    reserve_display t = do
        if (isJust $ smcup t)
            then marshall_cap_to_terminal t (fromJust . smcup) []
            else return ()
        -- Screen on OS X does not appear to support smcup?
        marshall_cap_to_terminal t clear_screen []
        return ()

    release_display t = do
        if (isJust $ rmcup t)
            then marshall_cap_to_terminal t (fromJust . rmcup) []
            else return ()
        marshall_cap_to_terminal t cnorm []
        return ()

    display_terminal_instance t b c = do
        let color_count 
                = case Terminfo.getCapability (term_info t) (Terminfo.tiGetNum "colors" ) of
                    Nothing -> 8
                    Just v -> toEnum v
        return $ c (DisplayContext b t color_count)

    display_bounds _t = do
        raw_size <- liftIO $ get_window_size
        case raw_size of
            ( w, h )    | w < 0 || h < 0 -> fail $ "getwinsize returned < 0 : " ++ show raw_size
                        | otherwise      -> return $ DisplayRegion (toEnum w) (toEnum h)

    -- Output the byte buffer of the specified size to the terminal device.
    output_byte_buffer _t out_ptr out_byte_count = do
        liftIO $ hPutBuf stdout out_ptr (fromEnum out_byte_count) 
        liftIO $ hFlush stdout

foreign import ccall "gwinsz.h c_get_window_size" c_get_window_size :: IO CLong

get_window_size :: IO (Int,Int)
get_window_size = do 
    (a,b) <- (`divMod` 65536) `fmap` c_get_window_size
    return (fromIntegral b, fromIntegral a)

data DisplayContext = DisplayContext
    { bounds :: DisplayRegion
    , term :: Term
    , supported_colors :: Word
    }

instance DisplayTerminal DisplayContext where
    context_region d = bounds d

    context_color_count d = supported_colors d

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

    -- Instead of evaluating all the rules related to setting display attributes twice (once in
    -- required bytes and again in serialize) or some memoization scheme just return a size
    -- requirement as big the longest possible control string.
    -- 
    -- Which is assumed to the be less than 512 for now. 
    --
    -- \todo Not verified as safe and wastes memory.
    attr_required_bytes _d _prev_attr _req_attr _diffs = 512

    -- Portably setting the display attributes is a giant pain in the ass.
    -- If the terminal supports the sgr capability (which sets the on/off state of each style
    -- directly ; and, for no good reason, resets the colors to the default) this always works:
    --  0. set the style attributes. This resets the fore and back color.
    --  1, If a foreground color is to be set then set the foreground color
    --  2. likewise with the background color
    -- 
    -- If the terminal does not support the sgr then 
    --  if there is a change from an applied color to the default (in either the fore or back color)
    --  then:
    --      0. reset all display attributes (sgr0)
    --      1. enter required style modes
    --      2. set the fore color if required
    --      3. set the back color if required
    --
    -- Entering the required style modes could require a reset of the display attributes. If this is
    -- the case then the back and fore colors always need to be set if not default.
    --
    -- All this (I think) is satisfied by the following logic:
    serialize_set_attr d prev_attr req_attr diffs out_ptr = do
        case (fore_color_diff diffs == ColorToDefault) || (back_color_diff diffs == ColorToDefault) of
            -- The only way to reset either color, portably, to the default is to use either the set
            -- state capability or the set default capability.
            True  -> do
                case req_display_cap_seq_for ( display_attr_caps $ term d )
                                             ( fixed_style attr )
                                             ( style_to_apply_seq $ fixed_style attr )
                    of
                        EnterExitSeq caps 
                            -- only way to reset a color to the defaults
                            ->  serialize_default_attr d out_ptr
                            >>= (\out_ptr' -> foldM (\ptr cap -> serialize_cap_expression cap [] ptr) out_ptr' caps)
                            >>= set_colors
                        SetState state
                            -- implicitly resets the colors to the defaults
                            ->  serialize_cap_expression ( fromJust $ set_attr_states 
                                                                    $ display_attr_caps 
                                                                    $ term d 
                                                         )
                                                         ( sgr_args_for_state state )
                                                         out_ptr
                            >>= set_colors
            -- Otherwise the display colors are not changing or changing between two non-default
            -- points.
            False -> do
                -- Still, it could be the case that the change in display attributes requires the
                -- colors to be reset because the required capability was not available.
                case req_display_cap_seq_for ( display_attr_caps $ term d )
                                             ( fixed_style attr )
                                             ( style_diffs diffs )
                    of
                        -- Really, if terminals were re-implemented with modern concepts instead of
                        -- bowing down to 40 yr old dumb terminal requirements this would be the
                        -- only case ever reached! 
                        -- Changes the style and color states according to the differences with the
                        -- currently applied states.
                        EnterExitSeq caps 
                            ->   foldM (\ptr cap -> serialize_cap_expression cap [] ptr) out_ptr caps
                            >>=  apply_color_diff set_fore_color ( fore_color_diff diffs )
                            >>=  apply_color_diff set_back_color ( back_color_diff diffs )
                        SetState state
                            -- implicitly resets the colors to the defaults
                            ->  serialize_cap_expression ( fromJust $ set_attr_states 
                                                                    $ display_attr_caps
                                                                    $ term d
                                                         )
                                                         ( sgr_args_for_state state )
                                                         out_ptr
                            >>= set_colors
        where 
            attr = fix_display_attr prev_attr req_attr
            set_colors ptr = do
                ptr' <- case fixed_fore_color attr of
                    Just c -> serialize_cap_expression ( set_fore_color $ term d ) 
                                                       [ ansi_color_index c ]
                                                       ptr
                    Nothing -> return ptr
                ptr'' <- case fixed_back_color attr of
                    Just c -> serialize_cap_expression ( set_back_color $ term d ) 
                                                       [ ansi_color_index c ]
                                                       ptr'
                    Nothing -> return ptr'
                return ptr''
            apply_color_diff _f NoColorChange ptr
                = return ptr
            apply_color_diff _f ColorToDefault _ptr
                = fail "ColorToDefault is not a possible case for apply_color_diffs"
            apply_color_diff f ( SetColor c ) ptr
                = serialize_cap_expression ( f $ term d ) 
                                           [ ansi_color_index c ]
                                           ptr

    default_attr_required_bytes d 
        = cap_expression_required_bytes (set_default_attr $ term d) []
    serialize_default_attr d out_ptr = do
        out_ptr' <- serialize_cap_expression (set_default_attr $ term d) [] out_ptr
        return out_ptr'

ansi_color_index :: Color -> Word
ansi_color_index (ISOColor v) = toEnum $ fromEnum v
ansi_color_index (Color240 v) = 16 + ( toEnum $ fromEnum v )

{- The sequence of terminfo caps to apply a given style are determined according to these rules.
 -
 -  1. The assumption is that it's preferable to use the simpler enter/exit mode capabilities than
 -  the full set display attribute state capability. 
 -
 -  2. If a mode is supposed to be removed but there is not an exit capability defined then the
 -  display attributes are reset to defaults then the display attribute state is set.
 -
 -  3. If a mode is supposed to be applied but there is not an enter capability defined then then
 -  display attribute state is set if possible. Otherwise the mode is not applied.
 -
 -  4. If the display attribute state is being set then just update the arguments to that for any
 -  apply/remove.
 -
 -  The style diffs each imply either the enter/exit control code or a reset to defaults ; set state
 -  sequence. This mapping satisfies the communitive monoid properties:
 -      - no diff * diff == diff
 -      - diff * no diff == diff
 -      - (diff_0 * diff_1) * diff_2 == diff_0 * ( diff_1 * diff_2 )
 -      - diff_0 * diff_1 == diff_1 * diff_0
 -  so the mapping is a sequence of mappend's applied to mempty. Where the monoid points appended
 -  depend on the diff. The accumulated value is the monad point that represents the final sequence
 -  to apply. The application (*) operator assures all the rules are followed.
 -
 -  The diff implies an enter/exit control code if:
 -      - The current 
 -}

data DisplayAttrSeq v
    = EnterExitSeq [v]
    | SetState DisplayAttrState

instance Monoid (DisplayAttrSeq v) where
    mempty = EnterExitSeq []
    SetState s `mappend` _ = SetState s
    _ `mappend` SetState s = SetState s
    (EnterExitSeq caps_0) `mappend` (EnterExitSeq caps_1) = EnterExitSeq (caps_0 `mappend` caps_1)

data DisplayAttrState = DisplayAttrState
    { apply_standout :: Bool
    , apply_underline :: Bool
    , apply_reverse_video :: Bool
    , apply_blink :: Bool
    , apply_dim :: Bool
    , apply_bold :: Bool
    }

sgr_args_for_state :: DisplayAttrState -> [CapParam]
sgr_args_for_state attr_state = map (\b -> if b then 1 else 0)
    [ apply_standout attr_state
    , apply_underline attr_state
    , apply_reverse_video attr_state
    , apply_blink attr_state
    , apply_dim attr_state
    , apply_bold attr_state
    , False -- invis
    , False -- protect
    , False -- alt char set
    ]

req_display_cap_seq_for :: DisplayAttrCaps -> Style -> [StyleStateChange] -> DisplayAttrSeq CapExpression
req_display_cap_seq_for caps s diffs =
    -- First pass: concat the monoid points that are implied by the diffs
    let base = mconcat $ map diff_point diffs
    -- Second pass: Apply the capability restrictions. 
    in apply_caps base
    where 
        set_state = SetState $ state_for_style s
        diff_point ApplyStandout = EnterExitSeq [ApplyStandout]
        diff_point ApplyUnderline = EnterExitSeq [ApplyUnderline] 
        diff_point ApplyReverseVideo = EnterExitSeq [ApplyReverseVideo]
        diff_point ApplyBlink = set_state
        diff_point ApplyDim = EnterExitSeq [ApplyDim]
        diff_point ApplyBold = EnterExitSeq [ApplyBold]
        diff_point RemoveStandout = EnterExitSeq [RemoveStandout]
        diff_point RemoveUnderline = EnterExitSeq [RemoveUnderline]
        diff_point RemoveReverseVideo = set_state
        diff_point RemoveBlink = set_state
        diff_point RemoveDim = set_state
        diff_point RemoveBold = set_state
        apply_caps ( SetState _ )
            = case set_attr_states caps of
                Nothing -> EnterExitSeq []
                Just _ -> set_state
        apply_caps (EnterExitSeq []) 
            = EnterExitSeq []
        apply_caps (EnterExitSeq (diff : diffs'))
            = case apply_caps' diff of
                SetState _ -> set_state
                p          -> p `mappend` apply_caps (EnterExitSeq diffs')
        apply_caps' ApplyStandout = m $ enter_standout caps
        apply_caps' ApplyUnderline = m $ enter_underline caps
        apply_caps' ApplyReverseVideo = m $ enter_reverse_video caps
        apply_caps' ApplyBlink = set_state
        apply_caps' ApplyDim = m $ enter_dim_mode caps
        apply_caps' ApplyBold = m $ enter_bold_mode caps
        apply_caps' RemoveStandout = m $ exit_standout caps
        apply_caps' RemoveUnderline = m $ exit_underline caps
        apply_caps' RemoveReverseVideo = set_state
        apply_caps' RemoveBlink = set_state
        apply_caps' RemoveDim = set_state
        apply_caps' RemoveBold = set_state
        m = maybe set_state (EnterExitSeq . return)

{-
set_style_required_bytes caps style diffs reset_cap = 
    let state = state_for_style style
        req_seq = req_seq_for caps style diffs
    in case req_seq of
        EnterExitSeq caps -> sum $ map (\cap -> cap_expression_required_bytes cap []) caps
        SetState -> cap_expression_required_bytes ( fromJust $ set_attr_states $ caps )
                                                  ( sgr_args_for_state state )
-}

state_for_style :: Style -> DisplayAttrState
state_for_style s = DisplayAttrState
    { apply_standout = is_style_set standout
    , apply_underline = is_style_set underline
    , apply_reverse_video = is_style_set reverse_video
    , apply_blink = is_style_set blink
    , apply_dim = is_style_set dim
    , apply_bold = is_style_set bold
    }
    where is_style_set = has_style s

style_to_apply_seq :: Style -> [StyleStateChange]
style_to_apply_seq s = mconcat
    [ apply_if_required ApplyStandout standout
    , apply_if_required ApplyUnderline underline
    , apply_if_required ApplyReverseVideo reverse_video
    , apply_if_required ApplyBlink blink
    , apply_if_required ApplyDim dim
    , apply_if_required ApplyBlink bold
    ]
    where 
        apply_if_required ap flag 
            = if 0 == ( flag .&. s )
                then []
                else [ ap ]

