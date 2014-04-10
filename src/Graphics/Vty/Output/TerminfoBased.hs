{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -D_XOPEN_SOURCE=500 #-}
{-# CFILES gwinsz.c #-}
-- |  Terminfo based terminal handling.
--
-- The color handling assumes tektronix like. No HP support provided. If the terminal is not one I
-- have easy access to then color support is entirely based of the docs. Probably with some
-- assumptions mixed in.
--
-- Copyright Corey O'Connor (coreyoconnor@gmail.com)
module Graphics.Vty.Output.TerminfoBased ( reserve_terminal )
    where

import Graphics.Vty.Prelude

import qualified Data.ByteString as BS
import Data.Terminfo.Parse
import Data.Terminfo.Eval

import Graphics.Vty.Attributes
import Graphics.Vty.DisplayAttributes
import Graphics.Vty.Output.Interface

import Blaze.ByteString.Builder (Write, writeToByteString)

import Control.Monad.Trans

import Data.Bits ((.&.))
import Data.Foldable (foldMap)
import Data.IORef
import Data.Maybe (isJust, isNothing, fromJust)
import Data.Monoid

import Foreign.C.Types ( CInt(..), CLong(..) )

import GHC.IO.Handle
#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ >= 611
import GHC.IO.Handle.Internals (withHandle_)
import GHC.IO.Handle.Types (Handle__(..))
import qualified GHC.IO.FD as FD
-- import qualified GHC.IO.Handle.FD as FD
import GHC.IO.Exception
import Data.Typeable (cast)
#else
import GHC.IOBase
import GHC.Handle hiding (fdToHandle)
import qualified GHC.Handle
#endif
#endif

import qualified System.Console.Terminfo as Terminfo
#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ >= 611
import System.IO.Error
#endif
#endif 
import System.Posix.Types (Fd(..))

data TerminfoCaps = TerminfoCaps 
    { smcup :: Maybe CapExpression
    , rmcup :: Maybe CapExpression
    , cup :: CapExpression
    , cnorm :: Maybe CapExpression
    , civis :: Maybe CapExpression
    , supports_no_colors :: Bool
    , use_alt_color_map :: Bool
    , set_fore_color :: CapExpression
    , set_back_color :: CapExpression
    , set_default_attr :: CapExpression
    , clear_screen :: CapExpression
    , clear_eol :: CapExpression
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
    
send_cap_to_terminal :: Output -> CapExpression -> [CapParam] -> IO ()
send_cap_to_terminal t cap cap_params = do
    output_byte_buffer t $ writeToByteString $ write_cap_expr cap cap_params

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
reserve_terminal :: ( Applicative m, MonadIO m ) => String -> Handle -> m Output
reserve_terminal in_ID out_handle = liftIO $ do
    ti <- Terminfo.setupTerm in_ID
    -- assumes set foreground always implies set background exists.
    -- if set foreground is not set then all color changing style attributes are filtered.
    msetaf <- probe_cap ti "setaf"
    msetf <- probe_cap ti "setf"
    let (no_colors, use_alt, set_fore_cap) 
            = case msetaf of
                Just setaf -> (False, False, setaf)
                Nothing -> case msetf of
                    Just setf -> (False, True, setf)
                    Nothing -> (True, True, error $ "no fore color support for terminal " ++ in_ID)
    msetab <- probe_cap ti "setab"
    msetb <- probe_cap ti "setb"
    let set_back_cap 
            = case msetab of
                Nothing -> case msetb of
                    Just setb -> setb
                    Nothing -> error $ "no back color support for terminal " ++ in_ID
                Just setab -> setab
    terminfo_caps <- pure TerminfoCaps
        <*> probe_cap ti "smcup"
        <*> probe_cap ti "rmcup"
        <*> require_cap ti "cup"
        <*> probe_cap ti "cnorm"
        <*> probe_cap ti "civis"
        <*> pure no_colors
        <*> pure use_alt
        <*> pure set_fore_cap
        <*> pure set_back_cap
        <*> require_cap ti "sgr0"
        <*> require_cap ti "clear"
        <*> require_cap ti "el"
        <*> current_display_attr_caps ti
    new_assumed_state_ref <- newIORef initial_assumed_state
    let t = Output
            { terminal_ID = in_ID
            , release_terminal = liftIO $ do
                send_cap set_default_attr []
                maybe_send_cap cnorm []
                hClose out_handle
            , reserve_display = liftIO $ do
                -- If there is no support for smcup: Clear the screen and then move the mouse to the
                -- home position to approximate the behavior.
                maybe_send_cap smcup []
                hFlush out_handle
                send_cap clear_screen []
            , release_display = liftIO $ do
                maybe_send_cap rmcup []
                maybe_send_cap cnorm []
            , display_bounds = do
                raw_size <- liftIO $ withFd out_handle get_window_size
                case raw_size of
                    (w, h)  | w < 0 || h < 0 -> fail $ "getwinsize returned < 0 : " ++ show raw_size
                            | otherwise      -> return (w,h)
            , output_byte_buffer = \out_bytes -> do
                BS.hPut out_handle out_bytes
                hFlush out_handle
            , context_color_count
                = case supports_no_colors terminfo_caps of
                    False -> case Terminfo.getCapability ti (Terminfo.tiGetNum "colors" ) of
                        Nothing -> 8
                        Just v -> toEnum v
                    True -> 1
            , supports_cursor_visibility = isJust $ civis terminfo_caps
            , assumed_state_ref = new_assumed_state_ref
            -- I think fix would help assure t_actual is the only reference. I was having issues
            -- tho.
            , mk_display_context = \t_actual -> liftIO . terminfo_display_context t_actual terminfo_caps
            }
        send_cap s = send_cap_to_terminal t (s terminfo_caps)
        maybe_send_cap s = when (isJust $ s terminfo_caps) . send_cap (fromJust . s)
    return t

require_cap :: (Applicative m, MonadIO m) => Terminfo.Terminal -> String -> m CapExpression
require_cap ti cap_name 
    = case Terminfo.getCapability ti (Terminfo.tiGetStr cap_name) of
        Nothing -> fail $ "Terminal does not define required capability \"" ++ cap_name ++ "\""
        Just cap_str -> parse_cap cap_str

probe_cap :: (Applicative m, MonadIO m) => Terminfo.Terminal -> String -> m (Maybe CapExpression)
probe_cap ti cap_name 
    = case Terminfo.getCapability ti (Terminfo.tiGetStr cap_name) of
        Nothing -> return Nothing
        Just cap_str -> Just <$> parse_cap cap_str

parse_cap :: (Applicative m, MonadIO m) => String -> m CapExpression
parse_cap cap_str = do
    case parse_cap_expression cap_str of
        Left e -> fail $ show e
        Right cap -> return cap

current_display_attr_caps :: ( Applicative m, MonadIO m ) 
                          => Terminfo.Terminal 
                          -> m DisplayAttrCaps
current_display_attr_caps ti 
    =   pure DisplayAttrCaps 
    <*> probe_cap ti "sgr"
    <*> probe_cap ti "smso"
    <*> probe_cap ti "rmso"
    <*> probe_cap ti "smul"
    <*> probe_cap ti "rmul"
    <*> probe_cap ti "rev"
    <*> probe_cap ti "dim"
    <*> probe_cap ti "bold"

foreign import ccall "gwinsz.h vty_c_get_window_size" c_get_window_size :: Fd -> IO CLong

get_window_size :: Fd -> IO (Int,Int)
get_window_size fd = do 
    (a,b) <- (`divMod` 65536) `fmap` c_get_window_size fd
    return (fromIntegral b, fromIntegral a)

terminfo_display_context :: Output -> TerminfoCaps -> DisplayRegion -> IO DisplayContext
terminfo_display_context t_actual terminfo_caps r = return dc
    where dc = DisplayContext
            { context_device = t_actual
            , context_region = r
            , write_move_cursor = \x y -> write_cap_expr (cup terminfo_caps) [toEnum y, toEnum x]
            , write_show_cursor = case cnorm terminfo_caps of
                Nothing -> error "this terminal does not support show cursor"
                Just c -> write_cap_expr c []
            , write_hide_cursor = case civis terminfo_caps of
                Nothing -> error "this terminal does not support hide cursor"
                Just c -> write_cap_expr c []
            , write_set_attr = terminfo_write_set_attr dc terminfo_caps
            , write_default_attr = write_cap_expr (set_default_attr terminfo_caps) []
            , write_row_end = write_cap_expr (clear_eol terminfo_caps) []
            , inline_hack = return ()
            }

-- | Portably setting the display attributes is a giant pain in the ass.
--
-- If the terminal supports the sgr capability (which sets the on/off state of each style
-- directly ; and, for no good reason, resets the colors to the default) this procedure is used: 
--
--  0. set the style attributes. This resets the fore and back color.
--  1, If a foreground color is to be set then set the foreground color
--  2. likewise with the background color
-- 
-- If the terminal does not support the sgr cap then:
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
-- This equation implements the above logic.
--
-- \todo This assumes the removal of color changes in the display attributes is done as expected
-- with no_colors == True. See `limit_attr_for_display`
--
-- \todo This assumes that fewer state changes, followed by fewer bytes, is what to optimize. I
-- haven't measured this or even examined terminal implementations. *shrug*
terminfo_write_set_attr :: DisplayContext -> TerminfoCaps -> FixedAttr -> Attr -> DisplayAttrDiff -> Write
terminfo_write_set_attr dc terminfo_caps prev_attr req_attr diffs = do
    case (fore_color_diff diffs == ColorToDefault) || (back_color_diff diffs == ColorToDefault) of
        -- The only way to reset either color, portably, to the default is to use either the set
        -- state capability or the set default capability.
        True  -> do
            case req_display_cap_seq_for (display_attr_caps terminfo_caps)
                                         (fixed_style attr )
                                         (style_to_apply_seq $ fixed_style attr) of
                -- only way to reset a color to the defaults
                EnterExitSeq caps -> write_default_attr dc
                                     `mappend` 
                                     foldMap (\cap -> write_cap_expr cap []) caps
                                     `mappend`
                                     set_colors
                -- implicitly resets the colors to the defaults
                SetState state -> write_cap_expr (fromJust $ set_attr_states 
                                                           $ display_attr_caps 
                                                           $ terminfo_caps
                                                 )
                                                 (sgr_args_for_state state)
                                  `mappend`
                                  set_colors
        -- Otherwise the display colors are not changing or changing between two non-default
        -- points.
        False -> do
            -- Still, it could be the case that the change in display attributes requires the
            -- colors to be reset because the required capability was not available.
            case req_display_cap_seq_for (display_attr_caps terminfo_caps)
                                         (fixed_style attr)
                                         (style_diffs diffs) of
                -- Really, if terminals were re-implemented with modern concepts instead of bowing
                -- down to 40 yr old dumb terminal requirements this would be the only case ever
                -- reached!  Changes the style and color states according to the differences with
                -- the currently applied states.
                EnterExitSeq caps -> foldMap (\cap -> write_cap_expr cap []) caps
                                     `mappend`
                                     write_color_diff set_fore_color (fore_color_diff diffs)
                                     `mappend`
                                     write_color_diff set_back_color (back_color_diff diffs)
                -- implicitly resets the colors to the defaults
                SetState state -> write_cap_expr (fromJust $ set_attr_states 
                                                           $ display_attr_caps terminfo_caps
                                                 )
                                                 (sgr_args_for_state state)
                                  `mappend` set_colors
    where 
        color_map = case use_alt_color_map terminfo_caps of
                        False -> ansi_color_index
                        True -> alt_color_index
        attr = fix_display_attr prev_attr req_attr
        set_colors =
            (case fixed_fore_color attr of
                Just c -> write_cap_expr (set_fore_color terminfo_caps)
                                         [toEnum $ color_map c]
                Nothing -> mempty)
            `mappend`
            (case fixed_back_color attr of
                Just c -> write_cap_expr (set_back_color terminfo_caps)
                                         [toEnum $ color_map c]
                Nothing -> mempty)
        write_color_diff _f NoColorChange
            = mempty
        write_color_diff _f ColorToDefault
            = error "ColorToDefault is not a possible case for apply_color_diffs"
        write_color_diff f (SetColor c)
            = write_cap_expr (f terminfo_caps) [toEnum $ color_map c]

-- | The color table used by a terminal is a 16 color set followed by a 240 color set that might not
-- be supported by the terminal.
--
-- This takes a Color which clearly identifies which pallete to use and computes the index
-- into the full 256 color pallete.
ansi_color_index :: Color -> Int
ansi_color_index (ISOColor v) = fromEnum v
ansi_color_index (Color240 v) = 16 + fromEnum v

-- | For terminals without setaf/setab
-- 
-- See table in `man terminfo`
-- Will error if not in table.
alt_color_index :: Color -> Int
alt_color_index (ISOColor 0) = 0
alt_color_index (ISOColor 1) = 4
alt_color_index (ISOColor 2) = 2
alt_color_index (ISOColor 3) = 6
alt_color_index (ISOColor 4) = 1
alt_color_index (ISOColor 5) = 5
alt_color_index (ISOColor 6) = 3
alt_color_index (ISOColor 7) = 7
alt_color_index (ISOColor v) = fromEnum v
alt_color_index (Color240 v) = 16 + fromEnum v

{- | The sequence of terminfo caps to apply a given style are determined according to these rules.
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
 -}
data DisplayAttrSeq
    = EnterExitSeq [CapExpression]
    | SetState DisplayAttrState

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

req_display_cap_seq_for :: DisplayAttrCaps -> Style -> [StyleStateChange] -> DisplayAttrSeq
req_display_cap_seq_for caps s diffs
    -- if the state transition implied by any diff cannot be supported with an enter/exit mode cap
    -- then either the state needs to be set or the attribute change ignored.
    = case (any no_enter_exit_cap diffs, isJust $ set_attr_states caps) of
        -- If all the diffs have an enter-exit cap then just use those
        ( False, _    ) -> EnterExitSeq $ map enter_exit_cap diffs
        -- If not all the diffs have an enter-exit cap and there is no set state cap then filter out
        -- all unsupported diffs and just apply the rest
        ( True, False ) -> EnterExitSeq $ map enter_exit_cap 
                                        $ filter (not . no_enter_exit_cap) diffs
        -- if not all the diffs have an enter-exit can and there is a set state cap then just use
        -- the set state cap.
        ( True, True  ) -> SetState $ state_for_style s
    where
        no_enter_exit_cap ApplyStandout = isNothing $ enter_standout caps
        no_enter_exit_cap RemoveStandout = isNothing $ exit_standout caps
        no_enter_exit_cap ApplyUnderline = isNothing $ enter_underline caps
        no_enter_exit_cap RemoveUnderline = isNothing $ exit_underline caps
        no_enter_exit_cap ApplyReverseVideo = isNothing $ enter_reverse_video caps
        no_enter_exit_cap RemoveReverseVideo = True
        no_enter_exit_cap ApplyBlink = True
        no_enter_exit_cap RemoveBlink = True
        no_enter_exit_cap ApplyDim = isNothing $ enter_dim_mode caps
        no_enter_exit_cap RemoveDim = True
        no_enter_exit_cap ApplyBold = isNothing $ enter_bold_mode caps
        no_enter_exit_cap RemoveBold = True
        enter_exit_cap ApplyStandout = fromJust $ enter_standout caps
        enter_exit_cap RemoveStandout = fromJust $ exit_standout caps
        enter_exit_cap ApplyUnderline = fromJust $ enter_underline caps
        enter_exit_cap RemoveUnderline = fromJust $ exit_underline caps
        enter_exit_cap ApplyReverseVideo = fromJust $ enter_reverse_video caps
        enter_exit_cap ApplyDim = fromJust $ enter_dim_mode caps
        enter_exit_cap ApplyBold = fromJust $ enter_bold_mode caps
        enter_exit_cap _ = error "enter_exit_cap applied to diff that was known not to have one."

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
style_to_apply_seq s = concat
    [ apply_if_required ApplyStandout standout
    , apply_if_required ApplyUnderline underline
    , apply_if_required ApplyReverseVideo reverse_video
    , apply_if_required ApplyBlink blink
    , apply_if_required ApplyDim dim
    , apply_if_required ApplyBlink bold
    ]
    where 
        apply_if_required op flag 
            = if 0 == (flag .&. s)
                then []
                else [op]

-- from https://patch-tag.com/r/mae/sendfile/snapshot/current/content/pretty/src/Network/Socket/SendFile/Internal.hs
-- The Fd should not be used after the action returns because the
-- Handler may be garbage collected and than will cause the finalizer
-- to close the fd.
withFd :: Handle -> (Fd -> IO a) -> IO a
#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ >= 611
withFd h f = withHandle_ "withFd" h $ \ Handle__{..} -> do
  case cast haDevice of
    Nothing -> ioError (ioeSetErrorString (mkIOError IllegalOperation
                                           "withFd" (Just h) Nothing)
                        "handle is not a file descriptor")
    Just fd -> f (Fd (fromIntegral (FD.fdFD fd)))
#else
withFd h f =
    withHandle_ "withFd" h $ \ h_ ->
      f (Fd (fromIntegral (haFD h_)))
#endif
#endif
