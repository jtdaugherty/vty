{-# LANGUAGE BangPatterns #-}
module Graphics.Vty.AttributeChange ( module Graphics.Vty.AttributeChange
                                    )
    where

import Graphics.Vty.Attributes
import Graphics.Vty.DisplayAttributes
import Graphics.Vty.Terminal.Generic

import Control.Monad.State.Strict

import Data.Bits ( (.&.), complement )
import Data.Monoid ( mappend )

type AttrChange v = State DisplayAttrDiff v

back_color :: Color -> AttrChange ()
back_color c = modify $ flip mappend ( DisplayAttrDiff [] NoColorChange ( SetColor c ) )

fore_color :: Color -> AttrChange ()
fore_color c = modify $ flip mappend ( DisplayAttrDiff [] ( SetColor c ) NoColorChange )

apply_style :: Style -> AttrChange ()
apply_style s = modify $ flip mappend ( DisplayAttrDiff ( diff_for_apply_styles s  ) 
                                                        NoColorChange 
                                                        NoColorChange 
                                      )

remove_style :: Style -> AttrChange ()
remove_style s = modify $ flip mappend ( DisplayAttrDiff ( diff_for_remove_styles s  ) 
                                                         NoColorChange 
                                                         NoColorChange 
                                       )

diff_for_apply_styles :: Style -> [ StyleStateChange ]
diff_for_apply_styles !s 
    | s == 0               = [ ]
    | ( s .&.  standout ) /= 0      = ApplyStandout : ( diff_for_apply_styles $! s .&. ( complement standout ) )
    | ( s .&.  underline ) /= 0     = ApplyUnderline : ( diff_for_apply_styles $! s .&. ( complement underline ) )
    | ( s .&.  reverse_video ) /= 0 = ApplyReverseVideo : ( diff_for_apply_styles $! s .&. ( complement reverse_video ) )
    | ( s .&.  blink ) /= 0         = ApplyBlink : ( diff_for_apply_styles $! s .&. ( complement blink ) )
    | ( s .&.  dim ) /= 0           = ApplyDim : ( diff_for_apply_styles $! s .&. ( complement dim ) )
    | ( s .&.  bold ) /= 0          = ApplyBold : ( diff_for_apply_styles $! s .&. ( complement bold ) )
    | otherwise            = error "diff_for_apply_styles: impossible style mask"

diff_for_remove_styles :: Style -> [ StyleStateChange ]
diff_for_remove_styles !s 
    | s == 0               = [ ]
    | ( s .&.  standout ) /= 0      = RemoveStandout : ( diff_for_remove_styles $! s .&. ( complement standout ) )
    | ( s .&.  underline ) /= 0     = RemoveUnderline : ( diff_for_remove_styles $! s .&. ( complement underline ) )
    | ( s .&.  reverse_video ) /= 0 = RemoveReverseVideo : ( diff_for_remove_styles $! s .&. ( complement reverse_video ) )
    | ( s .&.  blink ) /= 0         = RemoveBlink : ( diff_for_remove_styles $! s .&. ( complement blink ) )
    | ( s .&.  dim ) /= 0           = RemoveDim : ( diff_for_remove_styles $! s .&. ( complement dim ) )
    | ( s .&.  bold ) /= 0          = RemoveBold : ( diff_for_remove_styles $! s .&. ( complement bold ) )
    | otherwise            = error "diff_for_remove_styles: impossible style mask"

default_all :: AttrChange ()
default_all = modify $ flip mappend ( DisplayAttrDiff remove_all_styles ColorToDefault ColorToDefault )

remove_all_styles :: [ StyleStateChange ]
remove_all_styles = 
    [ RemoveStandout
    , RemoveUnderline
    , RemoveReverseVideo
    , RemoveBlink
    , RemoveDim
    , RemoveBold
    ]

put_attr_change :: TerminalHandle -> AttrChange () -> IO ()
put_attr_change t c = do
    return ()

