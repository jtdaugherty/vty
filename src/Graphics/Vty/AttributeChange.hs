module Graphics.Vty.AttributeChange ( module Graphics.Vty.AttributeChange
                                    )
    where

import Graphics.Vty.Attributes
import Graphics.Vty.DisplayAttributes
import Graphics.Vty.Terminal.Generic

import Control.Monad.State.Strict

type AttrChange v = State DisplayAttrDiff v

back_color :: Color -> AttrChange ()
back_color c = modify $ \ChangeState cs -> cs `mappend` [ DisplayAttrDiff [] NoColorChange ( SetTo c ) ]

style :: Style -> AttrChange ()
style s = modify $ \ChangeState cs -> cs `mappend` [ DisplayAttrDiff  NoColorChange NoColorChange ]

diff_styles :: Style -> Style -> [StyleStateChange]

default_all :: AttrChange ()
default_all = modify $ \ChangeState cs -> cs `mappend` [ DisplayAttrDiff remove_all_styles ColorToDefault ColorToDefault ]

put_attr_change :: TerminalHandle -> AttrChange () -> IO ()
put_attr_change t c = do
    return ()

