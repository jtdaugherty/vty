module Graphics.Vty.AttributeChange ( module Graphics.Vty.AttributeChange
                                    )
    where

import Graphics.Vty.Attributes
import Graphics.Vty.DisplayAttributes
import Graphics.Vty.Terminal.Generic

import Control.Monad.State.Strict

data ChangeState = ChangeState
    { change_sequence :: ()
    }

type AttrChange v = State ChangeState v

back_color :: Color -> AttrChange ()
back_color c = return ()

style :: Style -> AttrChange ()
style s = return ()

default_all :: AttrChange ()
default_all = return ()

put_attr_change :: TerminalHandle -> AttrChange () -> IO ()
put_attr_change t c = do
    return ()

