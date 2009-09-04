{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{- From http://www.reddit.com/r/haskell/comments/8ereh/a_here_document_syntax/
 - copyright unknown?
 -}
module HereDoc (heredoc) where

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib

heredoc :: QuasiQuoter
heredoc = QuasiQuoter (litE . stringL) (litP . stringL)

