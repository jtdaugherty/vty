{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Evaluates the paramaterized terminfo string capability with the
-- given parameters.
module Data.Terminfo.Eval
  ( writeCapExpr
  )
where

import Blaze.ByteString.Builder.Word
import Blaze.ByteString.Builder
import Data.Terminfo.Parse

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State.Strict
import Control.Monad.Writer

import Data.Bits ((.|.), (.&.), xor)
import Data.List

import qualified Data.Vector.Unboxed as Vector

-- | capability evaluator state
data EvalState = EvalState
    { evalStack :: ![CapParam]
    , evalExpression :: !CapExpression
    , evalParams :: ![CapParam]
    }

type Eval a = StateT EvalState (Writer Write) a

pop :: Eval CapParam
pop = do
    s <- get
    let v : stack' = evalStack s
        s' = s { evalStack = stack' }
    put s'
    return v

readParam :: Word -> Eval CapParam
readParam pn = do
    !params <- evalParams <$> get
    return $! genericIndex params pn

push :: CapParam -> Eval ()
push !v = do
    s <- get
    let s' = s { evalStack = v : evalStack s }
    put s'

applyParamOps :: CapExpression -> [CapParam] -> [CapParam]
applyParamOps cap params = foldl applyParamOp params (paramOps cap)

applyParamOp :: [CapParam] -> ParamOp -> [CapParam]
applyParamOp params IncFirstTwo = map (+ 1) params

writeCapExpr :: CapExpression -> [CapParam] -> Write
writeCapExpr cap params =
    let params' = applyParamOps cap params
        s0 = EvalState [] cap params'
    in snd $ runWriter (runStateT (writeCapOps (capOps cap)) s0)

writeCapOps :: CapOps -> Eval ()
writeCapOps = mapM_ writeCapOp

writeCapOp :: CapOp -> Eval ()
writeCapOp (Bytes !offset !count) = do
    !cap <- evalExpression <$> get
    let bytes = Vector.take count $ Vector.drop offset (capBytes cap)
    Vector.forM_ bytes $ tell.writeWord8
writeCapOp DecOut = do
    p <- pop
    forM_ (show p) $ tell.writeWord8.toEnum.fromEnum
writeCapOp CharOut = do
    pop >>= tell.writeWord8.toEnum.fromEnum
writeCapOp (PushParam pn) = do
    readParam pn >>= push
writeCapOp (PushValue v) = do
    push v
writeCapOp (Conditional expr parts) = do
    writeCapOps expr
    writeContitionalParts parts
    where
        writeContitionalParts [] = return ()
        writeContitionalParts ((trueOps, falseOps) : falseParts) = do
            -- (man 5 terminfo)
            -- Usually the %? expr part pushes a value onto the stack,
            -- and %t pops it from the stack, testing if it is nonzero
            -- (true). If it is zero (false), control passes to the %e
            -- (else) part.
            v <- pop
            if v /= 0
                then writeCapOps trueOps
                else do
                    writeCapOps falseOps
                    writeContitionalParts falseParts

writeCapOp BitwiseOr = do
    v0 <- pop
    v1 <- pop
    push $ v0 .|. v1
writeCapOp BitwiseAnd = do
    v0 <- pop
    v1 <- pop
    push $ v0 .&. v1
writeCapOp BitwiseXOr = do
    v1 <- pop
    v0 <- pop
    push $ v0 `xor` v1
writeCapOp ArithPlus = do
    v1 <- pop
    v0 <- pop
    push $ v0 + v1
writeCapOp ArithMinus = do
    v1 <- pop
    v0 <- pop
    push $ v0 - v1
writeCapOp CompareEq = do
    v1 <- pop
    v0 <- pop
    push $ if v0 == v1 then 1 else 0
writeCapOp CompareLt = do
    v1 <- pop
    v0 <- pop
    push $ if v0 < v1 then 1 else 0
writeCapOp CompareGt = do
    v1 <- pop
    v0 <- pop
    push $ if v0 > v1 then 1 else 0
