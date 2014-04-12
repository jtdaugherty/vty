{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{- Evaluates the paramaterized terminfo string capability with the given parameters.
 -
 -}
module Data.Terminfo.Eval (writeCapExpr)
    where

import Blaze.ByteString.Builder.Word
import Blaze.ByteString.Builder
import Data.Terminfo.Parse

import Control.Monad.Identity
import Control.Monad.State.Strict
import Control.Monad.Writer

import Data.Bits ((.|.), (.&.), xor)
import Data.List 
import Data.Word 

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
    !params <- get >>= return . evalParams
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
        s_0 = EvalState [] cap params'
    in snd $ runWriter (runStateT (writeCapOps (capOps cap)) s_0)

writeCapOps :: CapOps -> Eval ()
writeCapOps ops = mapM_ writeCapOp ops

writeCapOp :: CapOp -> Eval ()
writeCapOp (Bytes !offset !count) = do
    !cap <- get >>= return . evalExpression
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
        writeContitionalParts ((trueOps, false_ops) : falseParts) = do
            -- (man 5 terminfo)
            -- Usually the %? expr part pushes a value onto the stack, and %t pops  it  from  the
            -- stack, testing if it is nonzero (true).  If it is zero (false), control
            -- passes to the %e (else) part.
            v <- pop
            if v /= 0
                then writeCapOps trueOps
                else do
                    writeCapOps false_ops
                    writeContitionalParts falseParts

writeCapOp BitwiseOr = do
    v_0 <- pop
    v_1 <- pop
    push $ v_0 .|. v_1
writeCapOp BitwiseAnd = do
    v_0 <- pop
    v_1 <- pop
    push $ v_0 .&. v_1
writeCapOp BitwiseXOr = do
    v_1 <- pop
    v_0 <- pop
    push $ v_0 `xor` v_1
writeCapOp ArithPlus = do
    v_1 <- pop
    v_0 <- pop
    push $ v_0 + v_1
writeCapOp ArithMinus = do
    v_1 <- pop
    v_0 <- pop
    push $ v_0 - v_1
writeCapOp CompareEq = do
    v_1 <- pop
    v_0 <- pop
    push $ if v_0 == v_1 then 1 else 0
writeCapOp CompareLt = do
    v_1 <- pop
    v_0 <- pop
    push $ if v_0 < v_1 then 1 else 0
writeCapOp CompareGt = do
    v_1 <- pop
    v_0 <- pop
    push $ if v_0 > v_1 then 1 else 0

