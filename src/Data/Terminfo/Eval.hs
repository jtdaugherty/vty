{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{- Evaluates the paramaterized terminfo string capability with the given parameters.
 -
 - todo: This can be greatly simplified.
 -}
module Data.Terminfo.Eval (write_cap_expr)
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
    { eval_stack :: ![CapParam]
    , eval_expression :: !CapExpression
    , eval_params :: ![CapParam]
    }

type Eval a = StateT EvalState (Writer Write) a

pop :: Eval CapParam
pop = do
    s <- get
    let v : stack' = eval_stack s
        s' = s { eval_stack = stack' }
    put s'
    return v

read_param :: Word -> Eval CapParam
read_param pn = do
    !params <- get >>= return . eval_params
    return $! genericIndex params pn

push :: CapParam -> Eval ()
push !v = do
    s <- get
    let s' = s { eval_stack = v : eval_stack s }
    put s'

apply_param_ops :: CapExpression -> [CapParam] -> [CapParam]
apply_param_ops cap params = foldl apply_param_op params (param_ops cap)

apply_param_op :: [CapParam] -> ParamOp -> [CapParam]
apply_param_op params IncFirstTwo = map (+ 1) params

write_cap_expr :: CapExpression -> [CapParam] -> Write
write_cap_expr cap params =
    let params' = apply_param_ops cap params
        s_0 = EvalState [] cap params'
    in snd $ runWriter (runStateT (write_cap_ops (cap_ops cap)) s_0)

write_cap_ops :: CapOps -> Eval ()
write_cap_ops ops = mapM_ write_cap_op ops

write_cap_op :: CapOp -> Eval ()
write_cap_op (Bytes !offset !count) = do
    !cap <- get >>= return . eval_expression
    let bytes = Vector.take count $ Vector.drop offset (cap_bytes cap)
    Vector.forM_ bytes $ tell.writeWord8
write_cap_op DecOut = do
    p <- pop
    let out_str = show p
    forM_ out_str $ tell.writeWord8.toEnum.fromEnum
write_cap_op CharOut = do
    pop >>= tell.writeWord8.toEnum.fromEnum 
write_cap_op (PushParam pn) = do
    read_param pn >>= push
write_cap_op (PushValue v) = do
    push v
write_cap_op (Conditional expr parts) = do
    write_cap_ops expr
    write_contitional_parts parts
    where 
        write_contitional_parts [] = return ()
        write_contitional_parts ((true_ops, false_ops) : false_parts) = do
            -- (man 5 terminfo)
            -- Usually the %? expr part pushes a value onto the stack, and %t pops  it  from  the
            -- stack, testing if it is nonzero (true).  If it is zero (false), control
            -- passes to the %e (else) part.
            v <- pop
            if v /= 0
                then write_cap_ops true_ops
                else do
                    write_cap_ops false_ops
                    write_contitional_parts false_parts

write_cap_op BitwiseOr = do
    v_0 <- pop
    v_1 <- pop
    push $ v_0 .|. v_1
write_cap_op BitwiseAnd = do
    v_0 <- pop
    v_1 <- pop
    push $ v_0 .&. v_1
write_cap_op BitwiseXOr = do
    v_1 <- pop
    v_0 <- pop
    push $ v_0 `xor` v_1
write_cap_op ArithPlus = do
    v_1 <- pop
    v_0 <- pop
    push $ v_0 + v_1
write_cap_op ArithMinus = do
    v_1 <- pop
    v_0 <- pop
    push $ v_0 - v_1
write_cap_op CompareEq = do
    v_1 <- pop
    v_0 <- pop
    push $ if v_0 == v_1 then 1 else 0
write_cap_op CompareLt = do
    v_1 <- pop
    v_0 <- pop
    push $ if v_0 < v_1 then 1 else 0
write_cap_op CompareGt = do
    v_1 <- pop
    v_0 <- pop
    push $ if v_0 > v_1 then 1 else 0

