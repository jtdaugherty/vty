{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{- Evaluates the paramaterized terminfo string capability with the given parameters.
 -
 - todo: This can be greatly simplified.
 -}
module Data.Terminfo.Eval ( cap_expression_required_bytes
                          , serialize_cap_expression
                          )
    where

import Data.ByteString.Internal ( memcpy ) 
import Data.Marshalling
import Data.Terminfo.Parse

import Control.Monad.Identity
import Control.Monad.State.Strict

import Data.Bits ( (.|.), (.&.), xor )
import Data.List 

import GHC.Prim
import GHC.Word

-- | capability evaluator state
data EvalState = EvalState
    { eval_stack :: ![ CapParam ]
    , eval_expression :: !CapExpression
    , eval_params :: ![ CapParam ]
    }

type EvalT m a = StateT EvalState m a
type Eval a = EvalT Identity a

{-# SPECIALIZE pop :: EvalT IO CapParam #-}
pop :: Monad m => EvalT m CapParam
pop = do
    s <- get
    let v : stack' = eval_stack s
        s' = s { eval_stack = stack' }
    put s'
    return v

{-# SPECIALIZE read_param :: Word -> EvalT IO CapParam #-}
read_param :: Monad m => Word -> EvalT m CapParam
read_param pn = do
    !params <- get >>= return . eval_params
    return $! genericIndex params pn

{-# SPECIALIZE push :: CapParam -> EvalT IO () #-}
push :: Monad m => CapParam -> EvalT m ()
push !v = do
    s <- get
    let s' = s { eval_stack = v : eval_stack s }
    put s'

apply_param_ops :: CapExpression -> [CapParam] -> [CapParam]
apply_param_ops cap params = foldl apply_param_op params (param_ops cap)

apply_param_op :: [CapParam] -> ParamOp -> [CapParam]
apply_param_op params IncFirstTwo = map (+ 1) params

cap_expression_required_bytes :: CapExpression -> [CapParam] -> Word
cap_expression_required_bytes cap params = 
    let params' = apply_param_ops cap params
        s_0 = EvalState [] cap params'
    in fst $! runIdentity $! runStateT ( cap_ops_required_bytes $! cap_ops cap ) s_0

cap_ops_required_bytes :: CapOps -> Eval Word
cap_ops_required_bytes ops = do
    counts <- mapM cap_op_required_bytes ops
    return $ sum counts

cap_op_required_bytes :: CapOp -> Eval Word
cap_op_required_bytes (Bytes _ _ c) = return $ toEnum c
cap_op_required_bytes DecOut = do
    p <- pop
    return $ toEnum $ length $ show p
cap_op_required_bytes CharOut = do
    _ <- pop
    return 1
cap_op_required_bytes (PushParam pn) = do
    read_param pn >>= push
    return 0
cap_op_required_bytes (PushValue v) = do
    push v
    return 0
cap_op_required_bytes (Conditional expr parts) = do
    c_expr <- cap_ops_required_bytes expr
    c_parts <- cond_parts_required_bytes parts
    return $ c_expr + c_parts
    where 
        cond_parts_required_bytes [] = return 0
        cond_parts_required_bytes ( (true_ops, false_ops) : false_parts ) = do
            -- (man 5 terminfo)
            -- Usually the %? expr part pushes a value onto the stack, and %t pops  it  from  the
            -- stack, testing if it is nonzero (true).  If it is zero (false), control
            -- passes to the %e (else) part.
            v <- pop
            c_total <- if v /= 0
                        then cap_ops_required_bytes true_ops
                        else do
                            c_false <- cap_ops_required_bytes false_ops
                            c_remain <- cond_parts_required_bytes false_parts
                            return $ c_false + c_remain
            return c_total
cap_op_required_bytes BitwiseOr = do
    v_1 <- pop
    v_0 <- pop
    push $ v_0 .|. v_1
    return 0
cap_op_required_bytes BitwiseAnd = do
    v_1 <- pop
    v_0 <- pop
    push $ v_0 .&. v_1
    return 0
cap_op_required_bytes BitwiseXOr = do
    v_1 <- pop
    v_0 <- pop
    push $ v_0 `xor` v_1
    return 0
cap_op_required_bytes ArithPlus = do
    v_1 <- pop
    v_0 <- pop
    push $ v_0 + v_1
    return 0
cap_op_required_bytes ArithMinus = do
    v_1 <- pop
    v_0 <- pop
    push $ v_0 - v_1
    return 0
cap_op_required_bytes CompareEq = do
    v_1 <- pop
    v_0 <- pop
    push $ if v_0 == v_1 then 1 else 0
    return 0
cap_op_required_bytes CompareLt = do
    v_1 <- pop
    v_0 <- pop
    push $ if v_0 < v_1 then 1 else 0
    return 0
cap_op_required_bytes CompareGt = do
    v_1 <- pop
    v_0 <- pop
    push $ if v_0 > v_1 then 1 else 0
    return 0

serialize_cap_expression :: CapExpression -> [CapParam] -> OutputBuffer -> IO OutputBuffer
serialize_cap_expression cap params out_ptr = do
    let params' = apply_param_ops cap params
        s_0 = EvalState [] cap params'
    (!out_ptr', _) <- runStateT ( serialize_cap_ops out_ptr (cap_ops cap) ) s_0
    return $! out_ptr'

serialize_cap_ops :: OutputBuffer -> CapOps -> EvalT IO OutputBuffer
serialize_cap_ops out_ptr ops = foldM serialize_cap_op out_ptr ops

serialize_cap_op :: OutputBuffer -> CapOp -> EvalT IO OutputBuffer
serialize_cap_op !out_ptr ( Bytes !offset !byte_count !next_offset ) = do
    !cap <- get >>= return . eval_expression
    let ( !start_ptr, _ ) = cap_bytes cap
        !src_ptr = start_ptr `plusPtr` offset
        !out_ptr' = out_ptr `plusPtr` next_offset
    liftIO $! memcpy out_ptr src_ptr (fromIntegral byte_count)
    return $! out_ptr'
serialize_cap_op out_ptr DecOut = do
    p <- pop
    let out_str = show p
        out_bytes = string_to_bytes out_str
    serialize_bytes out_bytes out_ptr
serialize_cap_op out_ptr CharOut = do
    W# p <- pop
    -- XXX Truncate the character value to a single byte?
    let !out_byte = W8# (and# p 0xFF##)
        !out_ptr' = out_ptr `plusPtr` 1
    liftIO $ poke out_ptr out_byte
    return out_ptr'
serialize_cap_op out_ptr (PushParam pn) = do
    read_param pn >>= push
    return out_ptr
serialize_cap_op out_ptr (PushValue v) = do
    push v
    return out_ptr
serialize_cap_op out_ptr (Conditional expr parts) = do
    out_ptr' <- serialize_cap_ops out_ptr expr
    out_ptr'' <- serialize_cond_parts out_ptr' parts
    return out_ptr''
    where 
        serialize_cond_parts ptr [] = return ptr
        serialize_cond_parts ptr ( (true_ops, false_ops) : false_parts ) = do
            -- (man 5 terminfo)
            -- Usually the %? expr part pushes a value onto the stack, and %t pops  it  from  the
            -- stack, testing if it is nonzero (true).  If it is zero (false), control
            -- passes to the %e (else) part.
            v <- pop
            ptr'' <- if v /= 0
                        then serialize_cap_ops ptr true_ops
                        else do
                            ptr' <- serialize_cap_ops ptr false_ops
                            serialize_cond_parts ptr' false_parts
            return ptr''

serialize_cap_op out_ptr BitwiseOr = do
    v_0 <- pop
    v_1 <- pop
    push $ v_0 .|. v_1
    return out_ptr
serialize_cap_op out_ptr BitwiseAnd = do
    v_0 <- pop
    v_1 <- pop
    push $ v_0 .&. v_1
    return out_ptr
serialize_cap_op out_ptr BitwiseXOr = do
    v_1 <- pop
    v_0 <- pop
    push $ v_0 `xor` v_1
    return out_ptr
serialize_cap_op out_ptr ArithPlus = do
    v_1 <- pop
    v_0 <- pop
    push $ v_0 + v_1
    return out_ptr
serialize_cap_op out_ptr ArithMinus = do
    v_1 <- pop
    v_0 <- pop
    push $ v_0 - v_1
    return out_ptr
serialize_cap_op out_ptr CompareEq = do
    v_1 <- pop
    v_0 <- pop
    push $ if v_0 == v_1 then 1 else 0
    return out_ptr
serialize_cap_op out_ptr CompareLt = do
    v_1 <- pop
    v_0 <- pop
    push $ if v_0 < v_1 then 1 else 0
    return out_ptr
serialize_cap_op out_ptr CompareGt = do
    v_1 <- pop
    v_0 <- pop
    push $ if v_0 > v_1 then 1 else 0
    return out_ptr

