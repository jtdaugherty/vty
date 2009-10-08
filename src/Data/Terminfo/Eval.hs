{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NamedFieldPuns #-}
{- Evaluates the paramaterized terminfo string capability with the given parameters.
 -
 - todo: This can be greatly simplified.
 -}
module Data.Terminfo.Eval ( cap_expression_required_bytes
                          , serialize_cap_expression
                          , bytes_for_range
                          )
    where

import Data.Marshalling
import Data.Terminfo.Parse

import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Array.Unboxed
import Data.Bits ( (.|.) )
import Data.List 
import Data.Word

type Eval a = ReaderT (CapExpression,[CapParam]) (State [CapParam]) a
type EvalIO a = ReaderT (CapExpression,[CapParam]) (StateT [CapParam] IO) a

pop :: MonadState [CapParam] m => m CapParam
pop = do
    v : stack <- get
    put stack
    return v

read_param :: MonadReader (CapExpression, [CapParam]) m => Word -> m CapParam
read_param pn = do
    (_,params) <- ask
    return $ genericIndex params pn

push :: MonadState [CapParam] m => CapParam -> m ()
push v = do
    stack <-get
    put (v : stack)

apply_param_ops :: CapExpression -> [CapParam] -> [CapParam]
apply_param_ops cap params = foldl apply_param_op params (param_ops cap)

apply_param_op :: [CapParam] -> ParamOp -> [CapParam]
apply_param_op params IncFirstTwo = map (+ 1) params

-- | range is 0-based offset into cap_bytes and count
-- 
-- todo: The returned list is not assured to have a length st. length == count
bytes_for_range :: CapExpression -> (Word8, Word8) -> [Word8]
bytes_for_range cap (offset, count) 
    = take (fromEnum count) 
    $ drop (fromEnum offset) 
    $ elems 
    $ cap_bytes cap

cap_expression_required_bytes :: CapExpression -> [CapParam] -> Word
cap_expression_required_bytes cap params = 
    let params' = apply_param_ops cap params
    in fst $ runState (runReaderT (cap_ops_required_bytes $ cap_ops cap) (cap, params)) []

cap_ops_required_bytes :: CapOps -> Eval Word
cap_ops_required_bytes ops = do
    counts <- mapM cap_op_required_bytes ops
    return $ sum counts

cap_op_required_bytes :: CapOp -> Eval Word
cap_op_required_bytes (Bytes (_, c)) = return $ toEnum $ fromEnum c
cap_op_required_bytes DecOut = do
    p <- pop
    return $ toEnum $ length $ show p
cap_op_required_bytes (PushParam pn) = do
    read_param pn >>= push
    return 0
cap_op_required_bytes (Conditional expr parts) = do
    c_expr <- cap_ops_required_bytes expr
    c_parts <- foldM cond_parts_required_bytes 0 parts
    return $ c_expr + c_parts
    where 
        cond_parts_required_bytes in_c (true_ops, false_ops) = do
            -- (man 5 terminfo)
            -- Usually the %? expr part pushes a value onto the stack, and %t pops  it  from  the
            -- stack, testing if it is nonzero (true).  If it is zero (false), control
            -- passes to the %e (else) part.
            v <- pop
            c_branch <- if v /= 0
                            then cap_ops_required_bytes true_ops
                            else cap_ops_required_bytes false_ops
            return $ in_c + c_branch
cap_op_required_bytes BitwiseOr = do
    v_0 <- pop
    v_1 <- pop
    push $ v_0 .|. v_1
    return 0
cap_op_required_bytes CompareEq = do
    v_0 <- pop
    v_1 <- pop
    push $ if v_0 == v_1 then 1 else 0
    return 0
cap_op_required_bytes (LiteralInt i) = do
    return $ toEnum $ length $ show i

serialize_cap_expression :: CapExpression -> [CapParam] -> OutputBuffer -> IO OutputBuffer
serialize_cap_expression cap params out_ptr = do
    let params' = apply_param_ops cap params
    (out_ptr', _) <- runStateT (runReaderT (serialize_cap_ops out_ptr (cap_ops cap)) (cap, params')) []
    return out_ptr'

serialize_cap_ops :: OutputBuffer -> CapOps -> EvalIO OutputBuffer
serialize_cap_ops out_ptr ops = foldM serialize_cap_op out_ptr ops

serialize_cap_op :: OutputBuffer -> CapOp -> EvalIO OutputBuffer
serialize_cap_op out_ptr (Bytes (offset, c)) = do
    (cap, _) <- ask
    let out_bytes = bytes_for_range cap (offset, c)
    serialize_bytes out_bytes out_ptr
serialize_cap_op out_ptr DecOut = do
    p <- pop
    let out_str = show p
        out_bytes = string_to_bytes out_str
    serialize_bytes out_bytes out_ptr
serialize_cap_op out_ptr (PushParam pn) = do
    read_param pn >>= push
    return out_ptr
serialize_cap_op out_ptr (Conditional expr parts) = do
    out_ptr' <- serialize_cap_ops out_ptr expr
    out_ptr'' <- foldM serialize_cond_parts out_ptr' parts
    return out_ptr''
    where 
        serialize_cond_parts ptr (true_ops, false_ops) = do
            -- (man 5 terminfo)
            -- Usually the %? expr part pushes a value onto the stack, and %t pops  it  from  the
            -- stack, testing if it is nonzero (true).  If it is zero (false), control
            -- passes to the %e (else) part.
            v <- pop
            ptr' <- if v /= 0
                        then serialize_cap_ops ptr true_ops
                        else serialize_cap_ops ptr false_ops
            return ptr'
serialize_cap_op out_ptr BitwiseOr = do
    v_0 <- pop
    v_1 <- pop
    push $ v_0 .|. v_1
    return out_ptr
serialize_cap_op out_ptr CompareEq = do
    v_0 <- pop
    v_1 <- pop
    push $ if v_0 == v_1 then 1 else 0
    return out_ptr
serialize_cap_op out_ptr (LiteralInt i) = do
    let out_str = show i
        out_bytes = string_to_bytes out_str
    serialize_bytes out_bytes out_ptr

