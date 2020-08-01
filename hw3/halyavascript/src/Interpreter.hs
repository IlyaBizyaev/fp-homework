{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TypeFamilies      #-}

module Interpreter
  ( interpret
  )
where

import Control.Monad (join, liftM2, when)
import Control.Monad.State (State, evalState, get, modify, state)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable (Typeable, cast)

import HSExpr (HSExpr (..), HSVar, VarWrapper)


-- | Typed storage box for HalyavaScript variables.
data VarBox where
  VB ::Typeable a => a -> VarBox

-- | Map of variable reference ids to variable boxes.
type VarMap = Map Int VarBox
-- | HalyavaScript execution state.
type VarState = State VarMap

-- | Variable reference type.
data VarRef a = VarRef Int
-- | Type family instance for 'VarState's variable wrapper.
type instance VarWrapper VarState = VarRef

-- | Main interpreter function.
interpret :: VarState a -> a
interpret st = evalState st Map.empty

-- | Function that creates a new variable reference and assigns it an id.
newVarRef :: Typeable a => a -> VarState (VarRef a)
newVarRef value = state addVar
 where
  addVar :: VarMap -> (VarRef a, VarMap)
  addVar vm = (VarRef i, Map.insert i (VB value) vm) where i = Map.size vm

-- | Function that reads the variable by its reference.
readVarRef :: Typeable a => VarRef a -> VarState a
readVarRef (VarRef refId) = do
  vm <- get
  let vb = Map.lookup refId vm
  case vb of
    Just (VB x) -> case cast x of
      Nothing -> error "Cast failed"
      Just x1 -> return x1
    Nothing -> error "Nonexistent reference"

-- | Function that writes the variable by its reference.
writeVarRef :: Typeable a => VarRef a -> a -> VarState ()
writeVarRef (VarRef refId) value = modify $ Map.insert refId (VB value)

-- | Helper function for binary operator interpretation.
binWithSign :: HSVar a => (a -> a -> Bool) -> VarRef a -> a -> VarState Bool
binWithSign op ref1 val2 = do
  val1 <- readVarRef ref1
  return (op val1 val2)

instance HSExpr VarState where
  hsAssign :: HSVar a => VarRef a -> a -> VarState ()
  hsAssign = writeVarRef

  hsGt :: HSVar a => VarRef a -> a -> VarState Bool
  hsGt = binWithSign (>)

  hsGtE :: HSVar a => VarRef a -> a -> VarState Bool
  hsGtE = binWithSign (>=)

  hsLt :: HSVar a => VarRef a -> a -> VarState Bool
  hsLt = binWithSign (<)

  hsLtE :: HSVar a => VarRef a -> a -> VarState Bool
  hsLtE = binWithSign (<=)

  hsEq :: HSVar a => VarRef a -> a -> VarState Bool
  hsEq = binWithSign (==)

  hsSemicol :: VarState a -> VarState b -> VarState b
  hsSemicol = (>>)

  hsWithVar :: HSVar a => a -> (VarRef a -> VarState b) -> VarState b
  hsWithVar val f = newVarRef val >>= f

  hsWithVar2
    :: (HSVar a, HSVar c)
    => a
    -> c
    -> (VarRef a -> VarRef c -> VarState b)
    -> VarState b
  hsWithVar2 val1 val2 f = join $ liftM2 f (newVarRef val1) (newVarRef val2)

  hsReadVar :: HSVar a => VarRef a -> (a -> VarState b) -> VarState b
  hsReadVar ref f = readVarRef ref >>= f

  hsReadVar2
    :: (HSVar a, HSVar c)
    => VarRef a
    -> VarRef c
    -> (a -> c -> VarState b)
    -> VarState b
  hsReadVar2 ref1 ref2 f = join $ liftM2 f (readVarRef ref1) (readVarRef ref2)

  hsWhile :: VarState Bool -> VarState a -> VarState ()
  hsWhile cond body = do
    isExec <- cond
    when (isExec) (body >> hsWhile cond body)

  hsIfElse :: VarState Bool -> VarState a -> VarState a -> VarState a
  hsIfElse cond ifBody elseBody = do
    isExec <- cond
    if (isExec) then ifBody else elseBody

  hsFun
    :: (HSVar a, HSVar b)
    => b
    -> (VarRef a -> VarRef b -> VarState d)
    -> a
    -> VarState b
  hsFun output expr input = do
    in'  <- newVarRef input
    out' <- newVarRef output
    expr in' out' >> readVarRef out'

  hsFun2
    :: (HSVar a, HSVar b, HSVar c)
    => c
    -> (VarRef a -> VarRef b -> VarRef c -> VarState d)
    -> a
    -> b
    -> VarState c
  hsFun2 output expr input1 input2 = do
    in1' <- newVarRef input1
    in2' <- newVarRef input2
    out' <- newVarRef output
    expr in1' in2' out' >> readVarRef out'
