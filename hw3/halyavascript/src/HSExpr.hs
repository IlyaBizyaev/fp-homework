{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TypeFamilies      #-}

module HSExpr
  ( HSExpr(..)
  , HSVar(..)
  , VarWrapper
  , fun
  , fun2
  , ifElse
  , readVar
  , readVar2
  , while
  , withVar
  , withVar2
  , (@=)
  , (@>)
  , (@>=)
  , (@<)
  , (@<=)
  , (@==)
  , (#)
  )
where

import Data.Int (Int32)
import Data.Typeable (Typeable)

-- | Class to mark data types that are supported in HalyavaScript.
class (Ord a, Show a, Typeable a) => HSVar a where
  defaultVal :: a

instance HSVar Bool where
  defaultVal = False

instance HSVar Double where
  defaultVal = 0.0

instance HSVar Int32 where
  defaultVal = 0

instance HSVar String where
  defaultVal = ""

-- | Type family that enables using different variable wrappers in different
-- instances of HSExpr.
type family VarWrapper (f :: * -> *) :: * -> *

-- | Tagless final technique, applied to the HalyavaScript eDSL.
class HSExpr expr where
  hsAssign   :: HSVar a => (VarWrapper expr) a -> a -> expr ()
  hsGt       :: HSVar a => (VarWrapper expr) a -> a -> expr Bool
  hsGtE      :: HSVar a => (VarWrapper expr) a -> a -> expr Bool
  hsLt       :: HSVar a => (VarWrapper expr) a -> a -> expr Bool
  hsLtE      :: HSVar a => (VarWrapper expr) a -> a -> expr Bool
  hsEq       :: HSVar a => (VarWrapper expr) a -> a -> expr Bool
  hsSemicol  :: expr a -> expr b -> expr b
  hsWithVar  :: HSVar a => a -> ((VarWrapper expr) a -> expr b) -> expr b
  hsWithVar2 :: (HSVar a, HSVar c)
             => a -> c -> ((VarWrapper expr) a -> (VarWrapper expr) c -> expr b)
             -> expr b
  hsReadVar  :: HSVar a => (VarWrapper expr) a -> (a -> expr b) -> expr b
  hsReadVar2 :: (HSVar a, HSVar c)
             => (VarWrapper expr) a -> (VarWrapper expr) c -> (a -> c -> expr b)
             -> expr b
  hsWhile    :: expr Bool -> expr a -> expr ()
  hsIfElse   :: expr Bool -> expr a -> expr a -> expr a
  hsFun      :: (HSVar a, HSVar b)
             => b -> ((VarWrapper expr) a -> (VarWrapper expr) b -> expr d) -> a
             -> expr b
  hsFun2     :: (HSVar a, HSVar b, HSVar c)
             => c
             -> ((VarWrapper expr) a -> (VarWrapper expr) b -> (VarWrapper expr) c -> expr d)
             -> a -> b -> expr c

infixr 2 @=
infixr 2 @>
infixr 2 @>=
infixr 2 @<
infixr 2 @<=
infixr 2 @==

infixr 0 #

-- | Operator synonym for assignment.
(@=) :: (HSExpr expr, HSVar a) => (VarWrapper expr) a -> a -> expr ()
(@=) = hsAssign

-- | Operator synonym for "greater than".
(@>) :: (HSExpr expr, HSVar a) => (VarWrapper expr) a -> a -> expr Bool
(@>) = hsGt

-- | Operator synonym for "greater than or equal to".
(@>=) :: (HSExpr expr, HSVar a) => (VarWrapper expr) a -> a -> expr Bool
(@>=) = hsGtE

-- | Operator synonym for "less than".
(@<) :: (HSExpr expr, HSVar a) => (VarWrapper expr) a -> a -> expr Bool
(@<) = hsLt

-- | Operator synonym for "less than or equal to".
(@<=) :: (HSExpr expr, HSVar a) => (VarWrapper expr) a -> a -> expr Bool
(@<=) = hsLtE

-- | Operator synonym for "equal to".
(@==) :: (HSExpr expr, HSVar a) => (VarWrapper expr) a -> a -> expr Bool
(@==) = hsEq

-- | Operator synonym for a semicolon-like statement join.
(#) :: HSExpr expr => expr a -> expr b -> expr b
(#) = hsSemicol

-- | Convenient synonym for a variable scope.
withVar
  :: (HSExpr expr, HSVar a) => a -> ((VarWrapper expr) a -> expr b) -> expr b
withVar = hsWithVar

-- | Convenient synonym for a 2-variable scope.
withVar2
  :: (HSExpr expr, HSVar a, HSVar c)
  => a
  -> c
  -> ((VarWrapper expr) a -> (VarWrapper expr) c -> expr b)
  -> expr b
withVar2 = hsWithVar2

-- | Convenient synonym for variable access.
readVar
  :: (HSExpr expr, HSVar a) => (VarWrapper expr) a -> (a -> expr b) -> expr b
readVar = hsReadVar

-- | Convenient synonym for 2-variable access.
readVar2
  :: (HSExpr expr, HSVar a, HSVar c)
  => (VarWrapper expr) a
  -> (VarWrapper expr) c
  -> (a -> c -> expr b)
  -> expr b
readVar2 = hsReadVar2

-- | Convenient synonym for a while loop.
while :: HSExpr expr => expr Bool -> expr a -> expr ()
while = hsWhile

-- | Convenient synonym for an if-else statement.
ifElse :: HSExpr expr => expr Bool -> expr a -> expr a -> expr a
ifElse = hsIfElse

-- | Convenient synonym for a one-argument function.
fun
  :: (HSExpr expr, HSVar a, HSVar b)
  => b
  -> ((VarWrapper expr) a -> (VarWrapper expr) b -> expr d)
  -> a
  -> expr b
fun = hsFun

-- | Convenient synonym for a two-argument function.
fun2
  :: (HSExpr expr, HSVar a, HSVar b, HSVar c)
  => c
  -> (  (VarWrapper expr) a
     -> (VarWrapper expr) b
     -> (VarWrapper expr) c
     -> expr d
     )
  -> a
  -> b
  -> expr c
fun2 = hsFun2
