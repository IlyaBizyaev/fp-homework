module Task2
  ( doubleNeg
  , doubleNegElim
  , excludedNeg
  , pierce
  , thirdNegElim
  ) where

import Data.Void (Void)

type Neg a = a -> Void

-- | Proof of double negation.
-- doubleNeg :: a -> ((a -> Void) -> Void)
doubleNeg :: a -> Neg (Neg a)
doubleNeg f x = x f

-- | Proof of excluded negation.
-- excludedNeg :: (Either a (a -> Void) -> Void) -> Void
excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg f = (f . Right) (f . Left)

-- | Pierce's law.
-- Does not hold in intuitionistic logic, so the type is uninhabitable as per
-- the Curry-Howard isomorphism.
pierce :: ((a -> b) -> a) -> a
pierce = undefined

-- | Double negation elimination.
-- Does not hold in intuitionistic logic, so the type is uninhabitable as per
-- the Curry-Howard isomorphism.
doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

-- | Proof of third negation elimination.
-- thirdNegElim :: (((a -> Void) -> Void) -> Void) -> (a -> Void)
thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim x = x . doubleNeg
