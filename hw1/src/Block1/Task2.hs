module Block1.Task2
  ( Nat(..)
  , natToInteger
  , isEven
  )
where

-- | Natural numbers and zero.
data Nat = Z | S Nat

instance Show Nat where
  show x = show $ natToInteger x

instance Eq Nat where
  Z    == Z    = True
  Z    == _    = False
  _    == Z    = False
  S x' == S y' = x' == y'

instance Ord Nat where
  Z    <= _    = True
  S _  <= Z    = False
  S x' <= S y' = x' <= y'

instance Num Nat where
  -- | Nat addition.
  Z    + y = y
  S x' + y = S (x' + y)

  -- | Nat multiplication.
  Z      * _ = Z
  (S x') * y = y + x' * y

  -- | Nat subtraction (fails on negative results).
  Z    - S _  = error "Negative result in Nat subtraction"
  x    - Z    = x
  S x' - S y' = x' - y'

  -- | Absolute value of Nat (i.e. id).
  abs = id

  -- | Sign of Nat (0 or 1).
  signum Z     = Z
  signum (S _) = S Z

  -- | Utility function to construct Nat from a non-negative Integer.
  fromInteger 0 = Z
  fromInteger x
    | x < 0     = error "Attempt to construct Nat from a negative integer"
    | otherwise = S . fromInteger $ x - 1

-- | Utility function to convert Nat to Integer.
natToInteger :: Nat -> Integer
natToInteger Z      = 0
natToInteger (S x') = 1 + natToInteger x'

-- | Utility function to check if Nat is even.
isEven :: Nat -> Bool
isEven x = natToInteger x `mod` 2 == 0

