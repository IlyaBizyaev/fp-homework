module Block1.Task2
  ( Nat(..)
  , natToInteger
  , isEven
  )
where

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
  Z    + y = y
  S x' + y = S (x' + y)

  Z      * _ = Z
  (S x') * y = y + x' * y

  Z    - S _  = error "Negative result in Nat subtraction"
  x    - Z    = x
  S x' - S y' = x' - y'

  abs = id

  signum Z     = Z
  signum (S _) = S Z

  fromInteger 0 = Z
  fromInteger x
    | x < 0     = error "Attempt to construct Nat from a negative integer"
    | otherwise = S . fromInteger $ x - 1

natToInteger :: Nat -> Integer
natToInteger Z      = 0
natToInteger (S x') = 1 + natToInteger x'

isEven :: Nat -> Bool
isEven x = natToInteger x `mod` 2 == 0

