module Task5
  ( churchMult
  , churchPlus
  , churchToInt
  , succChurch
  , zero
  ) where

type Nat a = (a -> a) -> a -> a

-- | Zero constant for the Church numerals.
zero :: Nat a
zero _ x = x

-- | Successor function (increment) for the Church numerals.
succChurch :: Nat a -> Nat a
succChurch n = \f x -> f (n f x)

-- | Addition defined on the Church numerals.
churchPlus :: Nat a -> Nat a -> Nat a
churchPlus a b = \f x -> a f (b f x)

-- | Multiplication defined on the Church numerals.
churchMult :: Nat a -> Nat a -> Nat a
churchMult a b = \f x -> a (b f) x

-- | Convert a Church numeral to its integer value.
churchToInt :: Nat Integer -> Integer
churchToInt x = x (1 +) 0
