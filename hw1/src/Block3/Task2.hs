module Block3.Task2
  ( NonEmpty(..)
  , ThisOrThat(..)
  , Name(..)
  , Endo(..)
  )
where

-- | Non-empty list of arbitrary type.
data NonEmpty a = a :| [a] deriving (Show, Eq)

instance Semigroup (NonEmpty a) where
  (x :| x') <> (y :| y') = x :| (x' ++ y : y')

-- | A sample ADT with 3 constructors.
data ThisOrThat a b = This a | That b | Both a b deriving (Show, Eq)

instance Semigroup (ThisOrThat a b) where
  _ <> y = y

-- | A type for strings that should be joined with a dot.
newtype Name = Name String deriving (Show, Eq)

instance Semigroup Name where
  (Name x ) <> (Name "") = Name x
  (Name "") <> (Name y ) = Name y
  (Name x ) <> (Name y ) = Name $ x ++ '.' : y

instance Monoid Name where
  mempty = Name ""

-- | Monoid of endomorphisms under composition.
newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup (Endo a) where
  Endo f1 <> Endo f2 = Endo (f1 . f2)

instance Monoid (Endo a) where
  mempty = Endo id
