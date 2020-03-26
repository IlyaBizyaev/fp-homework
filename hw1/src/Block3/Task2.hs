module Block3.Task2
  ( NonEmpty(..)
  , ThisOrThat(..)
  , Name(..)
  , Endo(..)
  )
where

data NonEmpty a = a :| [a] deriving (Show, Eq)

instance Semigroup (NonEmpty a) where
  (x :| x') <> (y :| y') = x :| (x' ++ y : y')


data ThisOrThat a b = This a | That b | Both a b deriving (Show, Eq)

instance Semigroup (ThisOrThat a b) where
  _ <> y = y


newtype Name = Name String deriving (Show, Eq)

instance Semigroup Name where
  (Name x ) <> (Name "") = Name x
  (Name "") <> (Name y ) = Name y
  (Name x ) <> (Name y ) = Name $ x ++ '.' : y

instance Monoid Name where
  mempty = Name ""


newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup (Endo a) where
  Endo f1 <> Endo f2 = Endo (f1 . f2)

instance Monoid (Endo a) where
  mempty = Endo id
