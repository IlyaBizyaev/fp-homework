module Block4.Task3
  ( NonEmpty(..)
  )
where

import Control.Applicative

data NonEmpty a = a :| [a]

instance Semigroup (NonEmpty a) where
  (x :| x') <> (y :| y') = x :| (x' ++ y : y')

concat' :: [NonEmpty a] -> NonEmpty a
concat' [] = undefined
concat' (x:[]) = x
concat' (x:xs) = x <> (concat' xs)

instance Functor NonEmpty where
  fmap f (x :| y) = (f x) :| fmap f y

instance Applicative NonEmpty where
  pure x = x :| []

  (fh :| fs) <*> (xh :| xs) = (head applied) :| (tail applied) where
    applied = [f x | f <- fh:fs, x <- xh:xs]


instance Monad NonEmpty where
  (x :| xs) >>= f = (f x) <> (concat' (map f xs))

instance Foldable NonEmpty where
  foldr f z (th :| ts) = f th (foldr f z ts)

instance Traversable NonEmpty where
  traverse f (xh :| xs) = liftA2 (:|) (f xh) (traverse f xs)
