module Block4.Task2
  ( Tree(..)
  )
where

data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a

instance Functor Tree where
  fmap f (Leaf a    ) = Leaf (f a)
  fmap f (Branch x y) = Branch (fmap f x) (fmap f y)

instance Applicative Tree where
  pure = Leaf

  Leaf f       <*> Leaf x     = Leaf $ f x
  Leaf f       <*> Branch x y = Branch (Leaf f <*> x) (Leaf f <*> y)
  Branch lf rf <*> x          = Branch (lf <*> x) (rf <*> x)

instance Foldable Tree where
  foldr f z (Leaf x    ) = f x z
  foldr f z (Branch l r) = foldr f (foldr f z r) l

instance Traversable Tree where
  traverse f (Leaf x    ) = Leaf <$> f x
  traverse f (Branch l r) = Branch <$> traverse f l <*> traverse f r
