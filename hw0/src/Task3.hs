module Task3
  ( composition
  , contraction
  , identity
  , permutation
  ) where

-- | S combinator.
s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

-- | B combinator from the BCKW system.
composition :: (b -> c) -> (a -> b) -> a -> c
composition = s (const s) const

-- | I combinator from the SKI system.
identity :: a -> a
identity = s const const

-- | W combinator from the BCKW system.
contraction :: (a -> a -> b) -> a -> b
contraction = s s (s const)

-- | C combinator from the BCKW system.
permutation :: (a -> b -> c) -> b -> a -> c
permutation = s (s (const (s (const s) const)) s) (const const)
