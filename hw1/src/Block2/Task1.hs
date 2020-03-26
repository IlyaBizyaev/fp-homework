{-# LANGUAGE InstanceSigs #-}

module Block2.Task1
  ( SearchTreeFoldable
  )
where

import           Block1.Task3                   ( SearchTree(..) )

newtype SearchTreeFoldable a = SearchTreeFoldable (SearchTree a)

-- | Convert SearchTree's internal list representation to a classic list.
replicate' :: Integer -> a -> [a]
replicate' 0 _ = []
replicate' x y = y : replicate' (x - 1) y

instance Foldable SearchTreeFoldable where
  foldMap :: Monoid m => (a -> m) -> SearchTreeFoldable a -> m
  foldMap _ (SearchTreeFoldable STLeaf) = mempty
  foldMap f (SearchTreeFoldable (STNode element cnt l r)) =
    foldMap f (SearchTreeFoldable l)
      `mappend` foldMap f (replicate' cnt element)
      `mappend` foldMap f (SearchTreeFoldable r)

  foldr :: (a -> b -> b) -> b -> SearchTreeFoldable a -> b
  foldr _ z (SearchTreeFoldable STLeaf                  ) = z
  foldr f z (SearchTreeFoldable (STNode element cnt l r)) = foldr
    f
    (foldr f (foldr f z (SearchTreeFoldable r)) (replicate' cnt element))
    (SearchTreeFoldable l)
