module Block3.Task1
  ( maybeConcat
  , eitherConcat
  )
where

maybeConcat :: [Maybe [a]] -> [a]
maybeConcat []             = []
maybeConcat (Nothing : xs) = maybeConcat xs
maybeConcat (Just x  : xs) = x ++ (maybeConcat xs)

eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat xs = foldl f (mempty, mempty) xs where
  f (lefts, rights) (Left  l) = (lefts `mappend` l, rights)
  f (lefts, rights) (Right r) = (lefts, rights `mappend` r)
