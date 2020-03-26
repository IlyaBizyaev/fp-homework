module Block3.Task1
  ( maybeConcat
  , eitherConcat
  )
where

-- | Concatenate sublists from a list of 'Maybe' values.
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat []             = []
maybeConcat (Nothing : xs) = maybeConcat xs
maybeConcat (Just x  : xs) = x ++ (maybeConcat xs)

-- | Append monoidal elements from a list of Either, splitting by Left/Right.
eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat xs = foldl f (mempty, mempty) xs where
  f (lefts, rights) (Left  l) = (lefts `mappend` l, rights)
  f (lefts, rights) (Right r) = (lefts, rights `mappend` r)
