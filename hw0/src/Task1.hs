{-# LANGUAGE TypeOperators #-}

module Task1
  ( associator
  , distributivity
  , eitherAssoc
  ) where

-- | Distributivity for 'Either'.
distributivity :: Either a (b, c) -> (Either a b, Either a c)
distributivity (Left x)       = (Left x, Left x)
distributivity (Right (x, y)) = (Right x, Right y)

-- | Associator for 3-tuples.
associator :: (a, (b, c)) -> ((a, b), c)
associator (x, (y, z)) = ((x, y), z)

type (<->) a b = (a -> b, b -> a)

-- | First part of 'eitherAssoc'.
eitherAssoc1 :: Either a (Either b c) -> Either (Either a b) c
eitherAssoc1 (Left x)          = Left (Left x)
eitherAssoc1 (Right (Left y))  = Left (Right y)
eitherAssoc1 (Right (Right z)) = Right z

-- | Second part of 'eitherAssoc'.
eitherAssoc2 :: Either (Either a b) c -> Either a (Either b c)
eitherAssoc2 (Left (Left x))  = Left x
eitherAssoc2 (Left (Right y)) = Right (Left y)
eitherAssoc2 (Right z)        = Right (Right z)

-- | 2-way associator for 'Either'.
eitherAssoc :: Either a (Either b c) <-> Either (Either a b) c
eitherAssoc = (eitherAssoc1, eitherAssoc2)
