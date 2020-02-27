{-# LANGUAGE ScopedTypeVariables #-}

module Task7
  ( isDorianGreyNull
  , trueForAnyInteger
  , zipIntegersFromEither
  ) where

import Data.Either (lefts, rights)

-- | Result of a clumsy check for whether or not the string "Dorian Grey"
-- is empty.
-- Equals 'False'.
isDorianGreyNull :: Bool
isDorianGreyNull =
  let
    myUncurry                = uncurry :: (a -> b -> c) -> (a, b) -> c
    myId                     = id :: a -> a
    uncurryId                = myUncurry myId :: (b -> c, b) -> c
    strDorian                = "Dorian " :: String
    strGrey                  = " Grey" :: String
    myConcat                 = (++) :: [a] -> [a] -> [a]
    concatWithDorian         = myConcat strDorian :: String -> String
    pairDorianGrey = (concatWithDorian, strGrey) :: (String -> String, String)
    myNull                   = null :: [a] -> Bool
    myHead                   = head :: [a] -> a
    myMap                    = map :: (a -> b) -> [a] -> [b]
    listPairConcatDorianGrey = [pairDorianGrey] :: [(String -> String, String)]
    myDot                    = (.) :: (b -> c) -> (a -> b) -> a -> c
    composeWithNull          = myDot myNull :: (a -> [b]) -> a -> Bool
    nullHead                 = composeWithNull myHead :: [[a]] -> Bool
    mapUncurryId             = myMap uncurryId :: [(a -> b, a)] -> [b]
    listDorianGrey           = mapUncurryId listPairConcatDorianGrey :: [String]
    myDollar                 = ($) :: (a -> b) -> a -> b
    applyNullHead            = myDollar nullHead :: [[a]] -> Bool
  in
    applyNullHead listDorianGrey :: Bool

-- | Equals [(3, 64)].
zipIntegersFromEither :: [(Integer, Integer)]
zipIntegersFromEither =
  let int1           = 1 :: Integer
      int2           = 2 :: Integer
      myPlus         = (+) :: Num a => a -> a -> a
      plus1          = myPlus int1 :: Integer -> Integer
      int3           = plus1 int2 :: Integer
      int6           = 6 :: Integer
      myPow          = (^) :: (Num a, Integral b) => a -> b -> a
      powOf2         = myPow int2 :: Integer -> Integer
      int64          = powOf2 int6 :: Integer
      myLeft         = (Left) :: a -> Either a b
      myRight        = (Right) :: b -> Either a b
      leftInt3       = myLeft int3 :: Either Integer b
      rightInt64     = myRight int64 :: Either a Integer
      listEitherInts = [leftInt3, rightInt64] :: [Either Integer Integer]
      myZip          = zip :: [a] -> [b] -> [(a, b)]
      myLefts        = lefts :: forall a b . [Either a b] -> [a]
      myRights       = rights :: [Either a b] -> [b]
      zipLeftsRights =
          (\(x :: [Either a b]) ->
            (myZip (myLefts x :: [a]) :: [b] -> [(a, b)]) (myRights x :: [b])
          ) :: forall a b . [Either a b] -> [(a, b)]
  in  zipLeftsRights listEitherInts :: [(Integer, Integer)]

-- | Return 'True' for any 'Integer'.
trueForAnyInteger :: Integer -> Bool
trueForAnyInteger =
  let
    impl :: Bool -> Bool -> Bool
    impl = \(x :: Bool) (y :: Bool) -> ((not x :: Bool) || y :: Bool)
  in
    (let
       isMod2 :: Integer -> Bool
       isMod2 = \(x :: Integer) ->
         (((mod :: Integral a => a -> a -> a) x :: Integer -> Integer)
             (2 :: Integer) :: Integer
           )
           == (0 :: Integer) :: Bool
     in
       (let
          isMod4 :: Integer -> Bool
          isMod4 = \(x :: Integer) ->
            (((mod :: Integral a => a -> a -> a) x :: Integer -> Integer)
                (4 :: Integer) :: Integer
              )
              == (0 :: Integer) :: Bool
        in
          (\(x :: Integer) ->
            (impl (isMod4 x :: Bool) :: Bool -> Bool) (isMod2 x :: Bool) :: Bool
          ) :: Integer -> Bool
       ) :: Integer -> Bool
    ) :: Integer -> Bool
