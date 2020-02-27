module Task6
  ( example1
  , example2
  ) where

import Data.Maybe (mapMaybe)
import Task1 (distributivity)

-- | First example of WHNF definition.
-- Lazy evaluation:
-- 1: (Left ("harold" ++ " hide " ++ "the " ++ "pain"),
--     Left ("harold" ++ " hide " ++ "the " ++ "pain"))
-- WHNF
example1 :: (Either String b, Either String c)
example1 = distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))

-- | Helper function used in the second example.
-- Return Just (exp pi) for the letter o and Nothing for other characters.
foo :: Char -> Maybe Double
foo char = case char == 'o' of
  True  -> Just $ exp pi
  False -> Nothing

-- | Second example of WHNF definition.
-- Lazy evaluation:
-- 1: null (mapMaybe foo "pole chudes ochen' chudesno")
-- 2: null (mapMaybe foo 'p':"ole chudes ochen' chudesno")
-- 3: null (mapMaybe foo "ole chudes ochen' chudesno")
-- 4: null (mapMaybe foo 'o':"le chudes ochen' chudesno")
-- 5: null (exp pi : (mapMaybe foo "le chudes ochen' chudesno"))
-- 6: False
-- WHNF
example2 :: Bool
example2 = null $ mapMaybe foo "pole chudes ochen' chudesno"
