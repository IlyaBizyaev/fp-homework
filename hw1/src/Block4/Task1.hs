module Block4.Task1
  ( stringSum
  )
where

-- | If the string is a space-separated list of integers, sums them.
-- Otherwise considers the string malformed and returns Nothing.
stringSum :: String -> Maybe Int
stringSum s = sumMapped mapped where
  mapped    = mapM maybeWrapRead maybeNums
  maybeNums = map reads sWords
  sWords    = words s

  sumMapped :: Maybe [Int] -> Maybe Int
  sumMapped Nothing  = Nothing
  sumMapped (Just x) = Just (sum x)

  maybeWrapRead :: [(Int, String)] -> Maybe Int
  maybeWrapRead [(x, "")] = Just x
  maybeWrapRead _         = Nothing
