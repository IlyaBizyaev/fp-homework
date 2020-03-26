module Block4.Task1
  ( stringSum
  )
where

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
