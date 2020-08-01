module Main where

import Data.Int (Int32)

import HSExpr (HSExpr (..), fun, fun2, ifElse, readVar, readVar2, while, withVar, ( # ), (@=),
               (@==), (@>))
import Interpreter (interpret)
import Transpiler (transpile)

-- | HalyavaScript sample with integers, boolean, function and a loop.
-- For a given @a@ calculates @ceiling (log2 (a))@.
log2 :: HSExpr expr => Int32 -> expr Int32
log2 =
  fun 0 $ \a logCnt ->
  withVar 0 $ \accum ->
    accum @= 1 #
    logCnt @= 0 #
    while (readVar accum $ \eAccum -> a @> eAccum)
      ( readVar2 accum logCnt $ \eAccum eLogCnt ->
          accum @= eAccum + eAccum #
          logCnt @= eLogCnt + 1
      )

-- | HalyavaScript sample with strings and branching.
-- Returns a personalized greeting, repeated multiple times, with a surprise.
helloName :: HSExpr expr => Int32 -> String -> expr String
helloName =
  fun2 "" $ \times name res ->
    while (times @> 0)
      (
        readVar2 times name $ \eTimes eName ->
        readVar res $ \eRes ->
          (ifElse (times @== 2)
            (res @= eRes ++ "Surprise!\n")
            (res @= eRes ++ "Hello, " ++ eName ++ "\n")) #
          times @= eTimes - 1
      )

-- | Helper function to print an output separator.
sep :: IO ()
sep = putStrLn "---------------"

-- | Transpile and interpret 2 HalyavaScript eDSL samples, print results.
main :: IO ()
main = do
  let repr1 = transpile (log2 42)
  let repr2 = transpile (helloName 3 "Ilya")
  let res1  = interpret (log2 42)
  let res2  = interpret (helloName 3 "Ilya")
  sep >> putStrLn repr1
  sep >> putStrLn "Result 1:" >> print res1
  sep >> putStrLn repr2
  sep >> putStrLn "Result 2:" >> putStrLn res2
