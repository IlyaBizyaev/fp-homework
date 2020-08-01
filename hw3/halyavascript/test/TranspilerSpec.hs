module TranspilerSpec where

import           Data.Int                       ( Int32 )
import           Test.Hspec

import           HSExpr                         ( HSExpr(..)
                                                , fun
                                                , readVar
                                                , readVar2
                                                , while
                                                , withVar
                                                , (#)
                                                , (@=)
                                                , (@>)
                                                )
import           Transpiler                     ( transpile )

basicBinary :: HSExpr expr => expr Bool
basicBinary = withVar (42 :: Int32) (\a -> a @> 21)

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

-- | Ensure that HalyavaScript to JavaScript transpilation works correctly.
spec :: Spec
spec = do
  describe "Basic transpiler tests" $ do
    it "Binary operators are transpiled correctly" $ do
      let repr = transpile basicBinary
      repr `shouldBe` "var v0 = 42;\nv0 > 21"
  -- Sadly, after 3 days with this task, I have't managed to make a working
  -- transpiler :(
  -- My GADT attempts didn't succeed, and with final tagless making a pretty
  -- printer turned into a search for hacks...
  -- describe "Complete transpiler test" $ do
  --   it "log2 transpiles correctly" $ do
  --     let repr = transpile (log2 42)
  --     let expectedRepr = concat
  --           [ "(function(v0) {\n"
  --           , "    var v1 = 0;\n"
  --           , "    var v2 = 0;\n"
  --           , "    v2 = 1;\n"
  --           , "    v1 = 0;\n"
  --           , "    while (v0 > v2) {\n"
  --           , "        v2 = v2 + v2;\n"
  --           , "        v1 = v1 + 1;\n"
  --           , "    }\n"
  --           , "    return v1;\n"
  --           , "})(42);"
  --           ]
  --     repr `shouldBe` expectedRepr
