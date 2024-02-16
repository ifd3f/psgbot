module PSGBot.Parse.FStringSpec where

import Data.Foldable
import PSGBot.Parse.FString
import Test.Hspec
import Text.Parsec

spec :: Spec
spec = do
  describe "parseFStringInner" $ do
    traverse_
      (\(input, expected) ->
         it ("parses " ++ show input) $ do
           (parse (parseFStringInner parseInner) "testcase" input) `shouldBe`
             Right (FString expected))
      [ identityCase "asdf"
      , ( "as${foobar}df"
        ,  (FSChar <$> "as") ++ [FSSubs "foobar"] ++ (FSChar <$> "df"))
      , ("as\\${foobar}df",FSChar<$>"as${foobar}df" )
      , ("as\\${foobar}df",FSChar<$>"as${foobar}df" )
      ]

identityCase s = ( s
        ,  FSChar <$> s)

parseInner = do
  out <- many $ noneOf "}"
  pure out
