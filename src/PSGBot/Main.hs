{-# LANGUAGE OverloadedLists #-}

module PSGBot.Main where

import Control.Monad
import Data.Fix
import qualified Data.Map as Map
import Data.Maybe
import PSGBot.Expr
import System.Random
import System.Random.Stateful

testgrammar :: Env
testgrammar =
  Env $
    Map.fromList
      [ ( Ident "S1",
          Rule
            []
            ( Fix $
                Fix
                  <$> CatE
                    [ RuleE (Ident "NP") [],
                      RuleE (Ident "VP") []
                    ]
            )
        ),
        ( Ident "S2",
          Rule
            []
            ( Fix $
                Fix
                  <$> RuleE
                    (Ident "SuedForBeingLamer")
                    [ RuleE (Ident "NP") [],
                      RuleE (Ident "NP") []
                    ]
            )
        ),
        ( Ident "SuedForBeingLamer",
          Rule
            [Ident "s1", Ident "s2"]
            ( Fix $
                Fix
                  <$> CatE
                    [ RuleE (Ident "s1") [],
                      LitE "sued",
                      RuleE (Ident "s2") [],
                      LitE "for being a lamer version of",
                      RuleE (Ident "s1") []
                    ]
            )
        ),
        ( Ident "NP",
          Rule
            []
            ( Fix $
                CatE
                  [ Fix $ RuleE (Ident "D") [],
                    maybeE (Fix $ RuleE (Ident "Adj") []),
                    Fix $ RuleE (Ident "N") []
                  ]
            )
        ),
        ( Ident "VP",
          Rule
            []
            ( Fix $
                Fix
                  <$> CatE
                    [ RuleE (Ident "V") [],
                      RuleE (Ident "NP") []
                    ]
            )
        ),
        ( Ident "V",
          Rule
            []
            (
                 uniformUnionE
                     [ Fix $ LitE "licked",
                      Fix $ LitE "bit",
                      Fix $ LitE "kissed",
                      Fix $ LitE "hugged",
                      Fix $ LitE "patted"
                    ]
            )
        ),
        ( Ident "N",
          Rule
            []
            (
                 uniformUnionE
                    [ Fix $ LitE "cat",
                      Fix $ LitE "dog",
                      Fix $ LitE "fox"
                    ]
            )
        ),
        ( Ident "D",
          Rule
            []
            (
                 uniformUnionE
                     [ Fix $ LitE "the",
                      Fix $ LitE "a",
                      Fix $ LitE "that"
                    ]
            )
        ),
        ( Ident "Adj",
          Rule
            []
            (
                 uniformUnionE
                    [ Fix $ LitE "silly",
                      Fix $ LitE "stupid",
                      Fix $ LitE "furry",
                      Fix $ LitE "cute"
                    ]
            )
        )
      ]

main :: IO ()
main = do
  let grammar = testgrammar
  let rule = fromJust $ getRule grammar (Ident "S2")
  forM_ ([1 .. 10] :: [Int]) $ \_ -> do
    seed <- randomIO :: IO Int
    let pureGen = mkStdGen seed
    rand <- newIOGenM pureGen
    result <- evalExpr rand grammar (ruleOutput rule)
    print result
