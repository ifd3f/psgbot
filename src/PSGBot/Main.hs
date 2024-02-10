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
          Fix
            <$> Rule
              []
              ( CatE
                  [ RuleE (Ident "NP") [],
                    RuleE (Ident "VP") []
                  ]
              )
        ),
        ( Ident "NP",
          Rule
            []
            ( CatE
                [ Fix $ RuleE (Ident "D") [],
                  maybeE (Fix $ RuleE (Ident "Adj") []),
                  Fix $ RuleE (Ident "N") []
                ]
            )
        ),
        ( Ident "VP",
          Fix
            <$> Rule
              []
              ( CatE
                  [ RuleE (Ident "V") [],
                    RuleE (Ident "NP") []
                  ]
              )
        ),
        ( Ident "V",
          Fix
            <$> Rule
              []
              ( UnionE
                  [ LitE "licked",
                    LitE "bit",
                    LitE "kissed",
                    LitE "hugged",
                    LitE "patted"
                  ]
              )
        ),
        ( Ident "N",
          Fix
            <$> Rule
              []
              ( UnionE
                  [ LitE "cat",
                    LitE "dog",
                    LitE "fox"
                  ]
              )
        ),
        ( Ident "D",
          Fix
            <$> Rule
              []
              ( UnionE
                  [ LitE "the",
                    LitE "a",
                    LitE "that"
                  ]
              )
        ),
        ( Ident "Adj",
          Fix
            <$> Rule
              []
              ( UnionE
                  [ LitE "silly",
                    LitE "stupid",
                    LitE "furry",
                    LitE "cute"
                  ]
              )
        )
      ]

main :: IO ()
main = do
  let grammar = testgrammar
  let rule = fromJust $ getRule grammar (Ident "S1")
  forM_ [1 .. 10] $ \_ -> do
    seed <- randomIO :: IO Int
    let pureGen = mkStdGen seed
    rand <- newIOGenM pureGen
    result <- evalExpr rand grammar (Fix $ ruleOutput rule)
    print result
