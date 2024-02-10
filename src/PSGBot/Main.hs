module PSGBot.Main where

import PSGBot.Expr
import Data.Fix
import Data.Maybe
import Control.Monad
import System.Random
import System.Random.Stateful

testgrammar :: Grammar
testgrammar =
  Grammar
    [ Fix
        <$> Rule
          (Ident "S1")
          []
          ( CatE
              [ RuleE (Ident "NP") [],
                RuleE (Ident "VP") []
              ]
          ),
      Rule
        (Ident "NP")
        []
        ( CatE
            [ Fix $ RuleE (Ident "D") [],
              maybeE (Fix $ RuleE ( Ident "Adj") []),
              Fix $ RuleE (Ident "N") []
            ]
        ),
      Fix
        <$> Rule
          (Ident "VP")
          []
          ( CatE
              [ RuleE (Ident "V") [],
                RuleE (Ident "NP") []
              ]
          ),
      Fix
        <$> Rule
          (Ident "V")
          []
          ( UnionE
              [ LitE "licked",
                LitE "bit",
                LitE "kissed",
                LitE "hugged",
                LitE "patted"
              ]
          ),
      Fix
        <$> Rule
          (Ident "N")
          []
          ( UnionE
              [ LitE "cat",
                LitE "dog",
                LitE "fox"
              ]
          ),
      Fix
        <$> Rule
          (Ident "D")
          []
          ( UnionE
              [ LitE "the",
                LitE "a",
                LitE "that"
              ]
          ),
      Fix
        <$> Rule
          (Ident "Adj")
          []
          ( UnionE
              [ LitE "silly",
                LitE "stupid",
                LitE "furry",
                LitE "cute"
              ]
          )
    ]

main :: IO ()
main = do
  let grammar = testgrammar
  let rule = fromJust $ getRule grammar (Ident "S1")
  forM_ [1..10] $ \_ -> do
    seed <- randomIO :: IO Int
    let pureGen = mkStdGen seed
    rand <- newIOGenM pureGen
    result <- evalExpr rand grammar (Fix $ ruleOutput rule)
    print result
