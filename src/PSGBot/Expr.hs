{-# LANGUAGE DeriveFunctor #-}

module PSGBot.Expr where

import Control.Monad
import Data.Fix
import Data.Foldable
import Data.List (find)
import Data.Maybe
import System.Random
import System.Random.Stateful

newtype Ident = Ident String deriving (Show, Eq, Ord)

data ExprF e
  = LitE String
  | RuleE Ident [e]
  | CatE [e]
  | UnionE [e]
  deriving (Functor, Show)

type Expr = Fix ExprF

data RuleF e = Rule
  { ruleId :: Ident,
    ruleArgs :: [Ident],
    ruleOutput :: ExprF e
  }
  deriving (Functor, Show)

type Rule = RuleF Expr

newtype GrammarF e = Grammar [RuleF e] deriving (Functor, Show)

type Grammar = GrammarF Expr

getRule :: GrammarF e -> Ident -> Maybe (RuleF e)
getRule (Grammar rs) i = find (\r -> ruleId r == i) rs

evalExpr :: (StatefulGen g m) => g -> Grammar -> Expr -> m [String]
evalExpr g grammar (Fix e) = case e of
  LitE l -> pure [l]
  CatE exprs -> do
    x <- mapM (evalExpr g grammar) exprs
    pure $ join x
  UnionE exprs -> do
    let len = length exprs
    i <- uniformRM (0, len - 1) g
    let expr = exprs !! i
    evalExpr g grammar expr
  RuleE rName args ->
    let rule = fromJust $ getRule grammar rName
     in evalExpr g grammar (Fix $ ruleOutput rule)

maybeE :: Expr -> Expr
maybeE e = Fix $ UnionE [Fix $ CatE [], e]