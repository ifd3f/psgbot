{-# LANGUAGE DeriveFunctor #-}

module PSGBot.Expr where

import Control.Monad
import Data.Fix
import Data.Foldable
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
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
  { ruleArgs :: [Ident],
    ruleOutput :: ExprF e
  }
  deriving (Functor, Show)

type Rule = RuleF Expr

newtype EnvF e = Env (Map Ident (RuleF e))
  deriving (Functor, Show)

type Env = EnvF Expr

getRule :: EnvF e -> Ident -> Maybe (RuleF e)
getRule (Env rs) i = Map.lookup i rs

evalExpr :: (StatefulGen g m) => g -> Env -> Expr -> m [String]
evalExpr g env (Fix e) = case e of
  LitE l -> pure [l]
  CatE exprs -> do
    x <- mapM (evalExpr g env) exprs
    pure $ join x
  UnionE exprs -> do
    let len = length exprs
    i <- uniformRM (0, len - 1) g
    let expr = exprs !! i
    evalExpr g env expr
  RuleE rName args ->
    let rule = fromJust $ getRule env rName
     in evalExpr g env (Fix $ ruleOutput rule)

maybeE :: Expr -> Expr
maybeE e = Fix $ UnionE [Fix $ CatE [], e]