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

data Rule = Rule
  { ruleParams :: [Ident],
    ruleOutput :: Expr
  }

newtype Env = Env (Map Ident Rule)

getRule :: Env -> Ident -> Maybe Rule
getRule (Env rs) i = Map.lookup i rs

evalExpr :: (StatefulGen g m) => g -> Env -> Expr -> m [String]
evalExpr g env@(Env envMap) (Fix e) = case e of
  LitE l -> pure [l]
  CatE exprs -> do
    results <- mapM (evalExpr g env) exprs
    pure $ join results
  UnionE exprs -> do
    let len = length exprs
    i <- uniformRM (0, len - 1) g
    let expr = exprs !! i
    evalExpr g env expr
  RuleE rName argExprs -> do
    let rule = fromJust $ getRule env rName
    argResults <- mapM (evalExpr g env) argExprs
    let argsAsExprs = map (Fix . CatE . map (Fix . LitE)) argResults
    let env' = Map.fromList $ zip (ruleParams rule) (map (Rule []) argsAsExprs)
    evalExpr g (Env (Map.union env' envMap)) (ruleOutput rule)

maybeE :: Expr -> Expr
maybeE e = Fix $ UnionE [Fix $ CatE [], e]