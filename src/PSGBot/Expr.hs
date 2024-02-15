{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}

module PSGBot.Expr where

import Control.Monad
import Data.Fix
import Data.Foldable
import Data.List (find)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import System.Random
import System.Random.Stateful

newtype Ident =
  Ident String
  deriving (Show, Eq, Ord)

data ExprF e
  = LitE String
  | RuleE Ident [e]
  | CatE [e]
  | UnionE (NonEmpty (Float, e))
  deriving (Functor, Foldable, Traversable, Show)

type Expr = Fix ExprF

data Rule = Rule
  { ruleParams :: [Ident]
  , ruleOutput :: Expr
  }

newtype Env =
  Env (Map Ident Rule)

getRule :: Env -> Ident -> Maybe Rule
getRule (Env rs) i = Map.lookup i rs

-- | Given a list of weights, pick the element
pickWeighted :: NonEmpty (Float, a) -> Float -> a
pickWeighted ((w, x) :| []) _ = x
pickWeighted ((w, x) :| (n:rest)) i =
  if i' <= 0
    then x
    else pickWeighted (n :| rest) i'
  where
    i' = i - w

reduceExpr :: (StatefulGen g m) => g -> Env -> ExprF (m [String]) -> m [String]
reduceExpr g env@(Env envMap) =
  \case
    LitE l -> pure [l]
    CatE children -> do
      results <- sequence children
      pure $ join results
    UnionE children -> do
      let wSum = sum $ map (\(w, _) -> w) (NonEmpty.toList children)
      i <- uniformRM (0, wSum) g
      pickWeighted children i
    RuleE rName argVals -> do
      argVals' <- sequence argVals
      let rule = fromJust $ getRule env rName
          argValExprs = fmap (Fix . CatE . fmap (Fix . LitE)) argVals'
          env' =
            Map.fromList $ zip (ruleParams rule) (map (Rule []) argValExprs)
        -- must fully recurse because of eager evaluation here
       in evalExpr g (Env (Map.union env' envMap)) (ruleOutput rule)

evalExpr :: (StatefulGen g m) => g -> Env -> Fix ExprF -> m [String]
evalExpr g env = foldFix (reduceExpr g env)

maybeE :: Expr -> Expr
maybeE e = Fix $ UnionE ([(1, nullE), (1, e)])

nullE :: Expr
nullE = Fix $ CatE []

uniformUnionE :: NonEmpty Expr -> Expr
uniformUnionE xs = Fix $ UnionE $ fmap (\x -> (1, x)) xs
