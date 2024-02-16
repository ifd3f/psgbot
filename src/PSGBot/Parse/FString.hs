{-# LANGUAGE DeriveFunctor #-}

module PSGBot.Parse.FString where

import Data.Fix
import Text.ParserCombinators.Parsec hiding (spaces)

newtype FString e =
  FString [FStringBlock e]
  deriving (Functor, Show, Eq)

data FStringBlock e
  = FSChar Char
  | FSSubs e
  deriving (Functor, Show, Eq)

parseFStringInner :: Parser e -> Parser (FString e)
parseFStringInner parseInner = do
  x <-
    many
      ((string "\"" >> pure (FSChar '\\')) <|>
       (string "\\$" >> pure (FSChar '$')) <|>
       (FSSubs <$> parseSubs parseInner) <|>
       (FSChar <$> anyChar))
  return $ FString x

parseSubs :: Parser e -> Parser e
parseSubs parseInner = do
  between (string "${") (char '}') parseInner
