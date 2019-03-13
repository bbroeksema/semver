module Semver where

import NanoParsec

import Control.Applicative
import Data.Char
import Data.Either
import Data.List
import Data.List.Split
import Text.Read

data VersionError = Missing | LeadingZero | NotAnInt


data SemanticVersion = SemanticVersion {
    major :: Int,
    minor :: Int,
    patch :: Int
} deriving (Show)

nonZeroDigit :: Parser Char
nonZeroDigit = satisfy (\c -> (isDigit c) && (c /= '0'))

digit :: Parser Char
digit = satisfy isDigit

number :: Parser Int
number = do
  s  <- nonZeroDigit
  cs <- many digit
  return $ read (s:cs)

isDot :: Char -> Bool
isDot c = c == '.'

parseVersionString :: Parser SemanticVersion
parseVersionString = do
  major <- number
  _     <- satisfy isDot
  minor <- number
  _     <- satisfy isDot
  patch <- number
  return $ SemanticVersion major minor patch
