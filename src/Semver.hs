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
    patch :: Int,
    preRelease :: Maybe String
} deriving (Show)

nonZeroDigit :: Parser Char
nonZeroDigit = satisfy (\c -> (isDigit c) && (c /= '0'))

digit :: Parser Char
digit = satisfy isDigit

zero :: Parser Int
zero = do
    z <- satisfy (\c -> (isDigit c) && (c == '0'))
    return $ read (z:[])

number :: Parser Int
number = do
  s  <- nonZeroDigit
  cs <- many digit
  return $ read (s:cs)

dot :: Parser Char
dot = satisfy (\c -> c == '.')

dash :: Parser Char
dash = satisfy (\c -> c == '-')

alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum

identifier :: Parser String
identifier = do
  c  <- satisfy isAlpha
  cs <- many alphaNum
  return (c:cs)

preReleaseIdentifier :: Parser String
preReleaseIdentifier = do
  _ <- dash
  i <- identifier
  return i

parseVersionString :: Parser SemanticVersion
parseVersionString = do
  major <- zero <|> number
  _     <- dot
  minor <- zero <|> number
  _     <- dot
  patch <- zero <|> number
  pri   <- atMostOne preReleaseIdentifier
  return $ SemanticVersion major minor patch pri
