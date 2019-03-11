module Semver where

import Data.Either
import Data.List
import qualified Data.Text as T
import Text.Read

data VersionError = Missing | LeadingZero | NotAnInt

data InvalidSemanticVersion = InvalidSemanticVersion {
    ma :: Either VersionError Int,
    mi :: Either VersionError Int,
    pa :: Either VersionError Int
}

data SemanticVersion = SemanticVersion {
    major :: Int,
    minor :: Int,
    patch :: Int
}

parseVersionString :: String -> Either InvalidSemanticVersion SemanticVersion
parseVersionString versionString = do
    let parts = splitVersionString versionString
    let parsedParts = fmap parsePart (take 3 parts)
    partsToSemanticVersion parsedParts

partsToSemanticVersion :: [Either VersionError Int] -> Either InvalidSemanticVersion SemanticVersion
partsToSemanticVersion []            = Left (InvalidSemanticVersion (Left Missing) (Left Missing) (Left Missing))
partsToSemanticVersion (ma:[])       = Left (InvalidSemanticVersion ma (Left Missing) (Left Missing))
partsToSemanticVersion (ma:mi:[])    = Left (InvalidSemanticVersion ma mi (Left Missing))
partsToSemanticVersion ((Right ma):(Right mi):(Right pa):[]) = Right (SemanticVersion ma mi pa)
partsToSemanticVersion (ma:mi:pa:[]) = Left (InvalidSemanticVersion ma mi pa)

parsePart :: String -> Either VersionError Int
parsePart (x:xs) | x == '0' = Left LeadingZero
parsePart xs = case (readMaybe xs :: Maybe Int) of
                    Just num -> Right num
                    Nothing  -> Left NotAnInt

splitVersionString :: String -> [String]
splitVersionString versionString = fmap T.unpack (T.splitOn (T.pack ".") (T.pack versionString))
