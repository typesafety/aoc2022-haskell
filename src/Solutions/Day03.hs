module Solutions.Day03 where

import Prelube

import Data.Char (isUpper)
import Data.List qualified as List

import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P

-- | Solve part 1.
solve1 :: Text -> SolverResult
solve1 = SR . sum . map (priority . snd . attachDupe) . partialParseText inputP

priority :: Item -> Int
priority (Item c) = fromEnum c - offset
  where
    offset :: Int
    offset 
        | isUpper c = 38
        | otherwise = 96

attachDupe :: Rucksack -> (Rucksack, Item)
attachDupe r = (r, ) . partialFromJust . uncurry findDupe . both toList $ (r.first, r.second)

findDupe :: [Item] -> [Item] -> Maybe Item
findDupe [] _ = Nothing
findDupe (x : xs) ys
    | x `elem` ys = pure x
    | otherwise   = findDupe xs ys

-- | Solve part 2.
solve2 :: Text -> SolverResult
solve2 = todo

-- * Data types and parsers for them.

data Rucksack = Rucksack
    { first :: NonEmpty Item
    , second :: NonEmpty Item
    }
    deriving (Eq, Show)

newtype Item = Item Char
    deriving (Eq, Show, Enum) via Char

-- | Parser for text.
type Parser = P.Parsec Void Text

inputP :: Parser (NonEmpty Rucksack)
inputP = partialNonEmpty <$> P.someTill rowP P.eof

rowP :: Parser Rucksack
rowP = do
    line <- P.someTill P.letterChar P.eol
    let half = List.length line `div` 2
    let (h1, h2) = (List.take half $ line, List.drop half $ line)
    pure $ Rucksack (map Item . partialNonEmpty $ h1) (map Item . partialNonEmpty $ h2)
