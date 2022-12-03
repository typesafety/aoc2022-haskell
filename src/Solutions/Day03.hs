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
solve2 = SR
    . sum
    . fmap priority
    . partialFromJust
    . (traverse (\(x, y, z) -> findBadge x y z) <=< chunks3)
    . toList
    . partialParseText inputP

chunks3 :: [a] -> Maybe [(a, a, a)]
chunks3 = \case
    [] -> pure []
    x : y : z : xs -> do
        rest <- chunks3 xs
        pure $ (x, y, z) : rest
    _ -> error "FUCK"

findDupes :: [Item] -> [Item] -> [Item]
findDupes = go []
  where
    go :: [Item] -> [Item] -> [Item] -> [Item]
    go acc [] _ = acc
    go acc (x : xs) ys
        | x `elem` ys = go (x : acc) xs ys
        | otherwise   = go acc xs ys

findBadge :: Rucksack -> Rucksack -> Rucksack -> Maybe Item
findBadge r1 r2 r3 = do
    let intersection12 = uncurry findDupes . both (toList . items) $ (r1, r2)
    findDupe intersection12 ((toList . items) r3) 

items :: Rucksack -> NonEmpty Item
items r = r.first <> r.second

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
