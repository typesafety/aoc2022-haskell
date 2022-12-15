module Solutions.Day13 where

import Prelube

import Data.Foldable qualified as Foldable

import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as Lex


-- * Parsing

type Parser = P.Parsec Void Text

data Item = IList [Item] | IInt Int
    deriving (Eq, Show)

listP :: Parser Item
listP = IList <$> (P.char '[' *> P.sepBy itemP (P.char ',') <* P.char ']')

intP :: Parser Item
intP = IInt <$> Lex.decimal

itemP :: Parser Item
itemP = intP <|> listP

rowP :: Parser Item
rowP = itemP <* P.eol

pairP :: Parser (Item, Item)
pairP = (,) <$> rowP <*> rowP

inputP :: Parser (NonEmpty (Item, Item))
inputP = partialNonEmpty <$> P.someTill (pairP <* P.optional P.eol) P.eof

-- * Solve

data Trool = Yes | No | Idk
    deriving (Eq, Show)

cmpItems :: Item -> Item -> Trool
cmpItems l r = case (l, r) of
    (IInt nl, IInt nr) -> cmpInts nl nr
    (IList ll, IList lr) -> cmpLists ll lr
    (IList _, IInt _) -> cmpItems l (IList [r])
    (IInt _, IList _) -> cmpItems (IList [l]) r
  where
    cmpInts :: Int -> Int -> Trool
    cmpInts n1 n2
        | n1 < n2  = Yes
        | n1 > n2  = No
        | otherwise = Idk

    cmpLists :: [Item] -> [Item] -> Trool
    cmpLists [] [] = Idk
    cmpLists (_ : _) [] = No
    cmpLists [] (_ : _) = Yes
    cmpLists (x : xs) (y : ys) = case cmpItems x y of
        Idk -> cmpLists xs ys
        res -> res

-- | Solve part 1.
solve1 :: Text -> SolverResult
solve1 = SR
    . Foldable.foldl' (\acc t -> acc + fst t) 0
    . filter ((Yes ==) . snd)
    . zip ([1 ..] :: NonEmpty Int)
    . fmap (uncurry cmpItems)
    . partialParseText inputP

-- | Solve part 2.
solve2 :: Text -> SolverResult
solve2 = todo
