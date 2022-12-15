module Solutions.Day13 where

import Prelube

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


-- | Solve part 1.
solve1 :: Text -> SolverResult
solve1 = todo

-- | Solve part 2.
solve2 :: Text -> SolverResult
solve2 = todo
