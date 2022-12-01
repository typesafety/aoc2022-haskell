module Solutions.Day1 where

import Prelube

import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as Lex
 

-- | Solve part 1.
solve1 :: Text -> Text
solve1 = toTxt . show . s1 . partialParseText groupsP

-- | Helper for solving part 1.
s1 :: NonEmpty (NonEmpty Int) -> Int
s1 = maximum . map sum
    
solve2 :: Text -> Text
solve2 = toTxt . show . s2 . partialParseText groupsP

s2 :: NonEmpty (NonEmpty Int) -> Int
s2 = sum . take 3 . sortBy (flip compare) . map sum

-- | Parser for the puzzle input
type Parser = P.Parsec Void Text

groupsP :: Parser (NonEmpty (NonEmpty Int))
groupsP = partialNonEmpty <$> P.someTill groupP P.eof

groupP :: Parser (NonEmpty Int)
groupP = partialNonEmpty <$> P.someTill rowP P.eol

rowP :: Parser Int
rowP = Lex.decimal <* P.eol
