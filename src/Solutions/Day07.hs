module Solutions.Day07 where

import Prelube

import Data.Sequence qualified as Seq

import Data.IntMap.Strict qualified as IM
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as Lex


-- | Solve part 1.
solve1 :: Text -> SolverResult
solve1 = todo

-- | Solve part 2.
solve2 :: Text -> SolverResult
solve2 = todo



-- * Data types and parsers


data Cmd = Cd Location | Ls (Seq DirItem)
    deriving (Eq, Show)

data Location = Root | Back | Relative Text
    deriving (Eq, Show)

data DirItem = DirEntry Text | FileEntry Text Int
    deriving (Eq, Show)

-- | Fully qualified path.  The "/" location is inidcated by an empty list.
newtype Path = Path { unPath :: Seq Text }
    deriving (Eq, Show)

data Dir = Dir
    { path :: Seq Text
    , items :: Seq DirItem
    }
    deriving (Eq, Show)

type Parser = P.Parsec Void Text

inputP :: Parser [Cmd]
inputP = P.many cmdP

cmdP :: Parser Cmd
cmdP = cdP <|> lsP

cdP :: Parser Cmd
cdP = do
    _ <- P.string "$ cd"
    _ <- P.char ' '
    loc <- locP <* P.eol
    pure (Cd loc)

locP :: Parser Location
locP = relP <|> backP <|> rootP
  where
    backP :: Parser Location
    backP = P.string ".." *> pure Back

    rootP :: Parser Location
    rootP = P.char '/' *> pure Root

    relP :: Parser Location
    relP = Relative . toTxt <$> P.some (P.letterChar)

lsP :: Parser Cmd
lsP = do
    _ <- P.string "$ ls"
    _ <- P.eol
    dirItems <- P.many (dirItemP <* P.eol)
    pure $ Ls (Seq.fromList dirItems)

dirItemP :: Parser DirItem
dirItemP = dirEntryP <|> fileEntryP

dirEntryP :: Parser DirItem
dirEntryP = do
    _ <- P.string "dir"
    _ <- P.char ' '
    DirEntry . toTxt <$> P.some P.letterChar

fileEntryP :: Parser DirItem
fileEntryP = do
    size <- Lex.decimal
    _ <- P.space
    name <- P.some (P.letterChar <|> P.char '.')
    pure (FileEntry (toTxt name) size)
    
