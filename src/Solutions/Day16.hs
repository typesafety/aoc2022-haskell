{-# language TemplateHaskell #-}

module Solutions.Day16 where

import Prelube

import Control.Lens.Combinators qualified as L
import Control.Lens.Operators ((^.))
import Control.Lens.TH qualified as LensTH

import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as Lex


-- * Parsing

type Parser = P.Parsec Void Text

data Cave = Cave
    { _cNeighs :: [String]
    , _cFlow :: Int
    , _cName :: String
    }
    deriving (Eq, Show)
$(LensTH.makeLenses ''Cave)

rowP :: Parser Cave
rowP = do
    _ <- P.string "Valve "
    name <- nameP
    _ <- P.string " has flow rate="
    flow <- Lex.decimal
    _ <- P.string "; "
    _ <- P.string "tunnels" <|> P.string "tunnel"
    _ <- P.space1
    _ <- P.string "leads" <|> P.string "lead"
    _ <- P.string " to "
    _ <- P.string "valves" <|> P.string "valve"
    _ <- P.space1
    neighs <- P.sepBy1 nameP (P.string ", ")
    _ <- P.eol
    pure $ Cave neighs flow name

nameP :: Parser String
nameP = P.count 2 P.upperChar

inputP :: Parser (NonEmpty Cave)
inputP = partialNonEmpty <$> P.someTill rowP P.eof

-- * Solvers

-- | Solve part 1.
solve1 :: Text -> SolverResult
solve1 = todo

-- | Solve part 2.
solve2 :: Text -> SolverResult
solve2 = todo
