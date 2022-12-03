module Solutions.Day02 where

import Prelube

import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P

-- | Solve part 1.
solve1 :: Text -> SolverResult
solve1 = SR . sum . map scoreStrat . partialParseText inputP

-- | Calculate the score of running one Strat.
scoreStrat :: Strat -> Int
scoreStrat strat = baseVal (strat.response) + outcomeVal strat
  where
    baseVal :: RPS -> Int
    baseVal = \case
        Rock -> 1
        Paper -> 2
        Scissors -> 3

    outcomeVal :: Strat -> Int
    outcomeVal s = case compare s.opponent s.response of
        LT -> 0
        EQ -> 3
        GT -> 6

-- | Solve part 2.
solve2 :: Text -> SolverResult
solve2 = SR . sum . map (scoreStrat . adjustStrat) . partialParseText inputP2
  where
    adjustStrat :: Strat2 -> Strat
    adjustStrat s2 = Strat s2.opponent $ case s2.desired of
        Win -> win s2.opponent
        Draw -> draw s2.opponent
        Lose -> lose s2.opponent
      where
        win :: RPS -> RPS
        win = \case
            Rock -> Paper
            Paper -> Scissors
            Scissors -> Rock

        lose :: RPS -> RPS
        lose = win . win

        draw :: RPS -> RPS
        draw = id

-- * Data types and parsing of.

-- | Strategy for how to play against an opponents move.
data Strat = Strat
    { opponent :: RPS
    , response :: RPS
    }
    deriving (Eq, Show)

data Strat2 = Strat2
    { opponent :: RPS
    , desired :: Res
    }
    deriving (Eq, Show)

data Res = Win | Draw | Lose
    deriving (Eq, Show)

-- | Enum for the moves.
data RPS = Rock | Paper | Scissors
    deriving (Eq, Show)

instance Ord RPS where
    compare r1 r2 = case (r1, r2) of
        (Rock, Paper) -> GT
        (Rock, Scissors) -> LT
        (Rock, Rock) -> EQ
        (Paper, Scissors) -> GT
        (Paper, Rock) -> LT
        (Paper, Paper) -> EQ
        (Scissors, Rock) -> GT
        (Scissors, Paper) -> LT
        (Scissors, Scissors) -> EQ

-- | Parser for text.
type Parser = P.Parsec Void Text

inputP2 :: Parser (NonEmpty Strat2)
inputP2 = partialNonEmpty <$> P.someTill rowP2 P.eof

rowP2 :: Parser Strat2
rowP2 = do
    opponent <- tlOpponent =<< P.char 'A' <|> P.char 'B' <|> P.char 'C'
    _ <- P.spaceChar
    desired <- tlDesired =<< P.char 'X' <|> P.char 'Y' <|> P.char 'Z'
    _ <- P.eol
    pure (Strat2 opponent desired)
  where
    tlDesired :: Char -> Parser Res
    tlDesired = \case
        'X' -> pure Lose
        'Y' -> pure Draw
        'Z' -> pure Win
        _ -> fail "The desired result should be X, Y, or Z"

inputP :: Parser (NonEmpty Strat)
inputP = partialNonEmpty <$> P.someTill rowP P.eof

rowP :: Parser Strat
rowP = do
    opponent <- tlOpponent =<< P.char 'A' <|> P.char 'B' <|> P.char 'C'
    _ <- P.spaceChar
    response <- tlResponse =<< P.char 'X' <|> P.char 'Y' <|> P.char 'Z'
    _ <- P.eol
    pure (Strat opponent response)
  where
    tlResponse :: Char -> Parser RPS
    tlResponse = \case
        'X' -> pure Rock
        'Y' -> pure Paper
        'Z' -> pure Scissors
        _ -> fail "Your response should be X, Y, or Z."

tlOpponent :: Char -> Parser RPS
tlOpponent = \case
    'A' -> pure Rock
    'B' -> pure Paper
    'C' -> pure Scissors
    _ -> fail "Opponent's move should be A, B, or C."
