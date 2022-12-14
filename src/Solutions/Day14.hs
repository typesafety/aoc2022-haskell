module Solutions.Day14 where

import Prelube

import Data.Foldable (foldl')
import Data.HashSet qualified as HS

import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as Lex

-- * Parsing

type Parser = P.Parsec Void Text

pointP :: Parser (Int, Int)
pointP = do
    x <- Lex.decimal
    _ <- P.char ','
    y <- Lex.decimal
    pure (x, y)

wallP :: Parser (NonEmpty (Int, Int))
wallP = do
    points <- P.sepBy1 pointP (P.char ' ' *> P.string "->" <* P.char ' ')
    _ <- P.eol
    pure $ partialNonEmpty points

inputP :: Parser [NonEmpty (Int, Int)]
inputP = P.someTill wallP P.eof

-- * Solvers

type Walls = HashSet (Int, Int)
type Sands = HashSet (Int, Int)

makeWalls :: NonEmpty (Int, Int) -> Walls
makeWalls (h :| hs) = go h hs
  where
    go :: (Int, Int) -> [(Int, Int)] -> Walls
    go _ [] = HS.empty
    go (px, py) (apa@(x, y) : xs) =
        go apa xs <> HS.fromList [(a, b) | a <- range px x, b <- range py y]

    range :: Int -> Int -> [Int]
    range from to
        | from < to = [from .. to]
        | otherwise = [to .. from]

allWalls :: [NonEmpty (Int, Int)] -> Walls
allWalls = foldl' (\walls points -> makeWalls points <> walls) HS.empty

dropUntilDone :: Walls -> Sands
dropUntilDone walls = go HS.empty
  where
    go :: Sands -> Sands
    go sands =
        let newSands = dropSand walls sands
        in if HS.size newSands == HS.size sands
            then sands
            else go newSands

dropSand :: Walls -> Sands -> Sands
dropSand walls = move (500, 0)
  where
    move :: (Int, Int) -> Sands -> Sands
    move curr sands
        | snd curr > lowest = sands
        | otherwise = case maybeMove curr of 
            Just new -> move new sands
            Nothing  -> HS.insert curr sands
      where
        lowest :: Int
        lowest = HS.foldl' (\highest new -> max highest new) 0 . HS.map snd $ walls

        maybeMove :: (Int, Int) -> Maybe (Int, Int)
        maybeMove (x, y) = tryPlace (x, y + 1) <|> tryPlace (x - 1, y + 1) <|> tryPlace (x + 1, y + 1)

        tryPlace :: (Int, Int) -> Maybe (Int, Int)
        tryPlace point
            | point `HS.member` walls || point `HS.member` sands = Nothing
            | otherwise = Just point

-- | Solve part 1.
solve1 :: Text -> SolverResult
solve1 = SR . HS.size . dropUntilDone . allWalls . partialParseText inputP

-- | Solve part 2.
solve2 :: Text -> SolverResult
solve2 = todo
