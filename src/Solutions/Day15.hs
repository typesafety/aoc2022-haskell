{-# language TemplateHaskell #-}

module Solutions.Day15 where

import Prelube

import Control.Lens.Combinators qualified as L
import Control.Lens.Operators ((^.))
import Control.Lens.TH qualified as LensTH
import Data.Foldable (foldl')
import Data.List qualified as List

import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as Lex


-- * Data

newtype Beacon = Beacon { _unBeacon :: (Int, Int) }
    deriving (Eq, Show)
$(LensTH.makeLenses ''Beacon)

data Sensor = Sensor
    { _sLoc :: (Int, Int)
    , _sBeacon :: Beacon
    }
    deriving (Eq, Show)
$(LensTH.makeLenses ''Sensor)

-- * Parsing

type Parser = P.Parsec Void Text

coordP :: Parser (Int, Int)
coordP = do
    x <- P.string "x=" *> Lex.signed (pure ()) Lex.decimal
    _ <- P.char ','
    _ <- P.space 
    y <- P.string "y=" *> Lex.signed (pure ()) Lex.decimal
    pure (x, y)

rowP :: Parser Sensor
rowP = do
    _ <- P.string "Sensor at "
    sensorC <- coordP
    _ <- P.char ':'
    _ <- P.space
    _ <- P.string "closest beacon is at "
    beaconC <- coordP
    _ <- P.eol
    pure $ Sensor
        { _sLoc = sensorC
        , _sBeacon = Beacon beaconC
        }

inputP :: Parser (NonEmpty Sensor)
inputP = partialNonEmpty <$> P.someTill rowP P.eof

-- * Logic

manhattanS :: Sensor -> Int
manhattanS s = manhattan (s ^. sLoc) (s ^. sBeacon ^. unBeacon)

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

inSensorRange :: (Int, Int) -> Sensor -> Bool
inSensorRange p s =
    manhattanS s >= manhattan p (s ^. sLoc)
    && p /= (s ^. sBeacon ^. unBeacon)

cannotBe :: (Int, Int) -> NonEmpty Sensor -> Bool
cannotBe p = any (inSensorRange p)

xbounds :: NonEmpty Sensor -> (Int, Int)
xbounds ss =
    let sloc0 = head ss ^. sLoc ^. L._1'
    in foldl' f (sloc0, sloc0) ss
  where
    f :: (Int, Int) -> Sensor -> (Int, Int)
    f (leftmost, rightmost) s =
        (min (leftest s) leftmost, max (rightest s) rightmost)

    leftest :: Sensor -> Int
    leftest s = (s ^. sLoc ^. L._1') - manhattanS s 

    rightest :: Sensor -> Int
    rightest s = (s ^. sLoc ^. L._1') + manhattanS s

solve1atY :: NonEmpty Sensor -> Int -> Int
solve1atY ss y =
    let (start, end) = xbounds ss
        bs :: NonEmpty Bool = fmap (flip cannotBe ss) (zip [start .. end] (repeat y))
     in List.length $ filter id bs

-- * Part 2

data Rhombus = Rhombus
    { _rOrigo :: !(Int, Int)
    , _rMh :: !Int  -- Manhattan
    }
    deriving (Eq, Show)
$(LensTH.makeLenses ''Rhombus)

sensorToRhombus :: Sensor -> Rhombus
sensorToRhombus s = Rhombus
    { _rOrigo = s ^. sLoc
    , _rMh = manhattanS s
    }

xrangeAt :: Int -> Rhombus -> (Int, Int)
xrangeAt y r =
    let (rx, ry) = r ^. rOrigo
        rmh = r ^. rMh
        dy = abs (ry - y)
        lb = rx - (rmh - dy)
        rb = rx + (rmh - dy)
     in (lb, rb)

inRange :: Int -> (Int, Int) -> Bool
inRange n (x, y) = n >= x && n <= y

limit :: Int
limit = 4_000_000

searchRowY :: Int -> [Rhombus] -> Maybe (Int, Int)
searchRowY y rhombi = go 0
  where
    go :: Int -> Maybe (Int, Int)
    go x
        | x >= limit = Nothing
        | otherwise = case skip x rhombi of
            Just newX -> go newX
            Nothing -> Just (x, y)

    skip :: Int -> [Rhombus] -> Maybe Int
    skip _ [] = Nothing
    skip n (r : rs)
        | n `inRange` (lb, rb) = Just (rb + 1)
        | otherwise = skip n rs
      where
        (lb, rb) = xrangeAt y r
 
searchGrid :: [Rhombus] -> Maybe (Int, Int)
searchGrid rs = go [0 .. limit]
  where
    go :: [Int] -> Maybe (Int, Int)
    go [] = Nothing
    go (y : ys) = searchRowY y rs <|> go ys

-- * Solvers

-- | Solve part 1.
solve1 :: Text -> SolverResult
solve1 = SR . flip solve1atY 2_000_000 . partialParseText inputP

-- | Solve part 2.
solve2 :: Text -> SolverResult
solve2 = SR
    . uncurry (+)
    . first (* limit)
    . partialFromJust
    . searchGrid
    . fmap sensorToRhombus
    . toList
    . partialParseText inputP
