{-# language TemplateHaskell #-}

module Solutions.Day15 where

import Prelube

import Control.Lens.Combinators qualified as L
import Control.Lens.Operators ((^.))
import Control.Lens.TH qualified as LensTH
import Control.Monad.State.Strict qualified as State
import Data.Foldable (foldl')
import Data.IntMap.Strict qualified as IM
import Data.List qualified as List
import Data.Sequence qualified as Seq

import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as Lex

import Debug.Trace



-- $(LensTH.makeLenses ''???)

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

inRange :: (Int, Int) -> Sensor -> Bool
inRange p s = manhattanS s >= manhattan p (s ^. sLoc) && p /= (s ^. sBeacon ^. unBeacon)

cannotBe :: (Int, Int) -> NonEmpty Sensor -> Bool
cannotBe p = any (inRange p)

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
     in List.length $ filter (\b -> b) bs

-- * Solvers

-- | Solve part 1.
solve1 :: Text -> SolverResult
solve1 = SR . flip solve1atY 2_000_000 . partialParseText inputP

-- | Solve part 2.
solve2 :: Text -> SolverResult
solve2 = todo
