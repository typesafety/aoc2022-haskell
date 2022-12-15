{-# language TemplateHaskell #-}

module Solutions.Day15 where

import Prelube

import Control.Lens.Combinators qualified as L
import Control.Lens.Operators ((^.))
import Control.Lens.TH qualified as LensTH
import Control.Monad.State.Strict qualified as State
import Data.IntMap.Strict qualified as IM
import Data.Sequence qualified as Seq

import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as Lex



-- $(LensTH.makeLenses ''???)


-- * Parsing

newtype Beacon = Beacon { _unBeacon :: (Int, Int) }
    deriving (Eq, Show)
$(LensTH.makeLenses ''Beacon)

data Sensor = Sensor
    { _sLoc :: (Int, Int)
    , _sBeacon :: Beacon
    }
    deriving (Eq, Show)
$(LensTH.makeLenses ''Sensor)

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

-- * Solvers

-- | Solve part 1.
solve1 :: Text -> SolverResult
solve1 = todo

-- | Solve part 2.
solve2 :: Text -> SolverResult
solve2 = todo
