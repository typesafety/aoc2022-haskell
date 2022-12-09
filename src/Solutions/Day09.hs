{-# language TemplateHaskell #-}

module Solutions.Day09 where

import Prelube

import Control.Lens.Combinators qualified as L
import Control.Lens.TH qualified as LensTH
import Control.Monad.State.Strict qualified as State
import Data.HashSet qualified as HS

import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as Lex


-- | Visited coordinates.
type Visited = HS.HashSet (Int, Int)

data Env = Env
    { _envHead :: (Int, Int)
    , _envTails :: [(Int, Int)]
    , _envVisited :: Visited
    }
    deriving (Eq, Show)
$(LensTH.makeLenses ''Env)

initEnv1 :: Env
initEnv1 = Env
    { _envHead = (0, 0)
    , _envTails = [(0, 0)]
    , _envVisited = HS.singleton (0, 0)
    }

initEnv2 :: Env
initEnv2 = Env
    { _envHead = (0, 0)
    , _envTails = replicate 9 (0, 0)
    , _envVisited = HS.singleton (0, 0)
    }

runMotions :: [Motion] -> Env -> Env
runMotions motions = State.execState (steps motions)
  where
    steps :: [Motion] -> State Env ()
    steps [] = pure ()
    steps (m : ms) = do
        _ <- stepHead m
        newHead <- L.use envHead

        oldTails <- L.use envTails
        newTails <- moveTails oldTails newHead

        L.assign envTails newTails
        L.modifying envVisited (HS.insert (partialLast newTails))

        steps ms

    moveTails :: [(Int, Int)] -> (Int, Int) -> State Env [(Int, Int)]
    moveTails [] _ = pure []
    moveTails (t : ts) inFront = do
        let newLoc = t `moveToward` inFront
        rest <- moveTails ts newLoc
        pure $ newLoc : rest

    moveToward :: (Int, Int) -> (Int, Int) -> (Int, Int)
    moveToward me@(x, y) target@(tx, ty)
        | me `touching` target = me
        | x == tx && y < ty = (x, y + 1)
        | x == tx && y > ty = (x, y - 1)

        | y == ty && x < tx = (x + 1, y)
        | y == ty && x > tx = (x - 1, y)

        | x > tx && y > ty = (x - 1, y - 1)
        | x > tx && y < ty = (x - 1, y + 1)

        | x < tx && y > ty = (x + 1, y - 1)
        | x < tx && y < ty = (x + 1, y + 1)

        | otherwise = error "FUCK"

stepHead :: Motion -> State Env ()
stepHead m = do
    currH <- L.use envHead
    let newH = move m currH
    L.assign envHead newH

-- | Return True if the two coordinates touch.
touching :: (Int, Int) -> (Int, Int) -> Bool
touching  (meX, meY) (youX, youY) = abs (meX - youX) <= 1 && abs (meY - youY) <= 1

-- | Move a coordinate in the given direction.
move :: Motion -> (Int, Int) -> (Int, Int)
move m (x, y) = case m of
    L -> (x - 1, y)
    D -> (x, y + 1)
    U -> (x, y - 1)
    R -> (x + 1, y)

-- * Parsing

type Parser = P.Parsec Void Text

data Motion = L | D | U | R
    deriving (Eq, Show)

inputP :: Parser [Motion]
inputP = do
    mss <- P.someTill motionP P.eof
    pure $ mconcat mss

motionP :: Parser [Motion]
motionP = do
    dir <- P.upperChar <&> \case
        'L' -> L
        'D' -> D
        'U' -> U
        'R' -> R
        c -> error $ "Invalid direction: " <> [c]
    _ <- P.char ' '
    numSteps <- Lex.decimal
    _ <- P.eol
    pure $ replicate numSteps dir

-- | Solve part 1.
solve1 :: Text -> SolverResult
solve1 = SR
    . HS.size
    . L.view envVisited
    . flip runMotions initEnv1
    . partialParseText inputP

-- | Solve part 2.
solve2 :: Text -> SolverResult
solve2 = SR
    . HS.size
    . L.view envVisited
    . flip runMotions initEnv2
    . partialParseText inputP