{-# language TemplateHaskell #-}

module Solutions.Day10 where

import Prelube

import Control.Lens.Combinators qualified as L
import Control.Lens.Operators ((^.))
import Control.Lens.TH qualified as LensTH
import Control.Monad.State.Strict qualified as State
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Sequence qualified as Seq
import System.IO.Unsafe (unsafePerformIO)

import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as Lex

-- * Parsing

type Parser = P.Parsec Void Text

data Instr = AddX Int | Noop
    deriving (Eq, Show)

addxP :: Parser Instr
addxP = AddX <$> (P.string "addx" *> P.space1 *> Lex.signed (pure ()) Lex.decimal <* P.eol)

noopP :: Parser Instr
noopP = P.string "noop" *> P.eol *> pure Noop

instrP :: Parser Instr
instrP = addxP <|> noopP

inputP :: Parser [Instr]
inputP = P.someTill instrP P.eof

data Env = Env
    { _envX :: Int
    , _envSleep :: Int
    , _envInstrs :: [Instr]
    , _envPending :: Int
    }
    deriving (Eq, Show)
$(LensTH.makeLenses ''Env)

-- * Solvers

-- | Run n instructions at a time, returning the values of X after each time.
signalStrengthsAt :: [Int] -> [Instr] -> [Int]
signalStrengthsAt stops instrs = go 0 stops (Env 1 0 instrs 0)
  where
    go :: Int -> [Int] -> Env -> [Int]
    go _ [] _ = []
    go prevS (s : ss) e =
        let newEnv = applyN (s - prevS) tick e
        in (s * newEnv ^. envX) : go s ss newEnv

tick :: Env -> Env
tick env
    | env ^. envSleep > 0 = L.over envSleep decr env
    | otherwise = 
        let newX = env ^. envPending + env ^. envX
            (h, t) = fromMaybe (Noop, []) (List.uncons (env ^. envInstrs))
            (sleep, pending) = case h of
                AddX n -> (1, n)
                Noop   -> (0, 0)
        in Env newX sleep t pending

newtype Screen = Screen { unScreen :: (Seq (Seq Pixel)) }
    deriving (Eq, Show)

initScreen :: Screen
initScreen = Screen (Seq.replicate 6 (Seq.replicate 40 Dark))

data Pixel = Lit | Dark
    deriving (Eq, Show)

toChar :: Pixel -> Char
toChar = \case
    Lit  -> '#'
    Dark -> '.'

renderScreen :: Screen -> String
renderScreen =
    Foldable.foldl' (\acc s -> acc <> s) ""
    . fmap (Foldable.toList . (|> '\n') . fmap toChar)
    . unScreen

drawOne :: Int -> Int -> Screen -> Screen
drawOne x cyc screen@(Screen s)
    | ix `elem` ([x - 1 .. x + 1] :: [Int]) = Screen $ L.set (L.ix row . L.ix ix) Lit s
    | otherwise = screen
  where
    row :: Int
    row = cyc `div` 40

    ix :: Int
    ix = cyc `mod` 40

drawAll :: [Instr] -> Screen
drawAll instrs = State.execState (go 0 (Env 1 0 instrs 0)) initScreen
  where
    go :: Int -> Env -> State Screen ()
    go 240 _   = pure ()
    go cyc env = do
        let newEnv = tick env
        State.modify' (drawOne (newEnv ^. envX) cyc)
        go (cyc + 1) newEnv

-- | Solve part 1.
solve1 :: Text -> SolverResult
solve1 = SR
    . sum
    . signalStrengthsAt [20, 60, 100, 140, 180, 220]
    . partialParseText inputP

-- | Solve part 2.
solve2 :: Text -> SolverResult
solve2 = SR
    . (\s -> flip seq '!' $ unsafePerformIO (putStrLn s))
    . renderScreen
    . drawAll
    . partialParseText inputP
