{-# language TemplateHaskell #-}

module Solutions.Day10 where

import Prelube

import Control.Lens.Combinators qualified as L
import Control.Lens.Operators ((^.))
import Control.Lens.TH qualified as LensTH
import Data.List qualified as List

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

-- | Solve part 1.
solve1 :: Text -> SolverResult
solve1 = SR . sum . signalStrengthsAt [20, 60, 100, 140, 180, 220] . partialParseText inputP

-- | Solve part 2.
solve2 :: Text -> SolverResult
solve2 = todo
