{-# language TemplateHaskell #-}

module Solutions.Day11 where

import Prelube

import Control.Lens.Combinators qualified as L
import Control.Lens.TH qualified as LensTH
import Control.Monad.State.Strict qualified as State
import Data.IntMap.Strict qualified as IM
import Data.Sequence qualified as Seq

import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as Lex


data Monkey = Monkey
    { _monkeyId :: Int
    , _monkeyItems :: Seq Int
    , _monkeyOp :: Int -> Int
    , _monkeyTest :: Int -> Bool
    , _monkeyTrue :: Int
    , _monkeyFalse :: Int
    , _monkeyCount :: Int
    }
$(LensTH.makeLenses ''Monkey)

makeMonkey :: Int -> Seq Int -> (Int -> Int) -> (Int -> Bool) -> Int -> Int -> Monkey
makeMonkey identity startItems operation test trueTarget falseTarget =
    Monkey {
        _monkeyId = identity,
        _monkeyItems = startItems,
        _monkeyOp = operation,
        _monkeyTest = test,
        _monkeyTrue = trueTarget,
        _monkeyFalse = falseTarget,
        _monkeyCount = 0
    }
    
instance Show Monkey where
    show m = mconcat
        [ "Monkey "
        , "T=", show (L.view monkeyTrue m), " "
        , "F=", show (L.view monkeyFalse m), " "
        , "| "
        , show (L.view monkeyItems m), " "
        ]

-- * Parsing

type Parser = P.Parsec Void Text

idP :: Parser Int
idP = P.string "Monkey " *> Lex.decimal <* P.char ':' <* P.eol

itemsP :: Parser (Seq Int)
itemsP = do
    _ <- P.space1
    _ <- P.string "Starting items:"
    _ <- P.space
    items <- P.sepBy1 Lex.decimal (P.char ',' *> P.many (P.char ' '))
    _ <- P.eol
    pure (Seq.fromList items)

operationP :: Parser (Int -> Int)
operationP = do
    _ <- P.space1
    _ <- P.string "Operation:"
    _ <- P.space
    _ <- P.string "new"
    _ <- P.space
    _ <- P.char '='
    _ <- P.space

    _ <- P.string "old"
    _ <- P.space
    op <- P.oneOf @Seq ['*', '+'] >>= \case
        '*' -> pure (*)
        '+' -> pure (+)
        c -> fail $ "Expected operand '*' or '+', got " <> [c]
    _ <- P.space

    f <- nOpP op <|> oldOpP op
    _ <- P.eol
    pure f
  where
    nOpP :: (Int -> Int -> Int) -> Parser (Int -> Int)
    nOpP op = flip op <$> Lex.decimal

    oldOpP :: (Int -> Int -> Int) -> Parser (Int -> Int)
    oldOpP op = do
        _ <- P.string "old"
        pure (\x -> x `op` x)

testP :: Parser (Int -> Bool)
testP = do
    _ <- P.space1
    _ <- "Test:"
    _ <- P.space
    _ <- P.string "divisible by"
    _ <- P.space
    amt <- Lex.decimal
    _ <- P.eol
    pure ((== 0) . flip mod amt)

targetP :: Bool -> Parser Int
targetP b = do
    _ <- P.space1
    _ <- P.string "If"
    _ <- P.space1
    _ <- P.string bText
    _ <- P.char ':'
    _ <- P.space
    _ <- P.string "throw to monkey"
    _ <- P.space1
    n <- Lex.decimal
    _ <- P.eol
    pure n
  where
    bText :: Text
    bText = if b then "true" else "false"

monkeyP :: Parser Monkey
monkeyP = do
    ident <- idP
    items <- itemsP
    op <- operationP
    test <- testP
    true <- targetP True
    false <- targetP False

    pure (makeMonkey ident items op test true false)

inputP :: Parser (IntMap Monkey)
inputP = do
    monkeys <- P.sepBy1 monkeyP P.eol
    _ <- P.eof

    pure $ IM.fromList $ fmap (\m -> (L.view monkeyId m, m)) monkeys

-- * Solvers

-- | Solve part 1.
solve1 :: Text -> SolverResult
solve1 = todo

-- | Solve part 2.
solve2 :: Text -> SolverResult
solve2 = todo
