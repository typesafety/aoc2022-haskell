module Solutions.Day05 where

import Prelube

import Data.List qualified as List
import Data.Maybe (listToMaybe)

import Data.IntMap.Strict qualified as IM
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as Lex


-- | Solve part 1.
solve1 :: Text -> SolverResult
solve1 = SR
    . IM.foldl (\acc xs -> acc <> ((maybe "" ((: "")  . unCrate)  . listToMaybe) xs)) ""
    . partialFromJust
    . uncurry (flip (steps move1))
    . partialParseText inputP

-- | Solve part 2.
solve2 :: Text -> SolverResult
solve2 = SR
    . IM.foldl (\acc xs -> acc <> ((maybe "" ((: "")  . unCrate)  . listToMaybe) xs)) ""
    . partialFromJust
    . uncurry (flip (steps move2))
    . partialParseText inputP

steps :: (Int -> [Crate] -> [Crate] -> Maybe ([Crate], [Crate])) -> [Instruction] -> IntMap [Crate] -> Maybe (IntMap [Crate])
steps moveStrat instructions im = case instructions of
    []     -> Just im
    i : is -> step moveStrat i im >>= steps moveStrat is

step :: (Int -> [Crate] -> [Crate] -> Maybe ([Crate], [Crate])) -> Instruction -> IntMap [Crate] -> Maybe (IntMap [Crate])
step moveStrat instr im = do
    from <- IM.lookup instr.from im
    to <- IM.lookup instr.to im
    (newFrom, newTo) <- moveStrat instr.count from to
    pure . IM.insert instr.to newTo . IM.insert instr.from newFrom $ im

move1 :: Int -> [Crate] -> [Crate] -> Maybe ([Crate], [Crate])
move1 0 from to = Just (from, to)
move1 count (f : from) to = move1 (count - 1) from (f : to)
move1 _ [] _ = Nothing

move2 :: Int -> [Crate] -> [Crate] -> Maybe ([Crate], [Crate])
move2 count from to = Just (List.drop count from, List.take count from <> to)

type Parser = P.Parsec Void Text

inputP :: Parser (IntMap [Crate], [Instruction])
inputP = do
    crates <- crateStacksP
    _ <- P.takeWhileP Nothing (/= '\n')
    _ <- P.eol
    _ <- P.eol
    instructions <- instructionsP
    pure (crates, instructions)

-- * Parse instructions

data Instruction = Instruction
    { count :: Int
    , from :: Int
    , to :: Int
    }
    deriving (Eq, Show)

instructionsP :: Parser [Instruction]
instructionsP = P.someTill instructionP P.eof

instructionP :: Parser Instruction
instructionP = do
    count <- P.string "move" *> P.char ' ' *> Lex.decimal <* P.char ' '
    from <- P.string "from" *> P.char ' ' *> Lex.decimal <* P.char ' '
    to <- P.string "to" *> P.char ' ' *> Lex.decimal <* P.eol
    pure $ Instruction count from to

-- * Parse crates

newtype Crate = Crate { unCrate :: Char }
    deriving (Eq, Show)

crateStacksP :: Parser (IntMap [Crate])
crateStacksP = do
    maps :: [IntMap Crate] <- P.some (P.try crateRowP)

    -- Merge the list of mappings into list (stacks).
    pure $
        List.foldl'
            (monoidFoldMerge (flip (:)))
            IM.empty
            (List.reverse maps)
  where
    monoidFoldMerge :: Monoid a => (a -> b -> a) -> IntMap a -> IntMap b -> IntMap a
    monoidFoldMerge f =
        IM.mergeWithKey
            (\_ x1 x2 -> Just (f x1 x2))  -- Apply the merge function, ignoring the key.
            id                            -- A nonempty subtree is inserted as-is into the output.
            (IM.map (f mempty))           -- A nonempty subtree is merged with the monoid identity.

crateRowP :: Parser (IntMap Crate)
crateRowP = do
    xs :: [Maybe Crate] <- P.sepBy crateSpaceP (P.char ' ') <* P.eol
    pure . IM.mapMaybe id . IM.fromAscList $ List.zip [1..] xs

crateP :: Parser Crate
crateP = Crate <$> (P.char '[' *> P.upperChar <* P.char ']')

crateSpaceP :: Parser (Maybe Crate)
crateSpaceP = (Just <$> crateP) <|> (P.count 3 (P.char ' ') *> pure Nothing)

