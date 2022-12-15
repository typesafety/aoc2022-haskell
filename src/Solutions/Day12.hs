module Solutions.Day12 where

import Prelube

import Control.Monad.State.Strict qualified as State
import Data.Foldable qualified as Foldable
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List qualified as List
import Data.Maybe (catMaybes)
import Data.Sequence qualified as Seq

import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P


-- * Parsing

type Parser = P.Parsec Void Text

type HeightMap = HashMap (Int, Int) Char

rowP :: Parser [Char]
rowP = P.someTill P.letterChar P.eol

-------------------------------
inputP :: Parser HeightMap  -- \ 
inputP = do ------------------- \-- \
    rows <- P.someTill rowP P.eof -- |
    pure  -------------------------- /
        $ HM.fromList  -- \
            $ mconcat   ----------- \
                $ fmap (\(y, row)  -- \
                    -> fmap (\(x, c)  -- \
                        -> ((x, y), c))  ---------- \
                            $ List.zip [0 ..] row)  --- \
                                $ List.zip [0 ..] rows --- \___
-------------------------------------------------------------- |

bfs :: DGraph (Char, (Int, Int)) -> Maybe (Seq (Int, Int))
bfs dg@(Node (_, startC) _) = State.evalState (go [([], dg)]) (HS.singleton startC)
  where
    go :: [(Seq (Int, Int), DGraph (Char, (Int, Int)))] -> State (HashSet (Int, Int)) (Maybe (Seq (Int, Int)))
    go paths = do
        newPaths <- mconcat <$> mapM step paths
        
        case Foldable.find (\(_, Node (c, _) _) -> c == 'E') paths of
            Just (ps, Node (_, p) _) -> pure (Just (ps |> p))
            Nothing
                | newPaths == paths -> pure Nothing  -- Got stuck
                | otherwise -> go newPaths

    step :: (Seq (Int, Int), DGraph (Char, (Int, Int)))
        -> State (HashSet (Int, Int)) [(Seq (Int, Int), DGraph (Char, (Int, Int)))]
    step (path, Node (_, xy) ngs) = do
        visited <- State.get

        let neighsToVisit = List.filter (\(Node (_, p) _) -> not (p `HS.member` visited)) ngs

        forM_ neighsToVisit $ \(Node (_, p) _) -> State.modify' (HS.insert p)
        pure $ fmap (path |> xy, ) neighsToVisit

nodeData :: DGraph (Char, (Int, Int)) -> (Char, (Int, Int))
nodeData (Node x _) = x

data DGraph a = Node a [DGraph a]
    deriving (Eq, Show)

buildGraph :: (Int, Int) -> HeightMap -> DGraph (Char, (Int, Int))
buildGraph start hm = build start
  where
    build :: (Int, Int) -> DGraph (Char, (Int, Int))
    build point@(x, y) =
        let neighbors =
                List.filter (\xy -> cVal (hm HM.! xy) - cVal (hm HM.! point) <= 1)
                    $ List.filter (`HM.member` hm)
                        [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

        in Node (hm HM.! point, point) (fmap build neighbors)

cVal :: Char -> Int
cVal = \case
    'S' -> 0
    'E' -> cValMap HM.! 'z'
    c   -> cValMap HM.! c

cValMap :: HashMap Char Int
cValMap = HM.fromList $ List.zip ['a' .. 'z'] [0 ..]

startCoords :: HeightMap -> (Int, Int)
startCoords = HM.foldlWithKey' (\acc coords c -> if c == 'S' then coords else acc) (0, 0)

endCoords :: HeightMap -> (Int, Int)
endCoords = HM.foldlWithKey' (\acc coords c -> if c == 'E' then coords else acc) (0, 0)

allStartCoords :: HeightMap -> [(Int, Int)]
allStartCoords = HM.keys . HM.filter (`elem` ("aS" :: String))

allGraphs :: [(Int, Int)] -> HeightMap -> [DGraph (Char, (Int, Int))]
allGraphs starts hm = fmap (flip buildGraph hm) starts

-- | Solve part 1.
solve1 :: Text -> SolverResult
solve1 = SR
    . decr
    . Seq.length
    . partialFromJust
    . bfs
    . (\hm -> buildGraph (startCoords hm) hm)
    . partialParseText inputP

-- | Solve part 2.
solve2 :: Text -> SolverResult
solve2 = SR
    . List.minimum
    . fmap (decr . List.length)
    . catMaybes
    . Foldable.toList
    . fmap bfs
    . (\hm -> allGraphs (allStartCoords hm) hm)
    . partialParseText inputP
