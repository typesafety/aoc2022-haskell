{-# language TemplateHaskell #-}

module Solutions.Day16 where

import Prelube

import Control.Lens.Combinators qualified as L
import Control.Lens.Operators ((^.))
import Control.Lens.TH qualified as LensTH
import Control.Monad.Reader (ask, asks)
import Control.Monad.Reader qualified as Reader
import Control.Monad.State.Strict qualified as State
import Data.Foldable qualified as Foldable
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
import Data.List qualified as List
import Data.Sequence qualified as Seq

import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as Lex


-- * Parsing

data Cave = Cave
    { _cNeighs :: [String]
    , _cFlow :: Int  -- Pressure per second
    , _cName :: String
    }
    deriving (Eq, Show)
$(LensTH.makeLenses ''Cave)

newtype Time = Time { _unTime :: Int }
    deriving (Eq, Show, Ord, Num, Enum, Real, Integral, Hashable) via Int
$(LensTH.makeLenses ''Time)

type Parser = P.Parsec Void Text

rowP :: Parser Cave
rowP = do
    _ <- P.string "Valve "
    name <- nameP
    _ <- P.string " has flow rate="
    flow <- Lex.decimal
    _ <- P.string "; "
    _ <- P.string "tunnels" <|> P.string "tunnel"
    _ <- P.space1
    _ <- P.string "leads" <|> P.string "lead"
    _ <- P.string " to "
    _ <- P.string "valves" <|> P.string "valve"
    _ <- P.space1
    neighs <- P.sepBy1 nameP (P.string ", ")
    _ <- P.eol
    pure $ Cave neighs flow name

nameP :: Parser String
nameP = P.count 2 P.upperChar

inputP :: Parser [Cave]
inputP = P.someTill rowP P.eof

-- * Solvers

type Caves = HashMap String Cave

data Action = Open String | Move String
    deriving (Eq, Show)

data St = St
    { _sOpt :: HashMap (Time, String, HashSet String) (Seq Action)
    }
    deriving (Eq, Show)
$(LensTH.makeLenses ''St)

data Env = Env
    { _eCaves :: Caves
    , _eTime :: Time
    , _eClosed :: HashSet String
    }
    deriving (Eq, Show)
$(LensTH.makeLenses ''Env)

type Calc = ReaderT Env (State St)

caves :: [Cave] -> Caves
caves = HM.fromList . fmap (\c -> (c ^. cName, c))

limit :: Time
limit = 30

scoreCave :: Time -> Cave -> Int
scoreCave t c = t ^. unTime * c ^. cFlow

scorePath :: Time -> Caves -> Seq Action -> Int
scorePath _ _ Empty = 0
scorePath 0 _ _ = 0
scorePath t cs (a :<| as) = case a of
    Open name ->
        scoreCave (t - 1) (cs HM.! name)
        + scorePath (t - 1) cs as
    Move _ -> scorePath (t - 1) cs as

openZeroes :: Env -> Env
openZeroes e = L.over eClosed (HS.filter (\s -> ((e ^. eCaves) HM.! s) ^. cFlow /= 0)) e

runCalc :: [Cave] -> Seq Action
runCalc cs = flip State.evalState initSt (calc "AA" (openZeroes initEnv))
  where
    initEnv = Env (caves cs) limit (HS.fromList $ fmap (^. cName) cs)
    initSt = St HM.empty

calc :: String -> Env -> State St (Seq Action)
calc name env = L.uses sOpt (HM.lookup (env ^. eTime, name, env ^. eClosed)) >>= \case
    Just saved -> pure saved
    Nothing
        -- All valves are opened, nothing more to do.
        | HS.null (env ^. eClosed) -> pure []
        -- Time's up, nothing more to do.
        | env ^. eTime <= 0 -> pure []

        | otherwise -> do
            movedOnly <- visitNeighs (envMove env)
            opened <- case HS.member name (env ^. eClosed) of
                True -> fmap (Open name <|) <$> visitNeighs (envOpen env)
                False -> pure []
            let best = case sortPathsOnScore env (opened <> movedOnly) of
                    _ :|> top -> top
                    Empty     -> Empty

            L.modifying sOpt (HM.insert (env ^. eTime, name, env ^. eClosed) best)
            pure best

  where
    envOpen :: Env -> Env
    envOpen = L.over eClosed (HS.delete name) . L.over eTime decr

    envMove :: Env -> Env
    envMove = id

    sortPathsOnScore :: Env -> Seq (Seq Action) -> Seq (Seq Action)
    sortPathsOnScore e = Seq.unstableSortOn (scorePath (e ^. eTime) (e ^. eCaves))

    visitNeighs :: Env -> State St (Seq (Seq Action))
    visitNeighs e = do
        let neighs = ((e ^. eCaves) HM.! name) ^. cNeighs
        forM (Seq.fromList neighs) $ \s -> do
            path <- calc s (L.over eTime (\t -> t - 1) e)
            pure $ Move s <| path

-- | Solve part 1.
solve1 :: Text -> SolverResult
solve1 = SR . (\cs -> scorePath limit (caves cs) $ runCalc cs) . partialParseText inputP

-- | Solve part 2.
solve2 :: Text -> SolverResult
solve2 = todo
