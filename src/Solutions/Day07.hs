{-# language TemplateHaskell #-}

module Solutions.Day07 where

import Prelube

import Control.Lens.Combinators qualified as L
import Control.Lens.TH qualified as LensTH
import Control.Monad (forM_)
import Control.Monad.State.Strict (gets, modify')
import Control.Monad.State.Strict qualified as State
import Data.Foldable qualified as Foldable
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.List qualified as List
import Data.Sequence qualified as Seq


import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as Lex


-- * Data types

data Cmd = Cd Location | Ls (Seq DirItem)
    deriving (Eq, Show)

data Location = Root | Back | Relative Text
    deriving (Eq, Show)

data DirItem = DirEntry Text | FileEntry Text Int
    deriving (Eq, Show)

-- | Fully qualified path.  The "/" location is inidcated by an empty list.
newtype Path = Path { unPath :: Seq Text }
    deriving (Eq, Show, Hashable)

pop :: Path -> Path
pop (Path p) = case p of
    Empty    -> Path Empty
    ps :|> _ -> Path ps

push :: Text -> Path -> Path
push segment (Path p) = Path (p |> segment)

type DirMap = HashMap Path Dir
type Sizes = HashMap Path Int

data Dir = Dir
    { _dirPath :: Path
    , _dirFiles :: Seq (Text, Int)
    , _dirDirs :: Seq Text
    }
    deriving (Eq, Show)
$(LensTH.makeLenses ''Dir)

data Env = Env
    { _envPwd :: Path
    , _envMap :: DirMap
    }
    deriving (Eq, Show)
$(LensTH.makeLenses ''Env)

getSizes :: Env -> Sizes
getSizes env = State.execState fill (HM.empty)
  where
    dmap :: DirMap
    dmap = L.view envMap env

    sortedPaths :: Seq Path
    sortedPaths = Seq.sortOn (Seq.length . unPath) (Seq.fromList (HM.keys dmap))

    fill :: State Sizes ()
    fill = do
        forM_ sortedPaths $ \p -> do
            s <- getDirSize p
            modify' (HM.insert p s)

        pure ()

    getDirSize :: Path -> State Sizes Int
    getDirSize p = gets id <&> HM.lookup p >>= \case
        Just size -> pure size
        Nothing -> do
            let directFiles :: Seq (Text, Int) = L.view dirFiles (dmap HM.! p)
            let directSizes :: Int = Foldable.foldl' (+) 0 (fmap snd directFiles)

            let childPaths :: Seq Path = fmap (\t -> push t p) (L.view dirDirs (dmap HM.! p))
            childSizeTotal <- Foldable.foldl' (+) 0 <$> mapM getDirSize childPaths

            pure $ childSizeTotal + directSizes

buildStructure :: [Cmd] -> Env
buildStructure commands = State.execState (build commands) (Env (Path []) HM.empty)
  where
    build :: [Cmd] -> State Env ()
    build cs = mapM_ runCmd cs
        
runCmd :: Cmd -> State Env ()
runCmd = \case
    Cd loc -> do
        prevPwd <- L.use envPwd
        case loc of
            Relative t -> addDirToDirPath t prevPwd
            _ -> pure ()

        newPath <- move loc <$> L.use envPwd
        L.assign envPwd newPath
        L.modifying envMap (addDir newPath)
    Ls items -> do
        pwd <- L.use envPwd
        let (dirs, files) = groupItems items
        mapM_ (\name -> L.modifying envMap (addDir (push name pwd))) dirs
        mapM_ (\file -> addFileToDirAtPath file pwd) files
  where
    addFileToDirAtPath :: (Text, Int) -> Path -> State Env ()
    addFileToDirAtPath f p = do
        m :: DirMap <- L.use envMap
        let alterer = L.over dirFiles (|> f)
        let updatedDir = HM.adjust alterer p m
        L.modifying envMap (const updatedDir)

    addDirToDirPath :: Text -> Path -> State Env ()
    addDirToDirPath d p = do
        m :: DirMap <- L.use envMap
        let alterer = L.over dirDirs (|> d)
        let updatedDir = HM.adjust alterer p m
        L.modifying envMap (const updatedDir)

    groupItems :: Seq DirItem -> (Seq Text, Seq (Text, Int))
    groupItems = go ([], [])
      where
        go :: (Seq Text, Seq (Text, Int)) -> Seq DirItem -> (Seq Text, Seq (Text, Int))
        go acc Empty = acc
        go (dirs, files) (x :<| xs) = case x of
            DirEntry name       -> go (dirs |> name, files) xs
            FileEntry name size -> go (dirs, files |> (name, size)) xs

    addDir :: Path -> DirMap -> DirMap
    addDir p = HM.insertWith (\_new old -> old) p (Dir p [] [])

    move :: Location -> Path -> Path
    move loc p = case loc of
        Root -> Path Empty
        Back -> pop p
        Relative txt -> push txt p

-- * Parsing

type Parser = P.Parsec Void Text

inputP :: Parser [Cmd]
inputP = P.many cmdP

cmdP :: Parser Cmd
cmdP = cdP <|> lsP

cdP :: Parser Cmd
cdP = do
    _ <- P.string "$ cd"
    _ <- P.char ' '
    loc <- locP <* P.eol
    pure (Cd loc)

locP :: Parser Location
locP = relP <|> backP <|> rootP
  where
    backP :: Parser Location
    backP = P.string ".." *> pure Back

    rootP :: Parser Location
    rootP = P.char '/' *> pure Root

    relP :: Parser Location
    relP = Relative . toTxt <$> P.some (P.letterChar)

lsP :: Parser Cmd
lsP = do
    _ <- P.string "$ ls"
    _ <- P.eol
    dirItems <- P.many (dirItemP <* P.eol)
    pure $ Ls (Seq.fromList dirItems)

dirItemP :: Parser DirItem
dirItemP = dirEntryP <|> fileEntryP

dirEntryP :: Parser DirItem
dirEntryP = do
    _ <- P.string "dir"
    _ <- P.char ' '
    DirEntry . toTxt <$> P.some P.letterChar

fileEntryP :: Parser DirItem
fileEntryP = do
    size <- Lex.decimal
    _ <- P.space
    name <- P.some (P.letterChar <|> P.char '.')
    pure (FileEntry (toTxt name) size)

-- * Solvers need to go at the bottom because of TH

-- | Solve part 1.
solve1 :: Text -> SolverResult
solve1 = SR
    . HM.foldl' (+) 0
    . HM.filter (<= 100_000)
    . getSizes
    . buildStructure
    . partialParseText inputP

-- | Solve part 2.
solve2 :: Text -> SolverResult
solve2 = SR
    . List.head
    . List.sort
    . HM.elems
    . (\sizeMap -> HM.filter (>= amountToDelete sizeMap) sizeMap)
    . getSizes
    . buildStructure
    . partialParseText inputP
  where
    amountToDelete :: Sizes -> Int
    amountToDelete sizeMap = 30_000_000 - (70_000_000 - (sizeMap HM.! (Path [])))

    
