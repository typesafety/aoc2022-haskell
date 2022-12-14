{- | Alternative prelude.

 Add imports here for things used often.
-}
module Prelube (
    module PreludeLess,
    module Prelube,

    module Control.Applicative,
    module Control.Monad,
    module Control.Monad.Reader,
    module Control.Monad.State.Strict,
    module Data.Bifunctor,
    module Data.Char,
    module Data.Coerce,
    module Data.Functor,
    module Data.HashMap.Strict,
    module Data.HashSet,
    module Data.IntMap.Strict,
    module Data.Kind,
    module Data.List.NonEmpty,
    module Data.Maybe,
    module Data.Sequence,
    module Data.Text,
    module Data.Void,
    module Debug.Trace,
    module System.IO.Unsafe,

    module Debug.Pretty.Simple,
    module TextShow,
) where

import Prelude as PreludeLess hiding (
    -- Hide functions on lists by defaultin favor of NonEmpty variants.
    break,
    cycle,
    drop,
    dropWhile,
    filter,
    head,
    init,
    iterate,
    last,
    length,
    map,
    repeat,
    reverse,
    scanl,
    scanl1,
    scanr,
    scanr1,
    span,
    splitAt,
    tail,
    take,
    takeWhile,
    unzip,
    zip,
    zipWith,
    (!!),
 )

import Control.Applicative ((<|>))
import Control.Monad ((>=>), (<=<), when, forM_, forM,void, replicateM_)
import Control.Monad.Reader (Reader, ReaderT)
import Control.Monad.State.Strict (State, StateT)
import Data.Bifunctor
import Data.Char (digitToInt)
import Data.Coerce (coerce)
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.IntMap.Strict (IntMap)
import Data.Kind (Type)
import Data.List qualified as List
import Data.List.NonEmpty hiding ((<|))
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence (Seq(..), (<|), (|>))
import Data.Text (Text)
import Data.Text qualified as Txt
import Data.Text.IO qualified as Txt
import Data.Void (Void)
import Debug.Trace (
    trace,
    traceIO,
    traceId,
    traceM,
    traceShow,
    traceShowId,
    traceShowM,
 )
import GHC.Stack (HasCallStack)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import Debug.Pretty.Simple (
    pTrace,
    pTraceIO,
    pTraceId,
    pTraceM,
    pTraceShow,
    pTraceShowId,
    pTraceShowM,
 )
import Text.Megaparsec qualified as P
import TextShow hiding (singleton)


-- * Glue

data SolverResult :: Type where
    SR :: TextShow a => a -> SolverResult

-- | Convert a SolverResult to a Text.
showtSR :: SolverResult -> Text
showtSR (SR t) = showt t

-- * Parsing and plumbing

-- TODO: output error message on parse fail
partialParseText :: HasCallStack => P.Parsec Void Text b -> Text -> b
partialParseText p = fromMaybe (error "partialParseText: failed parse") . P.parseMaybe p

partialNonEmpty :: HasCallStack => [a] -> NonEmpty a
partialNonEmpty xs = case nonEmpty xs of
    Just ys -> ys
    Nothing -> error "partialNonEmpty: empty list"

partialFromJust :: HasCallStack => Maybe a -> a
partialFromJust = \case
    Just x -> x
    Nothing -> error "partialFromJust: Nothing"

partialHead :: HasCallStack => [a] -> a
partialHead = \case
    [] -> error "partialHead: empty list"
    xs -> List.head xs

partialLast :: HasCallStack => [a] -> a
partialLast = \case
    [] -> error "partialLast: empty list"
    xs -> List.last xs

-- * Working with Text

putTxtLn :: Text -> IO ()
putTxtLn = Txt.putStrLn

toTxt :: String -> Text
toTxt = Txt.pack

toStr :: Text -> String
toStr = Txt.unpack

-- * Bifunctor

both :: Bifunctor p => (a -> b) -> p a a -> p b b
both f = bimap f f

-- * Misc

-- | Apply a function N times.
applyN :: Int -> (a -> a) -> a -> a
applyN n f = List.foldl' (.) id (replicate n f)

-- | n - 1
decr :: Integral a => a -> a
decr n = n - 1

-- * Placeholders

{-# WARNING todo "Unhandled TODO placeholder expression." #-}
todo :: HasCallStack => a
todo = error "ERROR: TODO IN CODE"

{-# WARNING correct "Use of correct/unsafeCoerce in code." #-}
correct :: HasCallStack => a -> b
correct = unsafeCoerce
