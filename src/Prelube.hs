{- | Alternative prelude.

 Add imports here for things used often.
-}
module Prelube (
    module PreludeLess,
    module Prelube,
    module Control.Applicative,
    module Control.Monad,
    module Control.Monad.State.Strict,
    module Data.Bifunctor,
    module Data.Char,
    module Data.Coerce,
    module Data.HashMap.Strict,
    module Data.Functor,
    module Data.IntMap.Strict,
    module Data.Kind,
    module Data.List.NonEmpty,
    module Data.Maybe,
    module Data.Sequence,
    module Data.Text,
    module Data.Void,
    module Debug.Pretty.Simple,
    module TextShow,
) where

import Prelude as PreludeLess hiding (
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
import Control.Monad ((>=>), (<=<))
import Control.Monad.State.Strict (State)
import Data.Bifunctor
import Data.Char (digitToInt)
import Data.Coerce (coerce)
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.IntMap.Strict (IntMap)
import Data.Kind (Type)
import Data.List.NonEmpty hiding ((<|))
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence (Seq(..), (<|), (|>))
import Data.Text (Text)
import Data.Text qualified as Txt
import Data.Text.IO qualified as Txt
import Data.Void (Void)
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
import GHC.Stack (HasCallStack)
import Text.Megaparsec qualified as P
import TextShow hiding (singleton)

-- * Glue

data SolverResult :: Type where
    SR :: TextShow a => a -> SolverResult

-- | Convert a showtSR to a Text.
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

{-# WARNING todo "Unhandled TODO placeholder expression." #-}
todo :: HasCallStack => a
todo = error "ERROR: TODO IN CODE"

{-# WARNING correct "Use of correct/unsafeCoerce in code." #-}
correct :: HasCallStack => a -> b
correct = unsafeCoerce
