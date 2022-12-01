{- | Alternative prelude.

 Add imports here for things used often.
-}
module Prelube (
    module PreludeLess,
    module Prelube,
    module Data.Char,
    module Data.Coerce,
    module Data.List.NonEmpty,
    module Data.Maybe,
    module Data.Text,
    module Data.Void,
    module Debug.Pretty.Simple,
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

import Data.Char (digitToInt)
import Data.Coerce (coerce)
import Data.List.NonEmpty
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Txt
import Data.Text.IO qualified as Txt
import Data.Void (Void)
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
import Unsafe.Coerce (unsafeCoerce)

-- * Parsing

partialParseText :: HasCallStack => P.Parsec Void Text b -> Text -> b
partialParseText p = fromMaybe (error "partialParseText: failed parse") . P.parseMaybe p

partialNonEmpty :: HasCallStack => [a] -> NonEmpty a
partialNonEmpty xs = case nonEmpty xs of
    Just ys -> ys
    Nothing -> error "partialNonEmpty: empty list"

-- * Working with Text

putTxtLn :: Text -> IO ()
putTxtLn = Txt.putStrLn

toTxt :: String -> Text
toTxt = Txt.pack

toStr :: Text -> String
toStr = Txt.unpack

-- * Misc

{-# WARNING todo "Unhandled TODO placeholder expression." #-}
todo :: HasCallStack => a
todo = error "ERROR: TODO IN CODE"

{-# WARNING correct "Use of correct/unsafeCoerce in code." #-}
correct :: HasCallStack => a -> b
correct = unsafeCoerce
