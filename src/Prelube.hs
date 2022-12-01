-- | Alternative prelude.
--
-- Add imports here for things used often.

module Prelube (
    module Prelube,

    module Prelude,
    module Data.Coerce,
    module Debug.Pretty.Simple,
    module Txt,
) where

import Prelude

import Data.Coerce (coerce)
import Data.Text (Text)
import Data.Text qualified as Txt
import Data.Text.IO qualified as Txt
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
