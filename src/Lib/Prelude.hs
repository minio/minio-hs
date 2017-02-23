{-
Welcome to your custom Prelude
Export here everything that should always be in your library scope
For more info on what is exported by Protolude check:
https://github.com/sdiehl/protolude/blob/master/Symbols.md
-}
module Lib.Prelude
    ( module Exports
    , both

    , format
    ) where

import           Protolude as Exports

import           Data.Time as Exports (UTCTime(..), diffUTCTime)
import           Control.Monad.Trans.Maybe as Exports (runMaybeT, MaybeT(..))

import           Control.Monad.Catch as Exports (throwM, MonadThrow, MonadCatch)

import           Data.Text.Format as Exports (Shown(..))
import qualified Data.Text.Format as TF
import           Data.Text.Format.Params (Params)
import qualified Data.Text.Lazy as LT

format :: Params ps => TF.Format -> ps -> Text
format f args = LT.toStrict $ TF.format f args

-- import Data.Tuple as Exports (uncurry)

-- | Apply a function on both elements of a pair
both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b)
