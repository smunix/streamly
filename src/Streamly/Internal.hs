-- |
-- Module      : Streamly.Internal
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module is only for internal or experimental APIs.  The constructors and
-- routines exposed through this module are likely to change or to be removed
-- at any time in future without notice.  There is no warranty for the routines
-- in this module to work correctly, please use at your own risk.
--
-- If you think some of the routines exported through this module should be
-- included in the officially supported modules please raise an issue at
-- https://github.com/composewell/streamly/issues .
--
module Streamly.Internal
    (
    -- * Diagnostics
      inspectMode

    -- * Streamly.Internal.Data.Fold Experimental Exports
    , Fold (..)
    , toListRevF
    , rollingHash
    , rollingHashWithSalt
    , lmap
    , lmapM
    , lfilter
    , lfilterM
    , lcatMaybes
    , ltake
    , ltakeWhile
    , lsessionsOf
    , lchunksOf
    , duplicate
    , initialize
    , runStep

    -- * Streamly.Unfold Experimental Exports
    , Unfold (..)
    , fromList

    -- * Streamly.Memory.Array
    , readU
    )
where

import Prelude hiding (break, span, splitAt)

import Streamly.Memory.Array.Types (readU)
import Streamly.Streams.Combinators (inspectMode)
import Streamly.Unfold.Types (Unfold(..))
import Streamly.Unfold (fromList)

import Streamly.Fold.Internal
import Streamly.Internal.Data.Fold.Types
