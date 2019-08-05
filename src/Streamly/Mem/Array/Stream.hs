{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Mem.Array.Stream
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Combinators to efficiently manipulate streams of arrays.
--
module Streamly.Mem.Array.Stream
    (
    -- * Creation
      arraysOf

    -- * Flattening to elements
    , flatten
    -- , _flattenArraysRev
    , unlinesBy

    -- * Transformation
    , splitOn
    , compact -- compact

    -- * Elimination
    , toArray
    )
where

import Control.Monad.IO.Class (MonadIO(..))
-- import Data.Functor.Identity (Identity)
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (minusPtr, plusPtr, castPtr)
import Foreign.Storable (Storable(..))
import Prelude hiding (length, null, last, map, (!!), read)

import Streamly.Mem.Array.Types (Array(..), length)
import Streamly.Streams.Serial (SerialT)
import Streamly.Streams.StreamK.Type (IsStream)

import qualified Streamly.Mem.Array.Types as A
import qualified Streamly.Prelude as S
import qualified Streamly.Streams.StreamD as D
import qualified Streamly.Streams.Prelude as P

-- XXX efficiently compare two streams of arrays. Two streams can have chunks
-- of different sizes, we can handle that in the stream comparison abstraction.
-- This could be useful e.g. to fast compare whether two files differ.

-- | Convert a stream of arrays into a stream of their elements.
--
-- Same as the following but more efficient:
--
-- > flatten = S.concatMap A.read
--
-- @since 0.7.0
{-# INLINE flatten #-}
flatten :: (IsStream t, MonadIO m, Storable a) => t m (Array a) -> t m a
flatten m = D.fromStreamD $ A.flattenArrays (D.toStreamD m)

-- XXX should we have a reverseArrays API to reverse the stream of arrays
-- instead?
--
-- | Convert a stream of arrays into a stream of their elements reversing the
-- contents of each array before flattening.
--
-- @since 0.7.0
{-# INLINE _flattenArraysRev #-}
_flattenArraysRev :: (IsStream t, MonadIO m, Storable a) => t m (Array a) -> t m a
_flattenArraysRev m = D.fromStreamD $ A.flattenArraysRev (D.toStreamD m)

-- XXX use an Array instead as separator? Or use a separate unlinesArraysBySeq
-- API for that?
--
-- | Flatten a stream of arrays appending the given element after each
-- array.
--
-- @since 0.7.0
{-# INLINE unlinesBy #-}
unlinesBy :: (MonadIO m, IsStream t, Storable a)
    => a -> t m (Array a) -> t m a
unlinesBy x = D.fromStreamD . A.unlines x . D.toStreamD

-- | Split a stream of arrays on a given separator byte, dropping the separator
-- and coalescing all the arrays between two separators into a single array.
--
-- @since 0.7.0
{-# INLINE splitOn #-}
splitOn
    :: (IsStream t, MonadIO m)
    => Word8
    -> t m (Array Word8)
    -> t m (Array Word8)
splitOn byte s = D.fromStreamD $ A.splitOn byte $ D.toStreamD s

-- | Coalesce adjacent arrays in incoming stream to form bigger arrays of a
-- maximum specified size in bytes.
--
-- @since 0.7.0
{-# INLINE compact #-}
compact :: (MonadIO m, Storable a)
    => Int -> SerialT m (Array a) -> SerialT m (Array a)
compact n xs = D.fromStreamD $ A.packArraysChunksOf n (D.toStreamD xs)

-- | @arraysOf n stream@ groups the elements in the input stream into arrays of
-- @n@ elements each.
--
-- Same as the following but more efficient:
--
-- > arraysOf n = FL.chunksOf n (A.writeNF n)
--
-- @since 0.7.0
{-# INLINE arraysOf #-}
arraysOf :: (IsStream t, MonadIO m, Storable a)
    => Int -> t m a -> t m (Array a)
arraysOf n str =
    D.fromStreamD $ A.fromStreamDArraysOf n (D.toStreamD str)

-- XXX Both of these implementations of splicing seem to perform equally well.
-- We need to perform benchmarks over a range of sizes though.

-- CAUTION! length must more than equal to lengths of all the arrays in the
-- stream.
{-# INLINE spliceArraysLenUnsafe #-}
spliceArraysLenUnsafe :: (MonadIO m, Storable a)
    => Int -> SerialT m (Array a) -> m (Array a)
spliceArraysLenUnsafe len buffered = do
    arr <- liftIO $ A.newArray len
    end <- S.foldlM' writeArr (aEnd arr) buffered
    return $ arr {aEnd = end}

    where

    writeArr dst Array{..} =
        liftIO $ withForeignPtr aStart $ \src -> do
                        let count = aEnd `minusPtr` src
                        A.memcpy (castPtr dst) (castPtr src) count
                        return $ dst `plusPtr` count

{-# INLINE _spliceArraysBuffered #-}
_spliceArraysBuffered :: (MonadIO m, Storable a)
    => SerialT m (Array a) -> m (Array a)
_spliceArraysBuffered s = do
    buffered <- P.foldr S.cons S.nil s
    len <- S.sum (S.map length buffered)
    spliceArraysLenUnsafe len s

{-# INLINE spliceArraysRealloced #-}
spliceArraysRealloced :: forall m a. (MonadIO m, Storable a)
    => SerialT m (Array a) -> m (Array a)
spliceArraysRealloced s = do
    idst <- liftIO $ A.newArray (A.bytesToCount (undefined :: a)
                                (A.mkChunkSizeKB 4))

    arr <- S.foldlM' A.spliceWithDoubling idst s
    liftIO $ A.shrinkToFit arr

-- | Given a stream of arrays, splice them all together to generate a single
-- array. The stream must be /finite/.
--
-- @since 0.7.0
{-# INLINE toArray #-}
toArray :: (MonadIO m, Storable a) => SerialT m (Array a) -> m (Array a)
toArray = spliceArraysRealloced
-- spliceArrays = _spliceArraysBuffered

-- exponentially increasing sizes of the chunks upto the max limit.
-- XXX this will be easier to implement with parsers/terminating folds
-- With this we should be able to reduce the number of chunks/allocations.
-- The reallocation/copy based toArray can also be implemented using this.
--
{-
{-# INLINE toArraysInRange #-}
toArraysInRange :: (IsStream t, MonadIO m, Storable a)
    => Int -> Int -> Fold m (Array a) b -> Fold m a b
toArraysInRange low high (Fold step initial extract) =
-}

{-
-- | Fold the input to a pure buffered stream (List) of arrays.
{-# INLINE _toArraysOf #-}
_toArraysOf :: (MonadIO m, Storable a)
    => Int -> Fold m a (SerialT Identity (Array a))
_toArraysOf n = FL.lchunksOf n (A.writeNF n) FL.toStream
-}
