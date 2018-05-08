-- |
-- Module      : Streamly
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- The way a list represents a sequence of pure values, a stream represents a
-- sequence of monadic actions. The monadic stream API offered by Streamly is
-- very close to the Haskell "Prelude" pure lists' API, it can be considered as
-- a natural extension of lists to monadic actions. Streamly streams provide
-- concurrent composition and merging of streams. It can be considered as a
-- concurrent list transformer. In contrast to the "Prelude" lists, merging or
-- appending streams of arbitrary length is scalable and inexpensive.
--
-- The basic stream type is 'Stream', it represents a sequence of IO actions,
-- and is a 'Monad'.  The type 'StreamT' is a monad transformer that can
-- represent a sequence of actions in an arbitrary monad. The type 'Stream' is
-- in fact a synonym for @StreamT IO@.  There are a few more types similar to
-- 'StreamT', all of them represent a stream and differ only in the
-- 'Semigroup', 'Applicative' and 'Monad' compositions of the stream. 'Stream'
-- and 'Costream' types compose serially whereas 'ParAhead' and 'CoparAhead'
-- types compose concurrently. All these types can be freely inter-converted
-- using type combinators without any cost. You can freely switch to any type
-- of composition at any point in the program.  When no type annotation or
-- explicit stream type combinators are used, the default stream type is
-- inferred as 'Stream'.
--
-- Here is a simple console echo program example:
--
-- @
-- > runStream $ S.repeatM getLine & S.mapM putStrLn
-- @
--
-- For more details please see the "Streamly.Tutorial" module and the examples
-- directory in this package.
--
-- This module exports stream types, instances and some basic operations.
-- Functionality exported by this module include:
--
-- * Semigroup append ('<>') instances as well as explicit  operations for merging streams
-- * Monad and Applicative instances for looping over streams
-- * Zip Applicatives for zipping streams
-- * Stream type combinators to convert between different composition styles
-- * Some basic utilities to run and fold streams
--
-- See the "Streamly.Prelude" module for comprehensive APIs for construction,
-- generation, elimination and transformation of streams.
--
-- This module is designed to be imported unqualified:
--
-- @
-- import Streamly
-- @

module Streamly
    (
      MonadParallel

    -- * Stream transformers
    -- ** Serial Streams
    -- $serial
    , StreamT
    , CostreamT

    -- ** Parallel Streams
    -- $parAhead
    , ParAheadT
    , CoparAheadT
    , ParallelT

    -- ** Zipping Streams
    -- $zipping
    , ZipStreamM
    , ZipParallelM

    -- * Polymorphic Sum Operations
    -- $sum
    , splice
    , cosplice
    , parAhead
    , coparAhead
    , parallel

    -- * Stream Type Adapters
    -- $adapters
    , IsStream

    , asStream
    , asCostream
    , asParAhead
    , asCoparAhead
    , asParallel
    , asZipStream
    , asZipParallel
    , adapt

    -- * IO Streams
    , Stream
    , Costream
    , ParAhead
    , CoparAhead
    , Parallel
    , ZipStream
    , ZipParallel

    -- * Running Streams
    , runStream

    -- * Transformation
    , async

    -- * Polymorphic Fold Utilities
    -- $foldutils
    , foldWith
    , foldMapWith
    , forEachWith

    -- * Re-exports
    , Semigroup (..)
    -- * Deprecated
    , MonadAsync
    , Streaming
    , runStreaming
    , runStreamT
    , runInterleavedT
    , runParallelT
    , runZipAsync
    , runAsyncT
    , runZipStream
    , InterleavedT
    , AsyncT
    , ZipAsync
    , serially
    , interleaving
    , asyncly
    , parallely
    , zipping
    , zippingAsync
    , (<=>)
    , (<|)
    )
where

import Streamly.Streams
import Data.Semigroup (Semigroup(..))

-- $serial
--
-- Serial streams compose serially or non-concurrently. In a composed stream,
-- at any point of time only one constituent stream runs and yields an element.
-- The two serial stream types 'StreamT' and 'CostreamT' differ in how they
-- merge streams together in a 'Semigroup' or 'Monad' composition. As these
-- streams are serial, the sequence of items in a composed stream can be solely
-- determined by the position of elements in the consituent streams.

-- $parAhead
--
-- Parallel ahead streams compose in an ahead parallel manner. In a composed stream, at
-- any point of time more than one stream can run concurrently and yield
-- elements. The two parallel ahead types 'ParAheadT' and 'coParAheadT' differ in how
-- they merge streams together in 'Semigroup' or 'Monad' compositions. As
-- these streams compose concurrently, the sequence of items in a composed
-- stream cannot be determined by the position of elements in the constituent
-- streams.  The elements are yielded by the composed stream as they are
-- generated by the constituent streams on a first come first serve basis.
-- Therefore, on each run the stream may yield elements in a different sequence
-- depending on the delays introduced by scheduling.

-- $zipping
--
-- 'ZipStreamM' and 'ZipParallelM', provide 'Applicative' instances for zipping the
-- corresponding elements of two streams together. Note that these types are
-- not monads.

-- $sum
-- The 'Semigroup' operation '<>' of each stream type combines two streams in a
-- type specific manner. This section provides polymorphic versions of '<>'
-- which can be used to combine two streams in a predetermined way irrespective
-- of the type.

-- $adapters
--
-- You may want to use different stream composition styles at different points
-- in your program. Stream types can be freely converted or adapted from one
-- type to another.  The 'IsStream' type class facilitates type conversion of
-- one stream type to another. It is not used directly, instead the type
-- combinators provided below are used for conversions.
--
-- To adapt from one monomorphic type (e.g. 'parAheadT') to another monomorphic
-- type (e.g. 'StreamT') use the 'adapt' combinator. To give a polymorphic code
-- a specific interpretation or to adapt a specific type to a polymorphic type
-- use the type specific combinators e.g. 'asParAhead' or 'asCostream'. You
-- cannot adapt polymorphic code to polymorphic code, as it would not know
-- which specific type you are converting from or to. If you see a an
-- @ambiguous type variable@ error then most likely you are using 'adapt'
-- unnecessarily on polymorphic code.
--

-- $foldutils
--
-- These are variants of standard 'Foldable' fold functions that use a
-- polymorphic stream sum operation (e.g. 'parAhead' or 'cosplice') to fold a
-- container of streams.
