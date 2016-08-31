-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>
-- Stability:  experimental

module Utils
    ( -- * Utilities
      average
    )
    where

import Control.Wire
import Data.Foldable
import Data.Profunctor
import qualified Data.Sequence as Seq


-- | Average of the given event's payload over the last given number of
-- occurrences.

average :: (Fractional a, Monad m) => Int -> Wire m (Event a) (Event a)
average n = lmap (fmap go) (unfoldE Seq.empty)
    where
    go x xs' =
        let xs = Seq.take n (x Seq.<| xs')
        in (foldl' (+) 0 xs / fromIntegral (Seq.length xs),
            xs)
