-- |
-- Copyright:  (c) 2017 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>
-- Stability:  experimental

module Control.Wire.Utils
    ( -- * Wire utilities
      withM_,

      -- * Event utilities
      filterE,
      scan,
      scan',
      scanE,
      splitE,
      unalignE,
      unlessE
    )
    where

import Control.Category
import Control.Wire.Core
import Control.Wire.Internal
import Data.Align
import Data.Profunctor
import Data.These
import Prelude hiding ((.), id)


-- | Filter event occurrences using the given function.

filterE :: (a -> Bool) -> Event a -> Event a
filterE p = catMapE (\x -> if p x then Just x else Nothing)


-- | Left scan and hold of the given event.

scan :: (Monad m) => a -> Wire m (Event (a -> a)) a
scan x0 = hold x0 . scanE x0


-- | Left scan and hold of the given event.  The value switch occurs
-- instantly.

scan' :: (Monad m) => a -> Wire m (Event (a -> a)) a
scan' x0 = hold' x0 . scanE x0


-- | Left scan of the given event.

scanE :: (Applicative m) => a -> Wire m (Event (a -> a)) (Event a)
scanE = lmap (fmap $ \f x -> let y = f x in (y, y)) . unfoldE


-- | Split the given event

splitE :: Event (Either a b) -> (Event a, Event b)
splitE NotNow          = (NotNow, NotNow)
splitE (Now (Left x))  = (Now x,  NotNow)
splitE (Now (Right y)) = (NotNow, Now y)


-- | Split the given event
--
-- Inverse of 'align'.

unalignE :: Event (These a b) -> (Event a, Event b)
unalignE NotNow            = (NotNow, NotNow)
unalignE (Now (This x))    = (Now x,  NotNow)
unalignE (Now (That y))    = (NotNow, Now y)
unalignE (Now (These x y)) = (Now x,  Now y)


-- | Event difference: like the left event, but only when the right
-- event doesn't occur at the same time.

unlessE :: Event a -> Event b -> Event a
unlessE mx my = catMapE justThis (align mx my)


-- | Run the given action to initialise the given wire.  Simplified
-- variant of 'withM'.  Example:
--
-- > withM_ (scan f) action

withM_ :: (Monad m) => (s -> Wire m a b) -> m s -> Wire m a b
withM_ w c = withM w (const c)
