-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
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
      scanE
    )
    where

import Control.Category
import Control.Wire.Core
import Data.Profunctor
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


-- | Run the given action to initialise the given wire.  Simplified
-- variant of 'withM'.  Example:
--
-- > withM_ (scan f) action

withM_ :: (Monad m) => (s -> Wire m a b) -> m s -> Wire m a b
withM_ w c = withM w (const c)
