-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>
-- Stability:  experimental

{-# LANGUAGE TupleSections #-}

module Control.Wire.Controller
    ( -- * Controllers
      animate,
      control,
      newEvent,
      onEvent,
      stepWire
    )
    where

import Control.Wire.Internal


-- | Run the given action in every frame.

animate :: (Applicative m) => Wire m (m a) a
animate = let w = Wire (fmap (, w)) in w


-- | Run the given wire until its result event occurs.

control :: (Monad m) => Wire m () (Event b) -> m b
control w' = do
    (mx, w) <- stepWire w' ()
    event (control w) pure mx


-- | Construct an event from the given polling function.

newEvent :: (Functor m) => Wire m (m (Maybe b)) (Event b)
newEvent = Wire $ fmap (\mx -> (maybe NotNow Now mx, newEvent))


-- | Run the given action whenever the given event occurs.

onEvent :: (Applicative m) => Wire m (Event (m a)) (Event a)
onEvent =
    Wire $
    event (pure (NotNow, onEvent))
          (fmap $ \x -> (Now x, onEvent))
