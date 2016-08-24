-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>
-- Stability:  experimental

{-# LANGUAGE RankNTypes #-}

module Control.Wire.Core
    ( -- * Wires
      Wire,
      evalWith,
      initial,
      withM,

      -- * Events
      Event,
      catMapE,
      hold,
      hold',
      never,
      unfoldE
    )
    where

import Control.Wire.Internal


-- | Map and filter event occurrences using the given function.

catMapE :: (a -> Maybe b) -> Event a -> Event b
catMapE f = event NotNow (maybe NotNow Now . f)


-- | Evaluate the input using the given strategy in every frame.  Valid
-- arguments include functions like `seq`.

evalWith :: (Applicative m) => (forall b. a -> b -> b) -> Wire m a a
evalWith strat = let w = Wire (\x -> x `strat` pure (x, w)) in w


-- | Hold the latest occurrence of the given event starting with the
-- given initial value.  The value switch occurs in the next frame.

hold :: (Applicative m) => a -> Wire m (Event a) a
hold x' = delayW x' (hold' x')


-- | Hold the latest occurrence of the given event starting with the
-- given initial value.  The value switch occurs instantly.

hold' :: (Applicative m) => a -> Wire m (Event a) a
hold' x' = Wire $ (\x -> pure (x, hold' x)) . event x' id


-- | Run the given action once at the beginning.

initial :: (Applicative m) => Wire m (m a) a
initial = Wire $ fmap (\y -> (y, pure y))


-- | The event that never occurs.

never :: Event a
never = NotNow


-- | Unfold the given event using the state transition functions it
-- carries.

unfoldE :: (Applicative m) => s -> Wire m (Event (s -> (a, s))) (Event a)
unfoldE s' =
    Wire $ \mf ->
        pure (case mf of
                NotNow -> (NotNow, unfoldE s')
                Now f  -> let (x, s) = f s' in (Now x, unfoldE s))


-- | Run the given action to initialise the given wire.  Example:
--
-- > withM (scan f) actionFromInitialInput

withM :: (Monad m) => (s -> Wire m a b) -> (a -> m s) -> Wire m a b
withM w f =
    Wire $ \x -> do
        s0 <- f x
        stepWire (w s0) x
