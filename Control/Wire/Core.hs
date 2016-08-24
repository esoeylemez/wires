-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>
-- Stability:  experimental

{-# LANGUAGE ApplicativeDo #-}
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
      unfoldE,

      -- * Switching
      manage,
      manage',
      sequenceW,
      switch,
      switch'
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


-- | Sequence each of the given wires and collect their results.  If the
-- given event occurs, the its function is applied to the current set of
-- wires.  Changes are applied in the next frame.

manage
    :: (Traversable f, Applicative m)
    => f (Wire m a b)
    -> Wire m (a, Event (f (Wire m a b) -> f (Wire m a b))) (f b)
manage ws' =
    Wire $ \(x, mf) -> do
        ys <- traverse (`stepWire` x) ws'
        pure (fst <$> ys,
              manage (event id id mf (snd <$> ys)))


-- | Sequence each of the given wires and collect their results.  If the
-- given event occurs, the its function is applied to the current set of
-- wires.  Changes are applied immediately.

manage'
    :: (Traversable f, Applicative m)
    => f (Wire m a b)
    -> Wire m (a, Event (f (Wire m a b) -> f (Wire m a b))) (f b)
manage' ws' =
    Wire $ \(x, mf) -> do
        ys <- traverse (`stepWire` x) (event id id mf ws')
        pure (fst <$> ys, manage (snd <$> ys))


-- | The event that never occurs.

never :: Event a
never = NotNow


-- | Sequence each of the given wires and collect their results.

sequenceW :: (Traversable f, Applicative m) => f (Wire m a b) -> Wire m a (f b)
sequenceW ws' =
    Wire $ \x -> do
        ys <- traverse (\w' -> stepWire w' x) ws'
        pure (fst <$> ys, sequenceW (snd <$> ys))


-- | Acts like the given wire until its event occurs, then switches to
-- the wire the occurrence contained.  The switch occurs in the next
-- frame.

switch :: (Functor m) => Wire m a (b, Event (Wire m a b)) -> Wire m a b
switch w' =
    Wire $ \x -> do
        ((y, mw), w) <- stepWire w' x
        pure (y, event (switch w) id mw)


-- | Acts like the given wire until its event occurs, then switches to
-- the wire the occurrence contained.  The switch occurs immediately.

switch' :: (Monad m) => Wire m a (b, Event (Wire m a b)) -> Wire m a b
switch' w' =
    Wire $ \x -> do
        ((y, mw), w) <- stepWire w' x
        case mw of
          NotNow -> pure (y, switch w)
          Now nw -> stepWire nw x


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
