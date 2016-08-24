-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>
-- Stability:  experimental

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Control.Wire.Core
    ( -- * Wires
      Wire,
      evalWith,
      initial,

      -- * Events
      Event,
      catMapE,
      filterE,
      hold,
      hold',
      never,
      scan,
      scan',
      scanE,
      unfoldE,

      -- * Controllers
      animate,
      control,
      newEvent,
      onEvent,
      stepWire,
      withM,
      withM_
    )
    where

import Control.Wire.Internal
import Data.Profunctor


-- | Run the given action in every frame.

animate :: (Applicative m) => Wire m (m a) a
animate = let w = Wire (fmap (, w)) in w


-- | Map and filter event occurrences using the given function.

catMapE :: (a -> Maybe b) -> Event a -> Event b
catMapE f = event NotNow (maybe NotNow Now . f)


-- | Run the given wire until its result event occurs.

control :: (Monad m) => Wire m () (Event b) -> m b
control w' = do
    (mx, w) <- stepWire w' ()
    event (control w) pure mx


-- | Evaluate the input using the given strategy in every frame.  Valid
-- arguments include functions like `seq`.

evalWith :: (Applicative m) => (forall b. a -> b -> b) -> Wire m a a
evalWith strat = lmap (\x -> x `strat` pure x) animate


-- | Filter event occurrences using the given function.

filterE :: (a -> Bool) -> Event a -> Event a
filterE p ev@(Now x) | p x = ev
filterE _ _ = NotNow


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
initial =
    Wire $ \c -> do
        y <- c
        pure (y, pure y)


-- | The event that never occurs.

never :: Event a
never = NotNow


-- | Construct an event from the given polling function.

newEvent :: (Functor m) => Wire m (m (Maybe b)) (Event b)
newEvent =
    Wire $ \c -> do
        mx <- c
        pure (maybe NotNow Now mx, newEvent)


-- | Run the given action whenever the given event occurs.

onEvent :: (Applicative m) => Wire m (Event (m a)) (Event a)
onEvent =
    Wire $
    event (pure (NotNow, onEvent))
          (\c -> do x <- c; pure (Now x, onEvent))


-- | Left scan and hold of the given event.

scan :: (Applicative m) => (a -> b -> b) -> b -> Wire m (Event a) b
scan f z = delayW z (scan' f z)


-- | Left scan and hold of the given event.  Value switches occur
-- instantly.

scan' :: (Applicative m) => (a -> b -> b) -> b -> Wire m (Event a) b
scan' f = go
    where
    go x' =
        Wire $ \mdx ->
            let x = event x' (`f` x') mdx
            in pure (x, go x)


-- | Left scan of the given event.

scanE :: (Applicative m) => (a -> b -> b) -> b -> Wire m (Event a) (Event b)
scanE f = go
    where
    go x' =
        Wire $ \mdx ->
            pure (case mdx of
                    NotNow -> (NotNow, go x')
                    Now dx -> let x = f dx x' in (Now x, go x))


-- | Unfold the given event.

unfoldE :: (Applicative m) => (a -> s -> (b, s)) -> s -> Wire m (Event a) (Event b)
unfoldE f = go
    where
    go s' =
        Wire $
        pure .
        event (NotNow, go s')
              (\ds -> let (x, s) = f ds s' in (Now x, go s))


-- | Run the given action to initialise the given wire.  Example:
--
-- > withM (scan f) actionFromInitialInput

withM :: (Monad m) => (s -> Wire m a b) -> (a -> m s) -> Wire m a b
withM w f =
    Wire $ \x -> do
        s0 <- f x
        stepWire (w s0) x


-- | Run the given action to initialise the given wire.  Simplified
-- variant of 'withM'.  Example:
--
-- > withM_ (scan f) action

withM_ :: (Monad m) => (s -> Wire m a b) -> m s -> Wire m a b
withM_ w c =
    Wire $ \x -> do
        s0 <- c
        stepWire (w s0) x
