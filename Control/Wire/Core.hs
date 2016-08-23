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
      withM,

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

      -- * Controllers
      animate,
      control,
      newEvent,
      onEvent,
      stepWire
    )
    where

import Control.Wire.Internal


-- | Run the given action in every frame.

animate :: (Applicative m) => (a -> m b) -> Wire m a b
animate f = let w = Wire (fmap (, w) . f) in w


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
evalWith strat = animate (\x -> x `strat` pure x)


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

initial :: (Applicative m) => (a -> m b) -> Wire m a b
initial f =
    Wire $ \x -> do
        y <- f x
        pure (y, pure y)


-- | The event that never occurs.

never :: Event a
never = NotNow


-- | Construct an event from the given polling function.

newEvent :: (Functor m) => (a -> m (Maybe b)) -> Wire m a (Event b)
newEvent f = go
    where
    go =
        Wire $ \x -> do
            mx <- f x
            pure (maybe NotNow Now mx, go)


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


-- | Run the given action to initialise the given wire.  Example:
--
-- > withM (scan f) action

withM :: (Monad m) => (s -> Wire m a b) -> (a -> m s) -> Wire m a b
withM w f =
    Wire $ \x -> do
        s0 <- f x
        stepWire (w s0) x
