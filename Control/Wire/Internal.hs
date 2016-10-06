-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>
-- Stability:  experimental

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}

module Control.Wire.Internal
    ( -- * Wires
      Wire(..),
      delayW,

      -- * Events
      Event(..),
      event
    )
    where

import Control.Arrow
import Control.Category
import Control.DeepSeq
import Control.Monad.Fix
import Data.Align
import Data.Functor.Bind
import Data.Functor.Extend
import Data.Functor.Plus
import Data.Profunctor
import Data.These
import Prelude hiding ((.), id)


-- | An event is a timestamped stream of occurrences with payloads of
-- the given type.

data Event a
    = NotNow  -- ^ Not in this frame.
    | Now a   -- ^ In this frame with the given value.
    deriving (Functor)

instance Align Event where
    nil = NotNow

    alignWith f (Now x) (Now y) = Now (f (These x y))
    alignWith f (Now x) _       = Now (f (This x))
    alignWith f _       (Now y) = Now (f (That y))
    alignWith _ _       _       = NotNow

instance Alt Event where
    (<!>) = alignWith (mergeThese const)

instance Apply Event where
    Now f  <.> Now x  = Now (f x)
    _      <.> _      = NotNow

instance Bind Event where
    Now x  >>- f = event NotNow Now (f x)
    NotNow >>- _ = NotNow

    join (Now (Now x)) = Now x
    join _             = NotNow

instance Extend Event where
    duplicated = event NotNow (Now . Now)
    extended f = event NotNow (Now . f . Now)

instance (Monoid a) => Monoid (Event a) where
    mappend = alignWith (mergeThese mappend)
    mempty  = nil

instance (NFData a) => NFData (Event a) where
    rnf NotNow = ()
    rnf (Now x) = rnf x

instance Plus Event where
    zero = nil


-- | 'Wire' is a language for defining reactive systems.  It is similar
-- to the underlying monad @m@, but runs continuously.

newtype Wire m a b =
    Wire {
      -- | Run a single frame of the given wire.
      stepWire :: a -> m (b, Wire m a b)
    }

instance (Applicative m) => Applicative (Wire m a) where
    pure x = let w = Wire (\_ -> pure (x, w)) in w

    wf' <*> wx' =
        Wire $ \x' ->
            (\(f, wf) (x, wx) -> (f x, wf <*> wx))
            <$> stepWire wf' x'
            <*> stepWire wx' x'

instance (Monad m) => Arrow (Wire m) where
    arr f = let w = Wire (\x -> pure (f x, w)) in w
    first = first'
    second = second'

    wx' &&& wy' =
        Wire $ \x' ->
            (\(x, wx) (y, wy) -> ((x, y), wx &&& wy))
            <$> stepWire wx' x'
            <*> stepWire wy' x'

    wx' *** wy' =
        Wire $ \(x', y') ->
            (\(x, wx) (y, wy) -> ((x, y), wx *** wy))
            <$> stepWire wx' x'
            <*> stepWire wy' y'

instance (Monad m) => ArrowChoice (Wire m) where
    left = left'
    right = right'

    wl' +++ wr' =
        Wire $
        either (\x -> (\(y, wl) -> (Left y,  wl +++ wr')) <$> stepWire wl' x)
               (\x -> (\(y, wr) -> (Right y, wl' +++ wr)) <$> stepWire wr' x)

    wl' ||| wr' =
        Wire $
        either (\x -> (\(y, wl) -> (y, wl ||| wr')) <$> stepWire wl' x)
               (\x -> (\(y, wr) -> (y, wl' ||| wr)) <$> stepWire wr' x)

instance (MonadFix m) => ArrowLoop (Wire m) where
    loop = unfirst

instance (Monad m) => Category (Wire m) where
    id = let w = Wire (\x -> pure (x, w)) in w

    w2' . w1' =
        Wire $ \x0 -> do
            !(x1, w1) <- stepWire w1' x0
            !(x2, w2) <- stepWire w2' x1
            pure (x2, w2 . w1)

instance (Applicative m) => Choice (Wire m) where
    left' w' =
        Wire $
        either (\x -> (\(y, w) -> (Left y, left' w)) <$> stepWire w' x)
               (\x -> pure (Right x, left' w'))

    right' w' =
        Wire $
        either (\x -> pure (Left x, right' w'))
               (\x -> (\(y, w) -> (Right y, right' w)) <$> stepWire w' x)

instance (MonadFix m) => Costrong (Wire m) where
    unfirst w' =
        Wire $ \x' ->
            (\((x, _), w) -> (x, unfirst w))
            <$> mfix (\r -> stepWire w' (x', snd (fst r)))

    unsecond w' =
        Wire $ \x' ->
            (\((_, x), w) -> (x, unsecond w))
            <$> mfix (\r -> stepWire w' (fst (fst r), x'))

instance (Functor m) => Functor (Wire m a) where
    fmap = rmap

instance (Functor m) => Profunctor (Wire m) where
    dimap fl fr = go
        where
        go w' = Wire (fmap (\(y, w) -> (fr y, go w)) . stepWire w' . fl)

    lmap f = go
        where
        go w' = Wire (fmap (\(y, w) -> (y, go w)) . stepWire w' . f)

    rmap f = go
        where
        go w' = Wire $ fmap (\(y, w) -> (f y, go w)) . stepWire w'

instance (Functor m) => Strong (Wire m) where
    first' w' =
        Wire $ \(x', y) ->
            (\(x, w) -> ((x, y), first' w))
            <$> stepWire w' x'

    second' w' =
        Wire $ \(x, y') ->
            (\(y, w) -> ((x, y), second' w))
            <$> stepWire w' y'


-- | Delay the result of the given wire by one frame.

delayW :: (Functor m) => b -> Wire m a b -> Wire m a b
delayW y' w' =
    Wire $
        fmap (\(y, w) -> (y', delayW y w)) .
        stepWire w'


-- | Fold the given event.

event
    :: r         -- ^ Not now.
    -> (a -> r)  -- ^ Now.
    -> Event a   -- ^ Event to fold.
    -> r
event k _ NotNow  = k
event _ k (Now x) = k x
