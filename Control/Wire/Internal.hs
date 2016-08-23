-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>
-- Stability:  experimental

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}

module Control.Wire.Internal
    ( -- * Wires
      Wire(..),

      -- * Events
      Event(..),
      event
    )
    where

import Control.Arrow
import Control.Category
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

instance Plus Event where
    zero = nil


-- | 'Wire' is a language for defining reactive systems.  It is similar
-- to the underlying monad @m@, but runs continuously.

newtype Wire m a b =
    Wire {
      stepWire :: a -> m (b, Wire m a b)
    }
    deriving (Functor)

instance (Applicative m) => Applicative (Wire m a) where
    pure x = let w = Wire (\_ -> pure (x, w)) in w

    wf' <*> wx' =
        Wire $ \x' -> do
            (f, wf) <- stepWire wf' x'
            (x, wx) <- stepWire wx' x'
            pure (f x, wf <*> wx)

instance (Monad m) => Arrow (Wire m) where
    arr f = let w = Wire (\x -> pure (f x, w)) in w
    first = first'
    second = second'

    wx' &&& wy' =
        Wire $ \x' -> do
            (x, wx) <- stepWire wx' x'
            (y, wy) <- stepWire wy' x'
            pure ((x, y), wx &&& wy)

    wx' *** wy' =
        Wire $ \(x', y') -> do
            (x, wx) <- stepWire wx' x'
            (y, wy) <- stepWire wy' y'
            pure ((x, y), wx *** wy)

instance (Monad m) => ArrowChoice (Wire m) where
    left = left'
    right = right'

    wl' +++ wr' =
        Wire $
        either (\x -> do (y, wl) <- stepWire wl' x; pure (Left y, wl +++ wr'))
               (\x -> do (y, wr) <- stepWire wr' x; pure (Right y, wl' +++ wr))

    wl' ||| wr' =
        Wire $
        either (\x -> do (y, wl) <- stepWire wl' x; pure (y, wl ||| wr'))
               (\x -> do (y, wr) <- stepWire wr' x; pure (y, wl' ||| wr))

instance (MonadFix m) => ArrowLoop (Wire m) where
    loop = unfirst

instance (Monad m) => Category (Wire m) where
    id = let w = Wire (\x -> pure (x, w)) in w

    w2' . w1' =
        Wire $ \x0 -> do
            (x1, w1) <- stepWire w1' x0
            (x2, w2) <- stepWire w2' x1
            pure (x2, w2 . w1)

instance (Applicative m) => Choice (Wire m) where
    left' w' =
        Wire $
        either (\x -> do (y, w) <- stepWire w' x; pure (Left y, left' w))
               (\x -> pure (Right x, left' w'))

    right' w' =
        Wire $
        either (\x -> pure (Left x, right' w'))
               (\x -> do (y, w) <- stepWire w' x; pure (Right y, right' w))

instance (MonadFix m) => Costrong (Wire m) where
    unfirst w' =
        Wire $ \x' ->
            (\((x, _), w) -> (x, unfirst w))
            <$> mfix (\r -> stepWire w' (x', snd (fst r)))

    unsecond w' =
        Wire $ \x' ->
            (\((_, x), w) -> (x, unsecond w))
            <$> mfix (\r -> stepWire w' (fst (fst r), x'))

instance (Functor m) => Profunctor (Wire m) where
    dimap fl fr = go
        where
        go w' = Wire (fmap (\(y, w) -> (fr y, go w)) . stepWire w' . fl)

    lmap f = go
        where
        go w' = Wire (fmap (\(y, w) -> (y, go w)) . stepWire w' . f)

    rmap = fmap

instance (Functor m) => Strong (Wire m) where
    first' w' =
        Wire $ \(x', y) -> do
            (x, w) <- stepWire w' x'
            pure ((x, y), first' w)

    second' w' =
        Wire $ \(x, y') -> do
            (y, w) <- stepWire w' y'
            pure ((x, y), second' w)


-- | Fold the given event.

event
    :: r         -- ^ Not now.
    -> (a -> r)  -- ^ Now.
    -> Event a   -- ^ Event to fold.
    -> r
event k _ NotNow  = k
event _ k (Now x) = k x
