-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>
-- Stability:  experimental

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}

module Control.Wire.Internal
    ( -- * Wires
      Wire(..)
    )
    where

import Control.Arrow
import Control.Category
import Control.Monad.Fix
import Data.Profunctor
import Prelude hiding ((.), id)


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

instance (Monad m) => Category (Wire m) where
    id = let w = Wire (\x -> pure (x, w)) in w

    w2' . w1' =
        Wire $ \x0 -> do
            (x1, w1) <- stepWire w1' x0
            (x2, w2) <- stepWire w2' x1
            pure (x2, w2 . w1)

instance (Applicative m) => Choice (Wire m) where
    left' w' =
        Wire $ \mx ->
            case mx of
              Left x -> do
                  (y, w) <- stepWire w' x
                  pure (Left y, left' w)
              Right x -> pure (Right x, left' w')

    right' w' =
        Wire $ \mx ->
            case mx of
              Left x -> pure (Left x, right' w')
              Right x -> do
                  (y, w) <- stepWire w' x
                  pure (Right y, right' w)

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
