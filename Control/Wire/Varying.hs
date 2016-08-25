-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>
-- Stability:  experimental

{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module Control.Wire.Varying
    ( -- * Time-varying values
      Varying,
      changes,
      value,

      -- * Events
      holdV,
      holdV',
      scanV,

      -- * Controllers
      animateV
    )
    where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Wire.Internal
import Data.String
import Numeric


-- | A 'Varying' is a behaviour combined with an event that tracks when
-- the value changes.

data Varying a = Varying !Bool !a
    deriving (Foldable, Functor)

instance Applicative Varying where
    pure = Varying False
    (<*>) = ap

instance (Bounded a) => Bounded (Varying a) where
    minBound = pure minBound
    maxBound = pure maxBound

instance (NFData a) => NFData (Varying a) where
    rnf (Varying _ x) = rnf x

instance (Floating a) => Floating (Varying a) where
    (**) = liftA2 (**)
    pi = pure pi
    exp = fmap exp
    log = fmap log
    sqrt = fmap sqrt
    logBase = liftA2 logBase

    sin = fmap sin; asin = fmap asin; sinh = fmap sinh; asinh = fmap asinh
    cos = fmap cos; acos = fmap acos; cosh = fmap cosh; acosh = fmap acosh
    tan = fmap tan; atan = fmap atan; tanh = fmap tanh; atanh = fmap atanh

    log1p = fmap log1p
    expm1 = fmap expm1
    log1pexp = fmap log1pexp
    log1mexp = fmap log1mexp

instance (Fractional a) => Fractional (Varying a) where
    (/) = liftA2 (/)
    fromRational = pure . fromRational
    recip = fmap recip

instance (IsString a) => IsString (Varying a) where
    fromString = pure . fromString

instance Monad Varying where
    Varying cx x >>= f =
        let Varying cy y = f x
        in Varying (cx || cy) y

instance (Num a) => Num (Varying a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    fromInteger = pure . fromInteger
    negate = fmap negate
    signum = fmap signum


-- | Run the given action each time the given time-varying value
-- changes.

animateV :: (Applicative m) => (a -> m b) -> Wire m (Varying a) (Varying b)
animateV f =
    Wire $ \(Varying cx x) -> do
        (\y -> (Varying cx y, go y))
        <$> f x

    where
    go y' =
        Wire $ \(Varying cx x) ->
            if cx
              then (\y -> (Varying True y, go y)) <$> f x
              else pure (Varying False y', go y')


-- | The change event of the given time-varying value.

changes :: Varying a -> Event a
changes (Varying cx x) = if cx then Now x else NotNow


-- | Hold the latest occurrence of the given event starting with the
-- given initial value.  The value switch occurs in the next frame.

holdV :: (Applicative m) => a -> Wire m (Event a) (Varying a)
holdV = go . Varying False
    where
    go x' =
        Wire $ \mx ->
            pure (x', go (event (Varying False (value x')) (Varying True) mx))


-- | Hold the latest occurrence of the given event starting with the
-- given initial value.  The value switch occurs instantly.

holdV' :: (Applicative m) => a -> Wire m (Event a) (Varying a)
holdV' x' =
    Wire $ \mx ->
        pure $
        case mx of
          NotNow -> (Varying False x', holdV' x')
          Now x  -> (Varying True x, holdV' x)


-- | Left scan and hold of the given event.

scanV :: (Applicative m) => (a -> b -> b) -> b -> Wire m (Event a) (Varying b)
scanV f = go . Varying False
    where
    go x' =
        Wire $ \mdx ->
            pure (x', go (event (Varying False) (\dx -> Varying True . f dx) mdx (value x')))


-- | The behaviour value of the given time-varying value.

value :: Varying a -> a
value (Varying _ x) = x
