-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>

module Control.Wire.Trans
    ( -- * Monad transformers
      -- ** Reader
      asksW,
      askW,
      runReaderW
    )
    where

import Control.Monad.Reader
import Control.Wire.Controller
import Control.Wire.Core
import Data.Profunctor


-- | Get the given function applied to the environment value.

asksW :: (MonadReader a m) => Wire m (a -> b) b
asksW = lmap asks animate


-- | Get the environment value.

askW :: (MonadReader b m) => Wire m a b
askW = lmap (\_ -> ask) animate


-- | Embed the given 'ReaderT'-transformed monad.

runReaderW :: (Functor m) => Wire (ReaderT e m) a b -> Wire m (e, a) b
runReaderW = hoistW snd (\(env, _) c -> runReaderT c env)
