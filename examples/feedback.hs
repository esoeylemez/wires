-- |
-- Copyright:  (c) 2017 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>

{-# LANGUAGE Arrows #-}

module Main (main) where

import Control.Concurrent
import Control.Wire
import Control.Wire.Controller


main :: IO ()
main =
    control $ proc _ -> do
        ev <- newEvent -< do
            Just () <$ threadDelay 500000

        rec x <- scan 1 -< x `seq` (+ x) <$ ev

        animate -< never <$ print x
