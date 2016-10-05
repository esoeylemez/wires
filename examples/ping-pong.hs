-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>
-- Stability:  experimental

{-# LANGUAGE Arrows #-}

module Main (main) where

import Control.Monad.IO.Class
import Control.Wire
import Control.Wire.Controller
import GHC.Stats
import Prelude hiding ((.), id)
import System.Clock
import System.IO
import System.Mem
import Text.Printf
import Utils


newCharEvent :: (MonadIO m) => Wire m a (Event Char)
newCharEvent = proc _ ->
    newEvent -< liftIO $ do
        b <- hReady stdin
        if b then Just <$> getChar else pure Nothing


newTickEvent :: (MonadIO m) => Wire m a (Event Double)
newTickEvent = proc _ -> do
    times <- newEvent -< Just <$> getT
    withM_ unfoldE getT -< fmap (\t t' -> (secs (t - t'), t)) times

    where
    secs = (/ 1000000000) . fromInteger . toNanoSecs
    getT = liftIO (getTime Monotonic)

menu :: (MonadIO m) => Wire m a (Event (), Event (Wire m a (Event ())))
menu = proc _ -> do
    initial -< liftIO $ putStrLn "Welcome! Press Enter to begin!"
    chars <- newCharEvent -< ()
    id -< (\e -> (never, e)) $ myApp <$ filterE (== '\n') chars

myApp :: (MonadIO m) => Wire m a (Event ())
myApp = proc _ -> do
    initial -< liftIO $ putStrLn "Let's do this!"
    deltas <- newTickEvent -< ()
    fps <- hold 0 . (fmap recip <$> average 25) -< deltas
    chars <- newCharEvent -< ()

    acc <- scan 1 -< negate <$ filterE (== ' ') chars
    vel <- scan 0 -< fmap (\dt -> min 2 . max (-2) . (+ acc*dt)) deltas
    pos <- scan 0 -< fmap (\dt -> min 1 . max (-1) . (+ vel*dt)) deltas

    animate -< liftIO $ do
        performGC
        mem <- (`div` 1024) . currentBytesUsed <$> getGCStats
        printf "\r%8dk %8.2f %5.2f %s %s\027[K"
            mem
            fps
            vel
            (if acc > 0 then ">" else "<")
            (map (\xI ->
                      let x = -1 + 2 * fromInteger xI / 59
                      in if abs (pos - x) < 0.1 then 'X' else '.')
                 [0..59])
        hFlush stdout

    onEvent -< liftIO (putStrLn "") <$ filterE (== '\n') chars

    id -< () <$ filterE (== 'q') chars


main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    control . switch $ menu
