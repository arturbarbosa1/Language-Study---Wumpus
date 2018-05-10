{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Wumpus.Utils where

import Control.Monad (void, when)

import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.String.Conversions

import Network.Socket (Socket)
import Network.Socket.ByteString (send)

import Wumpus.Grid

checkWumpus :: Game -> Socket -> IO ()
checkWumpus Game{..} sock = do
  when ((grid !! (player - 1)) `interact1` wumpus) $ void $ send sock $ "I smell a wumpus\n"
  where
    interact1 (Room a b c) d = a == d || b == d || c == d

checkArrow :: Game -> Socket -> IO ()
checkArrow Game{..} sock = when arrowSemaphore $ void $ send sock $ "Shoot?\n"

checkPlayer :: Game -> Socket -> IO ()
checkPlayer Game{..} sock = do
  when ((grid !! (wumpus - 1)) `interact1` player) $ void $ send sock $ "I smell a player\n"
  where
    interact1 (Room a b c) d = a == d || b == d || c == d

checkBats :: Game -> Socket -> IO ()
checkBats Game{..} sock = do
  when ((grid !! (player - 1)) `interact1` bats) $ void $ send sock $ "Bats nearby\n"
  where
    interact1 (Room a b c) (Bat d e) = a == d || b == d || c == d || a == e || b == e || c == e

checkPits :: Game -> Socket -> IO ()
checkPits Game{..} sock = do
  when ((grid !! (player - 1)) `interact1` pits) $ void $ send sock $ "I feel a draft\n"
  where
    interact1 (Room a b c) (Pit d e) = a == d || b == d || c == d || a == e || b == e || c == e

closeEnough :: Int -> Room -> Bool
closeEnough r (Room a b c) = r == a || r == b || r == c

pp :: Room -> ByteString
pp (Room a b c) = cs $ show a <> ", " <> show b <> ", " <> show c
