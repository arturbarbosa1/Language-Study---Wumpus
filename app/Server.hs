{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent
import Control.Monad (void, forever)

import Data.String.Conversions
import Data.Monoid ((<>))

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (send, recv)

import Wumpus.Actions
import Wumpus.Grid
import Wumpus.Utils

main :: IO ()
main = do
  listener <- socket AF_INET Stream 0
  setSocketOption listener ReuseAddr 1
  bind listener (SockAddrInet 4242 iNADDR_ANY)
  listen listener 2
  startGame listener

startGame :: Socket -> IO ()
startGame sock = do
  game <- generateGame
  state <- newMVar game
  ready <- newEmptyMVar

  connPlayer <- accept sock
  threadIdPlayer <- forkIO $ runPlayer connPlayer state ready
  putStrLn $ "Player's thread: " <> show threadIdPlayer

  connWumpus <- accept sock
  threadIdWumpus <- forkIO $ runWumpus connWumpus state ready
  putStrLn $ "Wumpus's thread: " <> show threadIdWumpus

  void $ takeMVar ready
  closeSocks connPlayer connWumpus
    where
      closeSocks (sock1, _) (sock2, _) = close sock1 >> close sock2

loopPlayer :: Socket -> MVar Game -> IO ()
loopPlayer sock gameStore = forever $ do
  game@Game{..} <- takeMVar gameStore
  threadDelay 100
  if wumpusWins
  then do
    void $ send sock $ "You lose!\n"
    putMVar gameStore game
  else if playerWins
  then do
    void $ send sock $ "You win!\n"
    putMVar gameStore game
  else do
    putStrLn "Player's turn"
    void $ send sock $ "I'm in room " <> cs (show player) <> "\n"
    void $ send sock $ "I can go to rooms " <> pp (grid !! (player - 1)) <> "\n"
    void $ send sock $ "I have " <> cs (show arrows) <> " arrows left.\n"
    checkWumpus game sock
    checkBats game sock
    checkPits game sock
    void $ send sock $ "All wrong actions are completely under my care.\n\n"
    void $ send sock $ "> "
    cmdRaw <- recv sock 1024
    let cmd = parsePlayerCmd cmdRaw
    newGame <- execPlayerCmd cmd game
    putMVar gameStore newGame

runPlayer :: (Socket, SockAddr) -> MVar Game -> MVar Bool -> IO ()
runPlayer (sock, _) game ready = do
  void $ send sock "\nHello, player!\n\n"
  _ <- takeMVar ready
  void $ send sock "Game is started. Stay away from wumpus and try to hit him.\n"
  void $ send sock "You can move to neighbour room or shoot.\nIf you shoot you must name all rooms you expect to hit.\nYou have five arrows.\n"
  void $ send sock "Also you can stupidly sleep.\nExpect some dangers.\n\n"
  loopPlayer sock game
  _ <- putMVar ready True
  return ()

loopWumpus :: Socket -> MVar Game -> IO ()
loopWumpus sock gameStore = forever $ do
  game@Game{..} <- takeMVar gameStore
  threadDelay 100
  if wumpusWins
  then do
    void $ send sock $ "You win!\n"
    putMVar gameStore game
  else if playerWins
  then do
    void $ send sock $ "You lose!\n"
    putMVar gameStore game
  else do
    putStrLn "Wumpus's turn"
    void $ send sock $ "I'm in room " <> cs (show (wumpus)) <> "\n"
    void $ send sock $ "I can go to rooms " <> pp (grid !! (wumpus - 1)) <> "\n"
    checkPlayer game sock
    checkArrow game sock
    void $ send sock $ "All wrong actions are completely under my care.\n\n"
    void $ send sock $ "> "
    cmdRaw <- recv sock 1024
    let cmd = parseWumpusCmd cmdRaw
    putMVar gameStore (execWumpusCmd cmd game)

runWumpus :: (Socket, SockAddr) -> MVar Game -> MVar Bool -> IO ()
runWumpus (sock, _) game ready = do
  void $ send sock "\nHello, Wumpus!\n\n"
  putMVar ready True
  void $ send sock "Game is started.\n"
  void $ send sock "You can sleep.\nOr move to closest room.\n\n"
  threadDelay 1000
  loopWumpus sock game
  _ <- takeMVar ready
  _ <- putMVar ready True
  return ()
