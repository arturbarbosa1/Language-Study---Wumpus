{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (when, forever, void)
import System.Exit (exitSuccess)

import qualified Data.ByteString.Char8 as BSC
import Data.String.Conversions
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString (recv, send)

main :: IO ()
main = withSocketsDo $ do
    putStrLn "Input host:"
    host <- getLine
    addr <- resolve host "4242"
    open addr >>= talk
  where
    resolve host port = do
        let hints = defaultHints { addrSocketType = Stream }
        addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
        return addr
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock $ addrAddress addr
        putStrLn "Connected.\n"
        return sock
    talk sock = do
      forever $ do
        msg <- recv sock 1024
        BSC.putStr msg
        when (("You win!" `BSC.isInfixOf` msg) || ("You lose!" `BSC.isInfixOf` msg)) $ do
          close sock
          exitSuccess
        when ("> " `BSC.isInfixOf` msg) $ do
          cmd <- getLine
          void $ send sock (cs cmd)
