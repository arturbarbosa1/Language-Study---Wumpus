{-# LANGUAGE RecordWildCards #-}
module Wumpus.Actions where

import System.Random

import Data.ByteString (ByteString)
import Data.Char
import Data.String.Conversions

import Wumpus.Grid
import Wumpus.Utils

data CMD = Sleep | Move Int | Shoot Int Int Int Int Int

parsePlayerCmd :: ByteString -> CMD
parsePlayerCmd bs = check . words $ cs bs
  where
    check ("sleep":[]) = Sleep
    check ("move":arg:[]) = if all isDigit arg then Move (read arg) else Sleep
    check ("shoot":r:r1:r2:r3:r4:[]) = if all isDigit (concat [r,r1,r2,r3,r4])
      then Shoot (read r) (read r1) (read r2) (read r3) (read r4)
      else Sleep
    check _ = Sleep

parseWumpusCmd :: ByteString -> CMD
parseWumpusCmd bs = check . words $ cs bs
  where
    check ("sleep":[]) = Sleep
    check ("move":arg:[]) = if all isDigit arg then Move (read arg) else Sleep
    check _ = Sleep

execPlayerCmd :: CMD -> Game -> IO Game
execPlayerCmd Sleep g = return g { arrowSemaphore = False }
execPlayerCmd (Move room) g@Game{..} = do
  batRoom <- randomRIO (1,20)
  return $ if room `closeEnough` (grid !! (player - 1))
    then g { player = checkBatCarriage room batRoom
           , wumpusWins = checkBatCarriage room batRoom == wumpus
                       || checkBatCarriage room batRoom `closeEnoughP` pits
           , arrowSemaphore = False }
    else g { arrowSemaphore = False }
      where
        checkBatCarriage r batR = if closeEnoughB r bats
          then batR
          else r
        closeEnoughP p (Pit a b) = p == a || p == b
        closeEnoughB p (Bat a b) = p == a || p == b
execPlayerCmd (Shoot r1 r2 r3 r4 r5) g@Game{..} = do
  let newArrow = arrows - 1
  -- There is a smart wumpus who acts do not sleep forever. So let me not punish player with random arrows.
  -- Canonical possible strategy - make misdirected arrows dangerous and make wumpus sleep till arrow is wasted. A lot more math game.
      wumpusDead = arrowPath player [r1, r2, r3, r4, r5]
  return $ if newArrow > 0 then g { arrows = newArrow
                                  , playerWins = wumpusDead
                                  , arrowSemaphore = True}
                           else g { arrows = newArrow
                                  , arrowSemaphore = True}
  where
    arrowPath _ [] = False
    arrowPath p [x] = closeEnough p (grid !! (x - 1)) && x == wumpus
    arrowPath p (x:x1:xs) = if closeEnough p (grid !! (x - 1))
                            then (x == wumpus) || arrowPath x (x1:xs)
                            else False

execWumpusCmd :: CMD -> Game -> Game
execWumpusCmd Sleep g = g
execWumpusCmd (Move room) g@Game{..} = if room `closeEnough` (grid !! (wumpus - 1))
  then g { wumpus = room
         , wumpusWins = room == player }
  else g
execWumpusCmd _ g = g
