module Wumpus.Grid where

import System.Random

data Pit = Pit Int Int deriving (Eq, Show)
data Bat = Bat Int Int deriving (Eq, Show)
type Wumpus = Int
type Player = Int

data Room = Room Int Int Int

data Game = Game
  { grid           :: [Room]
  , pits           :: Pit
  , bats           :: Bat
  , wumpus         :: Wumpus
  , player         :: Player
  , arrows         :: Int
  , arrowSemaphore :: Bool
  , playerWins     :: Bool
  , wumpusWins     :: Bool
  }

-- Seems it's not important to have random enumeration. So it's hardcoded.
-- Thoughts on how to fix: build one facet first. Then add corresp. vertexes going through cartesian square of numbers.
-- It's non-functional algorhitm at all.
dodecahedron :: [Room]
dodecahedron =
  Room 2 3 4 :
  Room 1 9 10 :
  Room 1 9 7 :
  Room 1 5 6 :
  Room 4 10 14 :
  Room 4 7 13 :
  Room 3 6 12 :
  Room 3 9 11 :
  Room 2 8 17 :
  Room 2 5 16 :
  Room 8 12 18 :
  Room 7 11 15 :
  Room 6 14 15 :
  Room 5 13 20 :
  Room 12 13 19 :
  Room 10 17 20 :
  Room 9 16 18 :
  Room 11 17 19 :
  Room 15 18 20 :
  Room 14 19 16 : []

generateGame :: IO Game
generateGame = do
  -- Let's have variability!
  batRoom1 <- randomRIO (2,20)
  batRoom2 <- randomRIO (2,20)
  pitRoom1 <- randomRIO (2,20)
  pitRoom2 <- randomRIO (2,20)
  wumpusRoom <- randomRIO (2,20)
  return $ Game
    dodecahedron
    (Pit pitRoom1 pitRoom2)
    (Bat batRoom1 batRoom2)
    wumpusRoom 1 5 False False False
