{-
Main.hs

Created by Geoffrey Natin on 9/12/2017

This file contains top level functions for running a game of core wars 

https://github.com/nating/core-wars/src/Main.hs
-}

module Main where

import Parsing
import DataTypes
import Warrior
import Data.Map as M
import Control.Concurrent
import Control.Concurrent.STM
import System.Console.ANSI
import System.Random
import Data.List as L

timeLimit = 25000000             --25 seconds
memoryLength = 40                --Short for testing. (Standard is 8000)
minWarriorStartPosDistance = 10  --Short for testing. (Standard is 1000)



{-

The program reads in warriors 'programs' from files and places them apart in a DAT initialised map of instructions.

The MARS environment is a Map of integers to Instructions. It is stored in the Mars TVar, which all the warriors have access to.

Each warrior is a thread. Each thread has a list of instruction pointers, which correspond to where each of its tasks are.

Each warrior has access to the logger (MVar). Only one of the warriors can take the logger at once. 
The logger is taken so that a warrior can print out the instruction it has just performed to the console.

The 'main': 
   * creates the environment
   * prints out the starting state of the MARS environment
   * starts the warriors' threads (giving them their starting instruction pointer, a reference to the MARS environment & a reference to the logger)
   * sets a timer for game termination
   * prints out the final state of the MARS environment
-}
main = do
       programs   <- parsePrograms ["examples/program1.txt","examples/program1.txt","examples/program2.txt"]
       positions  <- getWarriorPositions (length programs)
       marsTV     <- atomically $ newTVar $ setUpMarsMemory programs positions memoryLength
       logger     <- newEmptyMVar
       putMVar logger ""
       initialMem <- atomically $ readTVar marsTV
       printMemoryNice initialMem Green
       putStrLn "Starting battle:"
       ids        <- createThreads positions marsTV logger
       threadDelay timeLimit
       mapM killThread ids
       putStrLn "Battle Terminated!"
       finalMem   <- atomically $ readTVar marsTV
       printMemoryNice finalMem Green



--createThreads takes the starting positions of every warrior, the MARS environment (TVar), and the logger (MVar) and starts each warrior off to take their turns.
createThreads :: [Int] -> Mars -> (MVar String) -> IO [ThreadId]
createThreads i m l = mapM (\ x -> forkIO (takeTurns [x] m l)) i



--setUpMarsMemory puts together the initialised memory and the memory positions of the programs, so that the game environment is initialised.
setUpMarsMemory :: [Program] -> [Int] -> Int -> Memory
setUpMarsMemory ps positions len = M.union ( createProgramMaps positions ps ) $ createDatMemory len



--createDatMemory is used to create the DAT initialised memory for the start of a core wars game
createDatMemory :: Int -> Memory
createDatMemory n = fromList $ zip [0..n] $ replicate n (OneFieldOp DAT (Immediate 0))



--createProgramMaps places programs into Memory locations at their starting positions.
--   The memory returned from this can then be 'union'ed with the initialised memory from the start of the game.
createProgramMaps :: [Int] -> [Program] -> Memory
createProgramMaps [] [] = empty
createProgramMaps (i:is) (p:ps) = M.union (fromList $ zip [i..] $ p) $ createProgramMaps is ps



--getWarriorPositions is used to get a list of starting positions for 'n' warriors
getWarriorPositions :: Int -> IO [Int]
getWarriorPositions n = do
       positions <- getNRandoms n ((memoryLength `div` minWarriorStartPosDistance)-1)
       if length (L.nub positions) == length positions
              then return (fmap (* minWarriorStartPosDistance) positions)
              else getWarriorPositions n



--getRands is used to get an infinite list of randomly generated numbers with a value between 1 and 'n'
getRands :: Int -> IO [Int]
getRands n = do
       g <- newStdGen
       return $ randomRs (1,n) g



--getNRandoms is used to get a list of length 'max' of randomly generated numbers with a value between 1 and the 'n'
getNRandoms :: Int -> Int -> IO [Int]
getNRandoms n max = do 
       rs <- getRands max
       return $ take n rs



