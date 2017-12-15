{-
Main.hs

Created by Geoffrey Natin on 9/12/2017

https://github.com/nating/core-wars/src/Main.hs
-}

module MyRealMain where

import Parsing
import DataTypes
import RedCode
import Data.Map as M
import Control.Concurrent
import Control.Concurrent.STM
import System.Console.ANSI
import System.Random
import Data.List as L

timeLimit = 50000000 --5 seconds
memoryLength = 40
minWarriorStartPosDistance = 10

wap = do
       ps <- parsePrograms ["program1.txt","program2.txt"]
       myMars <- atomically $ newTVar $ setUpMarsMemory ps memoryLength
       logger <- newEmptyMVar
       putMVar logger "TODO"
       initialMem <- atomically $ readTVar myMars
       printMemoryNice initialMem Green
       putStrLn "Starting battle:"
       ids <- createThreads [1,2,6] myMars logger
       threadDelay timeLimit
       mapM killThread ids
       putStrLn "Battle Terminated!"
       finalMem <- atomically $ readTVar myMars
       printMemoryNice finalMem Green

createThreads :: [Int] -> Mars -> (MVar String) -> IO [ThreadId]
createThreads i m l = mapM (\ x -> forkIO (takeTurns [x] m l)) i

setUpMarsMemory :: [Program] -> Int -> Memory
setUpMarsMemory ps len = M.union ( createProgramMaps [1,15] ps ) $ createDatMemory len

createDatMemory :: Int -> Memory
createDatMemory n = fromList $ zip [1..n] $ replicate n (OneFieldOp DAT (Immediate 0))

createProgramMaps :: [Int] -> [Program] -> Memory
createProgramMaps [] [] = empty
createProgramMaps (i:is) (p:ps) = M.union (fromList $ zip [i..] $ p) $ createProgramMaps is ps

-----------------Stuff trying to get random to work

{-

g2 :: Int -> IO [Int]
g2 n = do
       g <- newStdGen
       return $ randomRs (1,n) g

getRandoms :: Int -> Int -> IO [Int]
getRandoms n max = do 
       rs <- g2 max
       return $ take n rs

getWarriorPositions :: Int -> Int -> IO [Int]
getWarriorPositions l m = do
       let rs = getRandoms l m 
       if length (L.nub rs) == length rs
              then do return $ rs
              else do return $ getWarriorPositions l m --This might make the exact same random numbers
-}

