
module MyRealMain where

import Parsing
import DataTypes
import RedCode
import Data.Map
import Control.Concurrent
import Control.Concurrent.STM
import System.Console.ANSI

wap = do
       ps <- parsePrograms ["program1.txt","program2.txt"]
       myMars <- atomically $ newTVar $ setUpMarsMemory 20 ps
       logger <- newEmptyMVar
       putMVar logger "TODO"
       initialMem <- atomically $ readTVar myMars
       printMemoryNice initialMem Green
       putStrLn "Starting battle:"
       ids <- createThreads [1,2,6] myMars logger
       threadDelay 50000000
       mapM killThread ids
       putStrLn "Battle Terminated!"
       finalMem <- atomically $ readTVar myMars
       printMemoryNice finalMem Green

createThreads :: [Int] -> Mars -> (MVar String) -> IO [ThreadId]
createThreads i m l = mapM (\ x -> forkIO (takeRedcodeTurn [x] m l)) i

setUpMarsMemory :: Int -> [Program] -> Memory
setUpMarsMemory n p = union ( unions $ fmap (createProgramMap 1) p) $ createDatMemory n

createDatMemory :: Int -> Memory
createDatMemory n = fromList $ zip [1..n] $ replicate n (OneFieldOp DAT (Immediate 0))

createProgramMap :: Int -> Program -> Memory
createProgramMap s p = fromList $ zip [s..] $ p


{-

What's left to do:

Read the programs in from the command line (Just take strings in as arguments to the main function, and then parse all of them)

Place the programs in random places in memory (a function that creates numbers and if they're right, returns them, else returns another call to itself)
	(Must increase memory size if they are too long to all fit in comfortably)

Test all of the instructions

Reformat all of the code

Add comments project wide

-}