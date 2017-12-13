
module MyRealMain where

import Parsing
import DataTypes
import Control.Concurrent



{- THE PLAN

There are n threads which are just like variables that I create that run their own functions in turns

There is one memory 'TVar' which is a variable that each of the threads can see and edit 
(but not two at once, [but none of them will ever be running at once anyway])

There is a function that creates the memory 'TVar' and the threads to start at different instructions, then runs them.

Each of the threads has a threadID and a list of instruction pointers.

A thread can branch off to have multiple instruction pointers (adds to the list of ips).

Once a thread's instruction pointers list is empty, it is killed.


These are the thread functions:
    forkIO :: IO () -> IO ThreadId
    2 killThread :: ThreadId -> IO ()
    3 threadDelay :: Int -> IO ()

1 main = do
    2 forkIO (forever $ putChar 'o')
    3 forkIO (forever $ putChar 'O')

-}

fetty = parsePrograms ["program1.txt","program2.txt"]

wap = do
       ps <- parsePrograms ["program1.txt","program2.txt"]
       myMars <- atomically $ newTVar $ setMarsMemory 8000 ps
       before <- atomRead myMars
       putStrLn $ "Before: " ++ show before

createThreads :: [Int] -> [IO ThreadId]
createThreads i = fmap (\ x -> forkIO (takeRedcodeTurn [x])) i

setMarsMemory :: Int -> [Program] -> Memory
setMarsMemory n p = union ( unions $ fmap (createProgramMap 1) p) $ createDatMemory n

createDatMemory :: Int -> Memory
createDatMemory n = fromList $ zip [1..n] $ replicate n (OneFieldOp DAT (Immediate 0))

createProgramMap :: Int -> Program -> Memory
createProgramMap s p = fromList $ zip [s..] $ p




