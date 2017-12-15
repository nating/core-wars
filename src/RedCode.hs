{-
RedCode.hs

Created by Geoffrey Natin on 9/12/2017

https://github.com/nating/core-wars/src/RedCode.hs
-}

module RedCode where

import DataTypes
import Parsing
import Data.Map as M
import Control.Concurrent
import Control.Concurrent.STM
import Data.Maybe
import System.Console.ANSI
import Data.Hashable

warriorWaitTime = 1000000 --This slows the game down, so users can read what is going on in the game. Warriors wait before executing their next instruction.

{-
    take
-}
takeTurns :: [Int] -> Mars -> (MVar String) -> IO ()
takeTurns (i:ips) mars l = do
    (inst,newIPList) <- atomically $ takeTurns' (i:ips) mars
    loggedTurns <- takeMVar l
    id <- myThreadId
    setSGR [SetColor Foreground Vivid (getThreadColor id)]
    putStrLn $ (show id)++" at location "++(show i)++"  |   "++(show inst)
    threadDelay warriorWaitTime
    putMVar l "TODO"
    takeTurns newIPList mars l
takeTurns [] _ _ = do
    id <- myThreadId
    killThread id

takeTurns' :: [Int] -> Mars -> STM (Instruction,[Int])
takeTurns' (ip:ips) mars = do
    m <- (readTVar mars)
    let inst = getInstruction ip m
    let (nextIp,newMars) = performInstruction ip inst m
    writeTVar mars newMars
    case inst of
        (OneFieldOp DAT _) -> return (inst,ips)
        (TwoFieldOp DAT _ _) -> return (inst,ips)
        (OneFieldOp SPL _) -> return (inst,(ips++[nextIp,addrAdd ip 1 m]))
        _ -> return (inst,(ips++[nextIp]))

getInstruction :: Int -> Memory -> Instruction
getInstruction i mem = case M.lookup i mem of 
                         Just inst -> inst

-------------------------------INSTRUCTIONS---------------------------------------------

performInstruction :: Int -> Instruction -> Memory -> (Int,Memory)
performInstruction i (OneFieldOp o f) m     = ((performOne i o f m),m)
performInstruction i (TwoFieldOp o f1 f2) m = performTwo i o f1 f2 m

performOne :: Int -> Op -> Field -> Memory -> Int
performOne _ DAT _ _ = -1
performOne i JMP a m = addrAdd i (getFieldValue a) m
performOne i SPL a m = addrAdd i (getFieldValue a) m

performTwo :: Int -> Op -> Field -> Field -> Memory -> (Int,Memory)
performTwo i MOV (Immediate v) f2 m = (addrAdd i 1 m,newMem)
                            where 
                              targetAddress = getFinalAddress i f2 m
                              newMem = M.insert targetAddress (OneFieldOp DAT (Direct v)) m --TODO: I think here I might have to deal with the A-Field having an addressing mode
performTwo i MOV f1 f2 m = (addrAdd i 1 m,newMem)
                            where 
                              targetAddress = getFinalAddress i f2 m
                              overwritingInstructionIdx = getFinalAddress i f1 m
                              overwritingInstruction = fromJust $ M.lookup overwritingInstructionIdx m
                              newMem = M.insert targetAddress overwritingInstruction m --TODO: I think here I might have to deal with the A-Field having an addressing mode
performTwo i ADD f1 f2 m = (addrAdd i 1 m,newMem)
                            where
                              targetAddress = getFinalAddress i f2 m
                              targetInstruction = fromJust $ M.lookup targetAddress m
                              prevValue = getFieldValue $ getBField $ fromJust $ M.lookup targetAddress m
                              addVal = getFieldValue f1
                              updatedInstruction = updateBField targetInstruction (addrAdd prevValue addVal m)
                              newMem = M.insert targetAddress updatedInstruction m --TODO: I think here I might have to deal with the A-Field having an addressing mode
performTwo i SUB f1 f2 m = (addrAdd i 1 m,newMem)
                            where
                              targetAddress = getFinalAddress i f2 m
                              targetInstruction = fromJust $ M.lookup targetAddress m
                              prevValue = getFieldValue $ getBField $ fromJust $ M.lookup targetAddress m
                              subVal = getFieldValue f1
                              updatedInstruction = updateBField targetInstruction (addrSub prevValue subVal m)
                              newMem = M.insert targetAddress updatedInstruction m --TODO: I think here I might have to deal with the A-Field having an addressing mode
performTwo i JMZ f1 f2 m = if (getFieldValue f2)==0 then (addrAdd i (getFinalAddress i f1 m) m,m)
                           else (addrAdd i 1 m,m)
performTwo i JMN f1 f2 m = if (getFieldValue f2) /= 0 then (addrAdd i (getFinalAddress i f1 m) m,m)
                           else (addrAdd i 1 m,m)
performTwo i DJN f1 f2 m = if ((getFieldValue f2)-1) /= 0 then (addrAdd i (getFinalAddress i f1 m) m,m)
                           else (addrAdd i 1 m,m)
performTwo i CMP f1 f2 m = if (getFieldValue $ getBField $ fromJust $ M.lookup (getFinalAddress i f1 m) m)==(getFieldValue $ getBField $ fromJust $ M.lookup (getFinalAddress i f2 m) m) then ((i+1),m)
                           else (addrAdd i 2 m,m)

getFinalAddress :: Int -> Field -> Memory -> Int
getFinalAddress i (Direct v) m        = getAddress i m
getFinalAddress i (Immediate v) m     = getAddress i m
getFinalAddress i (Indirect v) m      = getAddress (addrAdd i v m) m
getFinalAddress i (AutoDecrement v) m = getAddress (addrAdd (i-1) v m) m

getAddress :: Int -> Memory -> Int
getAddress i m = case M.lookup i m of 
                      Just (OneFieldOp _ f1)   -> addrAdd i (getFieldValue f1) m
                      Just (TwoFieldOp _ _ f2) -> addrAdd i (getFieldValue f2) m

getFieldValue :: Field -> Int
getFieldValue (Direct v)        = v
getFieldValue (Indirect v)      = v
getFieldValue (Immediate v)     = v
getFieldValue (AutoDecrement v) = v

getBField :: Instruction -> Field
getBField (OneFieldOp _ b)   = b
getBField (TwoFieldOp _ _ b) = b

updateBField :: Instruction -> Int -> Instruction
updateBField (OneFieldOp o (Direct b)) i          = (OneFieldOp o (Direct i))
updateBField (OneFieldOp o (Immediate b)) i       = (OneFieldOp o (Immediate i))
updateBField (OneFieldOp o (Indirect b)) i        = (OneFieldOp o (Indirect i))
updateBField (OneFieldOp o (AutoDecrement b)) i   = (OneFieldOp o (AutoDecrement i))
updateBField (TwoFieldOp o a (Direct b)) i        = (TwoFieldOp o a (Direct i))
updateBField (TwoFieldOp o a (Immediate b)) i     = (TwoFieldOp o a (Immediate i))
updateBField (TwoFieldOp o a (Indirect b)) i      = (TwoFieldOp o a (Indirect i))
updateBField (TwoFieldOp o a (AutoDecrement b)) i = (TwoFieldOp o a (AutoDecrement i))

getAField :: Instruction -> Field
getAField (OneFieldOp _ a)   = a
getAField (TwoFieldOp _ a _) = a

addrAdd :: Int -> Int -> Memory -> Int
addrAdd a b m = (a+b) `mod` (length m)

addrSub :: Int -> Int -> Memory -> Int
addrSub a b m = abs $ (a-b) `mod` (length m)

getThreadColor :: ThreadId -> Color
getThreadColor tid = case (hash (show tid)) `mod` 5 of
    0 -> Red  
    1 -> Yellow   
    2 -> Blue     
    3 -> Magenta  
    4 -> Cyan     











