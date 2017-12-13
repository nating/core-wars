

{-
This function performs an instruction from the memory

It gets the current address of the thread, and does the action indicated by the instruction at that address

-}

import DataTypes
import Parsing
import Data.Map as M
import Control.Concurrent
import Data.Maybe

takeRedcodeTurn :: [Int] -> IO ()
takeRedcodeTurn (ip:ips) = do
                            mars <- getMars
                            let inst = getInstruction ip mars
                            let (nextIp,newMars) = performInstruction ip inst mars
                            print inst
                            case inst of
                               (OneFieldOp DAT _) -> takeRedcodeTurn $ ips
                               (TwoFieldOp DAT _ _) -> takeRedcodeTurn $ ips
                               (OneFieldOp SPL _) -> takeRedcodeTurn $ ips++[nextIp]++[ip]
                               _ -> takeRedcodeTurn $ ips++[nextIp]

getInstruction :: Int -> Memory -> Instruction
getInstruction i mem = case M.lookup i mem of 
                         Just inst -> inst

-------------------------------INSTRUCTIONS---------------------------------------------

performInstruction :: Int -> Instruction -> Memory -> (Int,Memory)
performInstruction i (OneFieldOp o f) m = ((performOne i o f m),m)
performInstruction i (TwoFieldOp o f1 f2) m = performTwo i o f1 f2 m

performOne :: Int -> Op -> Field -> Memory -> Int
performOne _ DAT _ _ = -1
performOne i JMP a m = (i+ getFieldValue a) `mod` (length m)
performOne i SPL a m = (i+ getFieldValue a) `mod` (length m)

performTwo :: Int -> Op -> Field -> Field -> Memory -> (Int,Memory)
performTwo i MOV f1 f2 m = ((i+1),newMem)
                            where 
                            targetAddress = getFinalAddress i f2 m
                            overwritingInstructionIdx = getFinalAddress i f1 m
                            overwritingInstruction = fromJust $ M.lookup overwritingInstructionIdx m
                            newMem = M.insert targetAddress overwritingInstruction m --TODO: I think here I might have to deal with the A-Field having an addressing mode
performTwo i ADD f1 f2 m = ((i+1),newMem)
                            where
                              targetAddress = getFinalAddress i f2 m
                              targetInstruction = fromJust $ M.lookup targetAddress m
                              prevValue = getFieldValue $ getBField $ fromJust $ M.lookup targetAddress m
                              addVal = getFieldValue f1
                              updatedInstruction = updateBField targetInstruction (prevValue+addVal)
                              newMem = M.insert targetAddress updatedInstruction m --TODO: I think here I might have to deal with the A-Field having an addressing mode
performTwo i SUB f1 f2 m = ((i+1),newMem)
                            where
                              targetAddress = getFinalAddress i f2 m
                              targetInstruction = fromJust $ M.lookup targetAddress m
                              prevValue = getFieldValue $ getBField $ fromJust $ M.lookup targetAddress m
                              subVal = getFieldValue f1
                              updatedInstruction = updateBField targetInstruction (prevValue-subVal)
                              newMem = M.insert targetAddress updatedInstruction m --TODO: I think here I might have to deal with the A-Field having an addressing mode
performTwo i JMZ f1 f2 m = if (getFieldValue f2)==0 then ((i + getFinalAddress i f1 m),m)
                           else ((i+1),m)
performTwo i JMN f1 f2 m = if (getFieldValue f2) /= 0 then (i + (getFinalAddress i f1 m),m)
                           else ((i+1),m)
performTwo i DJN f1 f2 m = if ((getFieldValue f2)-1) /= 0 then (i + (getFinalAddress i f1 m),m)
                           else ((i+1),m)
performTwo i CMP f1 f2 m = if (getFieldValue $ getBField $ fromJust $ M.lookup (getFinalAddress i f1 m) m)==(getFieldValue $ getBField $ fromJust $ M.lookup (getFinalAddress i f2 m) m) then ((i+1),m)
                           else ((i+2),m)

getFinalAddress :: Int -> Field -> Memory -> Int
getFinalAddress i (Direct v) mem        = getAddress i mem
getFinalAddress i (Immediate v) mem     = getAddress i mem
getFinalAddress i (Indirect v) mem      = getAddress (i+v) mem
getFinalAddress i (AutoDecrement v) mem = getAddress (i - 1 + v) mem

getAddress :: Int -> Memory -> Int
getAddress i mem = case M.lookup i mem of 
                      Just (OneFieldOp _ f1) -> i + (getFieldValue f1)
                      Just (TwoFieldOp _ _ f2) -> i + (getFieldValue f2)

getFieldValue :: Field -> Int
getFieldValue (Direct v) = v
getFieldValue (Indirect v) = v
getFieldValue (Immediate v) = v
getFieldValue (AutoDecrement v) = v

getBField :: Instruction -> Field
getBField (OneFieldOp _ b) = b
getBField (TwoFieldOp _ _ b) = b

updateBField :: Instruction -> Int -> Instruction
updateBField (TwoFieldOp o a (Direct b)) i = (TwoFieldOp o a (Direct i))
updateBField (TwoFieldOp o a (Immediate b)) i = (TwoFieldOp o a (Immediate i))
updateBField (TwoFieldOp o a (Indirect b)) i = (TwoFieldOp o a (Indirect i))
updateBField (TwoFieldOp o a (AutoDecrement b)) i = (TwoFieldOp o a (AutoDecrement i))

getAField :: Instruction -> Field
getAField (OneFieldOp _ a) = a
getAField (TwoFieldOp _ a _) = a

getMars :: IO Memory
getMars = return $ fromList [(0,(OneFieldOp DAT (Direct 0)))]
