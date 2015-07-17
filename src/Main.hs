module Main where

import Common.Prelude
import ByteCode
import qualified Data.IntMap as Map
import Pool
import Control.Monad.Random hiding (evalRandIO, Rand)
import qualified Data.Vector.Generic as Vec

code :: [Op]
code = [ Push
         1
       , Push
         1
       , Set
       , Push
         0
       , Get
       , Dup
       , Push
         1
       , Get
       , Mult
       , Push
         1
       , Set
       , Push
         (-1)
       , Add
       , Push
         0
       , Set
       , Push
         0
       , Get
       , Push
         (-1)
       , Add
       , Push
         (-25)
       , Cjmp ]

fact :: Int -> Int
fact n = readFinalMemory run 1
    where run = withTimeout 100 $ interpret [(0, n)] (serialize code)

-- fitness :: [(Int, NumType)] -> MachineRun -> [Double]
-- fitness [(0, n), (1, e)] r@(MachineRun run sig)
--     | sig /= PCOverflow = [-10000]
--     | isNothing $ deserialize $ getRunCode r = [-10000]
--     | otherwise         = [initScore - penalties]
--     where initScore = 100 / (abs (fromIntegral (readFinalMemory r 2) - targetFun (fromIntegral n) (fromIntegral e)) + 0.1)
--           len       = fromIntegral $ Vec.length $ getRunCode r
--           penalties = if len >= treshhold then (len - treshhold) ** 5 + treshhold / 1000
--                       else 0--len / 1000
--           targetFun n e = n ** e
--           treshhold   = 100
--
-- test :: Test
-- test = Test { getFitness = fitness
--             , getSetup   = return [[(0, x), (1, y)] | x <- [5..7], y <- [1..3]]
--             , getNumSurvivors = 10
--             , getSeed = [] }

divisionFit :: [(Int, NumType)] -> MachineRun -> Rand [Double]
divisionFit [(0, n)] r@(MachineRun run sig)
    | sig /= PCOverflow && sig /= Timeout =return [-1]
    | isNothing $ deserialize $ getRunCode r = return [-1]
    | otherwise = return [1, initScore -penalties]
    where initScore = 100 / (abs (fromIntegral (readFinalMemory r 1) - fromIntegral (n `div` 2)) + 0.1)
          len       = fromIntegral $ Vec.length $ getRunCode r
          penalties = if len >= treshhold then (len - treshhold) ** 2 + treshhold / 1000
                      else 0
          treshhold   = 100

divTest :: Test
divTest = Test { getFitness = divisionFit
               , getSetup   = return [[(0, n)] | n <- [1..100]]
               , getNumSurvivors = 10
               , getSeed = [] }

c :: Code
c = serialize [Push 9,Bool,Get,Get,Dup,Dup,Get,And,Dup,Get,Dup,Get,Push 3,Dup,And,Push 1,Bool,Pop,Set,Dup,Push 0,Get,Dup,Or,Mult,Dup,Bool,Get,Set,Push 9,Push 0,Dup,Push 3,Dup,And,Push 0,Push 2,Bool,Get,Get,Dup,Get,Get,Mult,Get,Push 2,Push 1,Get,Dup,Get,Push 3,Dup,And,Push 0,Dup,Push 3,Get,Push 2,Push 1,Set,Dup,Push 0,Get,Dup,Or,Mult,Dup,Bool,Get,Set,Push 9,Dup,Push 9,Dup,Bool]

divCode :: Code
divCode = serialize [Push 4,Push 3,Push 0,Get,Push 1,Push 3,Set,Dup,Push 2,Get,Dup,Push 2,Get,Get,Dup,Push 1,Push 2,Set,Get,Get,Set,Push 3,Push 3,Push 4,Push 1,Set,Push 3,Get,Push 4,Push 1,Set,Push 2,Push 0,Get,Jmp,Push 2,Get,Dup,Push 0,Pop,Set,Push 2,Push 0,Get,Jmp,Push 2,Get,Dup,Push 1,Set,Dup,Push 9,Get,Dup,Push 2,Push 9,Get,Dup,Push 9,Get,Get,Set,Dup]

expTest :: Int -> Int -> Int
expTest n e = readFinalMemory (withTimeout 10000 $ interpret [(0, n), (1, e)] c) 2

div' :: Int -> Int
div' n = readFinalMemory (withTimeout 10000 $ interpret [(0, n)] divCode) 1

main :: IO ()
main = do
    res <- solveTraceIO 100000 divTest
    mapM_ (\(c, _) -> print $ deserialize c) res
