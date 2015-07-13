module Main where

import Common.Prelude
import ByteCode
import qualified Data.IntMap as Map
import Pool
import Control.Monad.Random hiding (evalRandIO)
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

fitness :: [(Int, NumType)] -> MachineRun -> Double
fitness [(0, n), (1, e)] r@(MachineRun run sig)
    | sig /= PCOverflow = -10000
    | isNothing $ deserialize $ getRunCode r = -10000
    | otherwise         = initScore - penalties
    where initScore = 100 / (abs (fromIntegral (readFinalMemory r 2) - targetFun (fromIntegral n) (fromIntegral e)) + 0.1)
          len       = fromIntegral $ Vec.length $ getRunCode r
          penalties = if len >= treshhold then (len - treshhold) ** 5 + treshhold / 1000
                      else len / 1000
          targetFun n e = n ** e
          treshhold   = 30

test :: Test
test = Test { getFitness = fitness
            , getSetup   = return [[(0, x), (1, y)] | x <- [5..7], y <- [1..3]]
            , getNumSurvivors = 10 }

main :: IO ()
main = do
    res <- solveTraceIO 10000 test
    print $ deserialize $ fst $ head res
