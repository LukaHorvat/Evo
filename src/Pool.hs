{-# LANGUAGE ViewPatterns #-}
module Pool where

import Common.Prelude
import ByteCode
import Control.Monad.Random hiding (Rand, evalRand, evalRandIO)
import qualified Control.Monad.Random
import qualified Data.Vector.Generic as Vec
import Control.Monad.State

type Rand a = Control.Monad.Random.Rand StdGen a

evalRand :: StdGen -> Rand a -> a
evalRand gen r = Control.Monad.Random.evalRand r gen

evalRandIO :: Rand a -> IO a
evalRandIO r = do
    gen <- newStdGen
    return $ evalRand gen r

choose :: Int -> [(Int, Rand a)] -> Rand a
choose x = choose' . scanl1 (\(l, _) (r, r2) -> (l + r, r2))
    where choose' ((y, r) : rest) = if x <= y then r else choose' rest
          choose' []              = error $ show x ++ " out of range of choices"

weights :: [Op]
weights = Vec.toList ops \\ [Get, Set]

defaultDistribution :: Rand [Op]
defaultDistribution = do
    patternNum <- getRandomR (1, 100)
    choose patternNum
        [ (60, do
            x <- getRandomR (-1.0 :: Float, 1.0)
            let n = round $ x * x * 5
            return [Push n] )
        , (12, return [Get])
        , (8,  return [Set])
        , (10, return <$> uniform weights)
        , (10,  do
            x <- getRandomR (-1.0 :: Float, 1.0)
            let n = round $ (x * x - 0.5) * 100
            b <- getRandomR (1 :: Int, 10)
            return [Push n, if b > 8 then Jmp else Cjmp] )
        ]

intertwine :: [a] -> [a] -> [a]
intertwine l1 l2 = intertwine' $ zip l1 l2
    where intertwine' [(x, _)] = [x]
          intertwine' []       = []
          intertwine' ((x, _) : (_, y) : rest) = x : y : intertwine' rest

crossover :: Code -> Code -> Rand Code
crossover p1 p2 = do
    x <- getRandomR (-1.0 :: Float, 1.0)
    let numSections = avgLen `div` (25 + floor (x * x * 25)) + 1
    limits1 <- limits numSections (Vec.length p1)
    limits2 <- limits numSections (Vec.length p2)
    let slices1 = slices limits1 p1
        slices2 = slices limits2 p2
    return . Vec.concat . intertwine slices1 $ slices2
    where avgLen = (Vec.length p1 + Vec.length p2) `div` 2
          limits n len = (\l -> 0 : l ++ [len]) <$> sort <$> take n <$> getRandomRs (1, len)
          slices lims vec = map (\(l, r) -> Vec.slice l (r - l) vec) . pairwise $ lims

mutate :: Code -> Rand Code
mutate vec = do
    choiceN <- getRandomR (1, 3)
    codeI <- getRandomR (0, Vec.length vec - 1)
    choose choiceN
        [ (1, do
            incDec <- (\x -> x * 2 - 1) <$> getRandomR (0, 1)
            return $ Vec.unsafeUpd vec [(codeI, vec Vec.! codeI + incDec)] )
        , (1, do
            newCode <- serialize <$> defaultDistribution
            let left  = Vec.slice 0 codeI vec
                right = Vec.slice codeI (Vec.length vec - codeI) vec
            return $ Vec.concat [left, newCode, right] )
        , (1, do
            let left  = Vec.slice 0 codeI vec
                right = Vec.slice (codeI + 1) (Vec.length vec - codeI - 1) vec
            return $ Vec.concat [left, right] )
        ]

manyRand :: Int -> Rand a -> Rand [a]
manyRand 0 _ = return []
manyRand n r = do
    x <- r
    xs <- manyRand (n - 1) r
    return $ x : xs

randomCode :: Int -> Rand Code
randomCode n = serialize <$> concat <$> manyRand n defaultDistribution

iterateRand :: Int -> (a -> Rand a) -> a -> Rand [a]
iterateRand 0 _ _ = return []
iterateRand n f a = do
    b <- f a
    bs <- iterateRand (n - 1) f b
    return $ b : bs

data Test = Test { getSetup        :: Rand [[(Int, NumType)]]
                 , getFitness      :: [(Int, NumType)] -> MachineRun -> Double
                 , getNumSurvivors :: Int }

solve :: Int -> Test -> Rand [(Int, [(Code, Double)])]
solve n t = initialGeneration >>= iterateRand n (breed >=> select t) >>= addIndex
    where initialGeneration = map (\c -> (c, 0)) <$> manyRand (getNumSurvivors t) (randomCode 10)
          addIndex gens = return $ zipWith (\i gen -> gen `seq` (i, gen)) [0..] gens

select :: Test -> [Code] -> Rand [(Code, Double)]
select t gen = sorted
    where run s c = getFitness t s . withTimeout 10000 . interpret s $ c
          eval c = do
              setup <- getSetup t
              return $ average [run s c | s <- setup]
          fitM   = mapM eval gen
          sorted = do
              fit <- fitM
              let sorted' = sortBy (flip $ comparing snd) $ zip gen fit
              return $ take (getNumSurvivors t) sorted'

breed :: [(Code, Double)] -> Rand [Code]
breed (map fst -> gen) = do
    mutants <- mapM mutateRand gen
    crosses <- mapM (uncurry crossover) pairs
    return $ gen ++ mutants ++ crosses
    where len = length gen
          pairUp [] = []
          pairUp (x : xs) = map (\y -> (x, y)) xs
          pairs = concatMap pairUp $ tails gen
          mutateRand c = do
              x <- getRandomR (0, 1) :: Rand Double
              let n = ceiling $ fromIntegral (Vec.length c) * x ** 1
              res <- iterateRand n mutate c
              if null res then error "asd" else return $ last res

solveTraceIO :: Int -> Test -> IO [(Code, Double)]
solveTraceIO n t = do
    list <- evalRandIO $ solve n t
    forM_ list $ \(i, gen) -> when (i `mod` 100 == 0) $
        putStrLn $ "Generation " ++ show i ++ " scored " ++ show (maximum $ map snd gen)
    return . snd . last $ list

solveIO :: Int -> Test -> IO [(Code, Double)]
solveIO n t = do
    list <- evalRandIO $ solve n t
    return . snd . last $ list