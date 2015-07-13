{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Common.Prelude ( module Common.Prelude
                      , module X) where

import Prelude as X hiding (fst, snd)
import Common.PreludeTH
import Control.Applicative as X
import Control.Monad as X
import Data.List as X
import Data.Function as X
import Data.Ord as X
import Data.Char as X
import Data.Maybe as X
import Debug.Trace hiding (trace)

generateTupleAccessors ["fst", "snd", "trd", "frth", "ffth"]

trace :: Show a => a -> a
trace = traceShowId

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new = replace'
    where len = length old
          replace' [] = []
          replace' tgt | take len tgt == old = new ++ replace' (drop len tgt)
          replace' (x : xs) = x : replace' xs

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith list prefix = take (length prefix) list == prefix

changeIf :: (a -> Bool) -> a -> a -> a
changeIf p v x | p x       = v
               | otherwise = x

pairwise :: [a] -> [(a, a)]
pairwise (x : y : xs) = (x, y) : pairwise (y : xs)
pairwise _            = []

iterateM :: Monad m => (a -> m a) -> a -> m [a]
iterateM f a = do
    b <- f a
    bs <- iterateM f b
    return $ b : bs

average :: Fractional a => [a] -> a
average l = sum l / fromIntegral (length l)
