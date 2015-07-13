{-# LANGUAGE RecordWildCards, LambdaCase #-}
module ByteCode where

import Common.Prelude
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector
import qualified Data.Vector.Generic as Vec
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import Data.Bits

type NumType = Int

data Op = Push NumType
        | Pop  -- Pop a value of the stack (removes it)
        | Add  -- Pop two values of the stack, add them and push the result
        | Mult -- Pop two values of the stack, mutiply them and push the result
        | Dup  -- Push the value currently on top of the stack
        | Set  -- Pop two values off the stack, the topmost one is the address
               -- and the next one is the value to store in that memory location
        | Get  -- Pop one value of the stack, read the value at that memory location
               -- and push the result
        | And  -- Pop two values of the stack, AND them and push the result
        | Or   -- Pop two values of the stack, OR them and push the result
        | Xor  -- Pop two values of the stack, XOR them and push the result
        | Jmp  -- Pop a value from the stack and jump to that index in the code (relative)
        | Cjmp -- Pop a two value from the stack, the topmost is the index to which
               -- to jump to if the second value is not equal to zero (relative)
        | Swp  -- Swaps the two values on the top of the stack
        | Bool -- Pop a value of the stack and push 0 if the value was 0, 1 otherwise
        deriving (Eq, Ord, Read, Show)

oneParam, twoParam, ops :: Data.Vector.Vector Op
oneParam = Vec.fromList [Pop, Dup, Get, Jmp, Bool]
twoParam = Vec.fromList [Add, Mult, Set, And, Or, Xor, Cjmp, Swp]
ops = oneParam Vec.++ twoParam

toCode :: Op -> [NumType]
toCode (Push n) = [0, n]
toCode other     =
    case Vec.elemIndex other ops of
        Nothing -> error $ "Op code " ++ show other ++ " is unaccounted for"
        Just  i -> [fromIntegral i + 1]

fromCode :: NumType -> Maybe Op
fromCode = (ops Vec.!?) . subtract 1 . fromIntegral

numParams :: Op -> Int
numParams op | op `Vec.elem` oneParam = 1
             | op `Vec.elem` twoParam = 2
             | otherwise                      =
                 error $ "Op code " ++ show op ++ " doesn't have a parameter count"

type Code = Vector NumType

serialize :: [Op] -> Code
serialize = Vec.fromList . concatMap toCode

deserialize :: Code -> Maybe [Op]
deserialize = sequence . deserialize' . Vec.toList
    where deserialize' (0 : x : xs) = Just (Push x) : deserialize' xs
          deserialize' (x : xs)     = fromCode x    : deserialize' xs
          deserialize' []           = []

data MachineSignal = PCUnderflow
                   | PCOverflow
                   | StackUnderflow
                   | MissingValue
                   | BadCode
                   | Timeout
                   deriving (Eq, Ord, Read, Show)

data Machine = Machine { getMemory         :: IntMap NumType
                       , getProgramCounter :: NumType
                       , getStack          :: [NumType]
                       , getCode           :: Code
                       , getStackSize      :: Int }
                       deriving (Eq, Ord, Read, Show)

checkPCUnderflow :: Machine -> Either MachineSignal Machine
checkPCUnderflow m@Machine{..}
    | getProgramCounter < 0 = Left PCUnderflow
    | otherwise             = Right m

checkPCOverflow :: Machine -> Either MachineSignal Machine
checkPCOverflow m@Machine{..}
    | Vec.length getCode <= getProgramCounter = Left PCOverflow
    | otherwise                               = Right m

checkMissingValue :: Machine -> Either MachineSignal Machine
checkMissingValue m@Machine{..}
    | Vec.length getCode <= getProgramCounter = Left MissingValue
    | otherwise                               = Right m

checkStackUnderflow :: Int -> Machine -> Either MachineSignal Machine
checkStackUnderflow n m@Machine{..}
    | getStackSize >= n = Right m
    | otherwise         = Left StackUnderflow

getOp :: Int -> Either MachineSignal Op
getOp n = maybe (Left BadCode) Right $ fromCode n

iterate' :: (a -> Either b a) -> a -> [Either b a]
iterate' f x = case f x of
    Left b  -> [Right x, Left b]
    Right y -> Right x : iterate' f y

incCounter :: Machine -> Machine
incCounter m = m { getProgramCounter = getProgramCounter m + 1 }

advance :: Machine -> Either MachineSignal Machine
advance = checkPCUnderflow >=> checkPCOverflow >=> readCode
    where readCode m@Machine{..} = case currentOp of
              0 -> pushNext . incCounter $ m
              n -> getOp n >>= \op -> processOp op m
              where currentOp = getCode Vec.! getProgramCounter

processOp :: Op -> Machine -> Either MachineSignal Machine
processOp op = checkStackUnderflow opNumParams >=> (return . incCounter . runOp . popParams opNumParams)
    where opNumParams = numParams op
          popParams n m@Machine{..} = (take n getStack, m { getStack = drop n getStack
                                                          , getStackSize = getStackSize - n })
          runOp (params, m@Machine{..}) = case op of
              Add  -> pushValue (x + y) m
              Pop  -> m
              Mult -> pushValue (x * y) m
              Dup  -> pushValue x . pushValue x $ m
              Set  -> m { getMemory = Map.insert (fromIntegral x) y getMemory }
              Get  -> pushValue (fromMaybe 0 $ Map.lookup (fromIntegral x) getMemory) m
              And  -> pushValue (x .&. y) m
              Or   -> pushValue (x .|. y) m
              Xor  -> pushValue (x `xor` y) m
              Jmp  -> moveCounter (-1) . moveCounter (fromIntegral x) $ m
              Cjmp -> if y /= 0 then moveCounter (-1) . moveCounter (fromIntegral x) $ m else m
              Swp  -> pushValue x . pushValue y $ m
              Bool -> if x /= 0 then pushValue 1 m else pushValue 0 m
              Push _ -> error "progressOp given Push"
              where x = head params
                    y = head . tail $ params

moveCounter :: Int -> Machine -> Machine
moveCounter n m@Machine{..} = m { getProgramCounter = getProgramCounter + n }

pushValue :: NumType -> Machine -> Machine
pushValue x m@Machine{..} = m { getStack = x : getStack
                              , getStackSize = getStackSize + 1 }

pushNext :: Machine -> Either MachineSignal Machine
pushNext = checkMissingValue >=> (return . incCounter . doPush)
    where doPush m@Machine{..} = pushValue (getCode Vec.! getProgramCounter) m

interpret :: [(Int, NumType)] -> Code -> [Either MachineSignal Machine]
interpret mem vec = iterate' advance initial
    where initial = Machine { getMemory         = foldl' (flip . uncurry $ Map.insert) Map.empty mem
                            , getProgramCounter = 0
                            , getStack          = []
                            , getCode           = vec
                            , getStackSize      = 0 }

data MachineRun = MachineRun { getStates :: [Machine]
                             , getSignal :: MachineSignal }
                             deriving (Eq, Ord, Show, Read)

getRun :: [Either MachineSignal Machine] -> MachineRun
getRun states = MachineRun [state | Right state <- states] (head [signal | Left signal <- states])

withTimeout :: Int -> [Either MachineSignal Machine] -> MachineRun
withTimeout n states
    | not . null . drop n $ states = MachineRun [state | Right state <- take n states] Timeout
    | otherwise                    = getRun states

lastState :: MachineRun -> Machine
lastState MachineRun{..} = if null getStates then error $ show getSignal else last getStates

readFinalMemory :: MachineRun -> Int -> Int
readFinalMemory m i = fromMaybe 0 $ Map.lookup i (getMemory $ lastState m)

getRunCode :: MachineRun -> Code
getRunCode = getCode . head . getStates

defaultRun :: Code -> MachineRun
defaultRun = withTimeout 10000 . interpret []
