{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Day2 where

type MachineL = ([Int], Int)

class Machine a where
  getInstructionPointer :: a -> Int
  setInstructionPointer :: Int -> a -> a
  createFromSource :: String -> a
  readDirect :: Int -> a -> Int
  readIndirect :: Int -> a -> Int
  writeDirect :: Int -> Int -> a -> a
  getNextOp :: a -> Int

instance Machine MachineL where
  getInstructionPointer (_, i) = i
  setInstructionPointer i (p, _) = (p, i)
  createFromSource source = ((map read $ splitOn ',' source), 0)
  readDirect pos (code, _) = code !! pos
  readIndirect pos (code, _) = code !! (code !! pos)
  writeDirect addr val (code, ip) =
    (take addr code ++ [val] ++ drop (1 + addr) code, ip)
  getNextOp (code, ip) = code !! ip

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn sep xs = splitOn' sep xs [] []

splitOn' :: Eq a => a -> [a] -> [a] -> [[a]] -> [[a]]
splitOn' _ [] buf results = reverse $ reverse buf : results
splitOn' sep (x:xs) buf results
  | x == sep = splitOn' sep xs [] (reverse buf : results)
  | otherwise = splitOn' sep xs (x : buf) results

main :: IO ()
main = do
  source <- readFile "Day2.txt"
  let machine = (createFromSource source) :: MachineL
  print $ run machine
  case search machine inputList 19690720 of
    Just (a, b) -> print $ (100 * a + b)
    Nothing -> print "No solution found"

search :: Machine a => a -> [(Int, Int)] -> Int -> Maybe (Int, Int)
search _ [] _ = Nothing
search m (x:xs) goal
  | runOnInputPair m x == goal = Just x
  | otherwise = search m xs goal

inputList :: [(Int, Int)]
inputList = [(a, b) | a <- [0 .. 99], b <- [0 .. 99]]

runOnInputPair :: Machine a => a -> (Int, Int) -> Int
runOnInputPair m (a, b) = runWithInputs a b m

runWithInputs :: Machine a => Int -> Int -> a -> Int
runWithInputs a b m = run (writeDirect 2 b (writeDirect 1 a m))

run :: Machine a => a -> Int
run m =
  let op = getNextOp m
   in let ip = getInstructionPointer m
       in case op of
            1 ->
              let a = readIndirect (ip + 1) m
               in let b = readIndirect (ip + 2) m
                   in let d = readDirect (ip + 3) m
                       in let m' = writeDirect d (a + b) m
                           in run (setInstructionPointer (ip + 4) m')
            2 ->
              let a = readIndirect (ip + 1) m
               in let b = readIndirect (ip + 2) m
                   in let d = readDirect (ip + 3) m
                       in let m' = writeDirect d (a * b) m
                           in run (setInstructionPointer (ip + 4) m')
            99 -> readDirect 0 m
            _ -> error $ "Undefined instruction: " ++ show op
