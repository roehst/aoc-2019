{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Day2 where

import Control.Monad.State

data Program =
  Program
    { pOps :: [Int]
    , pIp :: Int
    }
  deriving (Show)

split :: Eq a => a -> [a] -> [[a]]
split s l = go s l [] []
  where
    go :: Eq a => a -> [a] -> [a] -> [[a]] -> [[a]]
    go _ [] buf results = reverse $ buf : results
    go sep (x:xs) buf results
      | sep == x = go sep xs [] (reverse buf : results)
      | otherwise = go sep xs (x : buf) results

parse :: String -> [Int]
parse source = read <$> split ',' source

update :: Int -> a -> [a] -> [a]
update _ _ [] = []
update 0 a (_:xs) = a : xs
update pos a (x:xs) = x : update (pos - 1) a xs

loadProgram :: IO Program
loadProgram =
  Program <$> (update 1 12 <$> update 2 2 <$> parse <$> readFile "Day2.txt") <*>
  return 0

main :: IO ()
main = do
  program <- loadProgram
  print program
  let (a, _) = runState eval program
  print a

eval :: State Program Int
eval = do
  op <- fetchCurrentInstruction
  case op of
    1 -> do
      addIID 1 2 3
      moveIp 4
      eval
    2 -> do
      mulIID 1 2 3
      moveIp 4
      eval
    99 -> head <$> gets pOps
    _ -> error "Unrecognized op code"

moveIp :: Int -> State Program ()
moveIp offset = do
  program <- get
  put program {pIp = pIp program + offset}

addIID :: Int -> Int -> Int -> State Program ()
addIID a b c = do
  x <- readIndirect a
  y <- readIndirect b
  z <- readDirect c
  writeDirect z (x + y)

mulIID :: Int -> Int -> Int -> State Program ()
mulIID a b c = do
  x <- readIndirect a
  y <- readIndirect b
  z <- readDirect c
  writeDirect z (x * y)

fetchCurrentInstruction :: State Program Int
fetchCurrentInstruction = do
  Program {..} <- get
  return $ pOps !! pIp

readDirect :: Int -> State Program Int
readDirect offset = do
  Program {..} <- get
  return $ pOps !! (pIp + offset)

readIndirect :: Int -> State Program Int
readIndirect offset = do
  Program {..} <- get
  return $ pOps !! (pOps !! (pIp + offset))

writeDirect :: Int -> Int -> State Program ()
writeDirect dest value = do
  Program {..} <- get
  modify $ \s -> s {pOps = update dest value pOps}
