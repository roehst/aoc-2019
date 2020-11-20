{-# OPTIONS_GHC -Wall #-}

module Day2 where

type Program = [Int]

split :: Eq a => a -> [a] -> [[a]]
split s l = go s l [] []
  where
    go :: Eq a => a -> [a] -> [a] -> [[a]] -> [[a]]
    go _ [] buf results = reverse $ buf : results
    go sep (x:xs) buf results
      | sep == x = go sep xs [] (reverse buf : results)
      | otherwise = go sep xs (x : buf) results

parse :: String -> Program
parse source = map (read) $ split ',' source

main :: IO ()
main = do
  source <- readFile "Day2.txt"
  let program = parse source
  let program' = update (update program 1 12) 2 2
  print $ eval program' 0

update :: [a] -> Int -> a -> [a]
update [] _ _ = error "List overflow"
update (_:xs) 0 v = v : xs
update (x:xs) n v = x : update xs (n - 1) v

eval :: Program -> Int -> Int
eval program ip =
  let op = program !! ip
   in case op of
        1 ->
          let a = program !! (program !! (ip + 1))
           in let b = program !! (program !! (ip + 2))
               in let dest = program !! (ip + 3)
                   in let program' = update program dest (a + b)
                       in eval program' (ip + 4)
        2 ->
          let a = program !! (program !! (ip + 1))
           in let b = program !! (program !! (ip + 2))
               in let dest = program !! (ip + 3)
                   in let program' = update program dest (a * b)
                       in eval program' (ip + 4)
        99 -> program !! 0
        _ -> error $ "Unknown opcode " ++ show op
