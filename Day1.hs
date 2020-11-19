module Day1 where

main :: IO ()
main = do
  masses <- readMasses
  let totalMass = sum masses
  let totalFuel = sum $ map fuel $ masses
  let totalFuel' = sum $ map fuel' $ masses
  print $ totalFuel
  print $ totalFuel' - totalMass

readMasses :: IO [Int]
readMasses = map read . lines <$> readFile "Day1.txt"

fuel :: Int -> Int
fuel mass = mass `div` 3 - 2

fuel' :: Int -> Int
fuel' mass = go mass 0
  where
    go :: Int -> Int -> Int
    go mass acc
      | mass > 0 = go (fuel mass) (acc + mass)
      | otherwise = acc
