{-# LANGUAGE TypeApplications #-}
-- |

module Day01 where

import Utils

solution1 :: IO Int
solution1 = do
  samples <- load @Int "./day01.txt"

  let zipped = zip samples (tail samples)
      sol = length . filter (uncurry (<)) $ zipped
  pure sol

solution2 :: IO Int
solution2 = do
  samples <- load @Int "./day01.txt"

  pure (calc samples 0)


calc :: [Int] -> Int -> Int
calc [] acc = acc
calc ls acc = calc (tail ls) (if increase ls then acc + 1 else acc)
  where    
    increase :: [Int] -> Bool
    increase xs = sumThree xs < (sumThree . tail $ xs)
    sumThree :: [Int] -> Int
    sumThree = sum . take 3
