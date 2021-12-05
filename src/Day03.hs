{-# LANGUAGE TypeApplications #-}
module Day03 where

import Utils
import Data.List
import Prelude 
import Data.Char (digitToInt)


solutions :: IO ()
solutions = do
  l <- load' "./day03.txt"
  let t = transpose l
      rawGamma = snd . mostCommon <$> t
      gamma = toDec rawGamma
      epsilon = toDec . invert $ rawGamma
      part1 = gamma * epsilon
  print part1

  let oxygen = toDec . oxygenGenRating $ l
      co2    = toDec . co2ScrubberRating $ l
      part2 = oxygen * co2
  print part2

mostCommon :: (Ord a) => [a] -> (Int, a)
mostCommon = maximum . map (\x -> (length x, head x)) . group . sort

invert :: String -> String
invert = fmap (\c -> if c == '0' then '1' else '0')

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0


rating :: (Ord a) => [[a]] -> Int -> (Bool -> Bool) -> [a]
rating [x] _ _ = x
rating xs idx f =
  let (n, a) = mostCommon $ column xs idx
      xa' = take n $ filter (f . same idx a) xs
      idx' = idx + 1
      in rating xa' idx' f
  where
    same :: (Eq a) => Int -> a -> [a] -> Bool
    same i e row = row !! i == e

oxygenGenRating :: (Ord a) => [[a]] -> [a]
oxygenGenRating xs = rating xs 0 id

co2ScrubberRating :: (Ord a) => [[a]] -> [a]
co2ScrubberRating xs = rating xs 0 not

column :: [[a]] -> Int -> [a]
column str n = go str n []
  where
    go :: [[a]] -> Int -> [a] -> [a]
    go [] _ acc = acc
    go (h:t) idx acc =
      let a    = h !! idx
          acc' = a:acc    
      in go t idx acc' 

