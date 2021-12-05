{-# LANGUAGE TypeApplications #-}
module Day02 where

import Utils
import Data.Foldable (fold)
 
data Command =
    Forward Int
  | Up Int
  | Down Int
  deriving (Read, Show)  

data Position = Position
  { horizontal :: Int
  , depth :: Int
  , aim :: Int
  } deriving (Show)

instance Monoid Position where
  mempty = Position 0 0 0

instance Semigroup Position where
  (Position x1 y1 _) <> (Position x2 y2 _) = Position (x1 + x2) (y1 + y2) 0

solutions :: IO ()
solutions = do
 commands <- load @Command "./day02.txt"

 let solution1 = let (Position h d _) = fold $ toPosition <$> commands in h * d
     solution2 = let (Position h d _) = foldl (flip nextPosition) mempty commands in h * d

 print solution1
 print solution2

 
toPosition :: Command -> Position
toPosition (Up n) = Position 0 (negate n) 0
toPosition (Down n) = Position 0 n 0
toPosition (Forward n) = Position n 0 0

nextPosition :: Command -> Position -> Position
nextPosition (Up n) (Position h d a) = Position h d (a - n) 
nextPosition (Down n) (Position h d a) = Position h d (a + n) 
nextPosition (Forward n) (Position h d a) = Position (h + n) (d + n * a) a 


