module D5 where

import Data.List

main :: IO ()
main = interact $ takeTop . journey ship . drop 10 . lines

newtype Matrix = Matrix [[Char]]
  deriving (Show)

ship :: Matrix
ship =
  Matrix
    [ ['H', 'R', 'B', 'D', 'Z', 'F', 'L', 'S'],
      ['T', 'B', 'M', 'Z', 'R'],
      ['Z', 'L', 'C', 'H', 'N', 'S'],
      ['S', 'C', 'F', 'J'],
      ['P', 'G', 'H', 'W', 'R', 'Z', 'B'],
      ['V', 'J', 'Z', 'G', 'D', 'N', 'M', 'T'],
      ['G', 'L', 'N', 'W', 'F', 'S', 'P', 'Q'],
      ['M', 'Z', 'R'],
      ['M', 'C', 'L', 'G', 'V', 'R', 'T']
    ]

move :: Matrix -> Int -> Int -> Int -> Matrix
move (Matrix m) q i o = Matrix $ replace (replace m (o - 1, nStake)) (i -1, mStake)
  where
    nCut = take q . reverse $ m !! (i - 1)
    mStake = reverse . drop q . reverse $ m !! (i -1)
    nStake = m !! (o - 1) ++ reverse nCut

-- For first part delete reverse in the line above

takeTop :: Matrix -> String
takeTop (Matrix m) = map last' m
  where
    last' [] = ' '
    last' a = last a

cleanInput :: String -> (Int, Int, Int)
cleanInput xs = f $ words xs
  where
    f ys = (read $ ys !! 1, read $ ys !! 3, read $ ys !! 5)

journey :: Matrix -> [String] -> Matrix
journey m xs = foldl' f m road
  where
    road = map cleanInput xs
    f m (a, b, c) = move m a b c

replace :: [[Char]] -> (Int, [Char]) -> [[Char]]
replace [] _ = []
replace (_ : xs) (0, a) = a : xs
replace (x : xs) (n, a) =
  if n < 0
    then x : xs
    else x : replace xs (n -1, a)
