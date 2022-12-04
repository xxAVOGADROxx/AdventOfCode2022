module D1 where
import           Data.List

main :: IO ()
main = interact $ show .  solve . lines

solve :: [String] -> Integer
solve xs = solve2 xs []

-- first part
solve1 :: [String] -> [Integer] -> Integer
solve1 [] ys = maximum ys
solve1 (x:xs) ys = solve1 (dropWhile (/= "") xs) (subset : ys)
  where
    subset = sum $ map read $  takeWhile (/= "") xs

-- Second part
solve2 :: [String] -> [Integer] -> Integer
solve2 [] ys = sum . take 3 . reverse . sort $ ys
solve2   (x:xs) ys = solve2 (dropWhile (/= "") xs) (subset : ys)
  where
    subset = sum $ map read $ takeWhile (/= "") xs

-- command
-- cabal run adventoofcode2022 < input/inputD1.txt
