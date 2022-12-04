module D2 where

main :: IO()
main = interact $ show . solve' .  lines

solve' :: [String] -> Integer
solve' xs = solve2 xs 0

-- first part
solve1 :: [String] -> Integer -> Integer
solve1 [] n = n
solve1 (x:xs) n = case x of
  "A X" ->  solve1 xs (n+4)
  "A Y" ->  solve1 xs (n+8)
  "A Z" ->  solve1 xs (n+3)
  "B X" ->  solve1 xs (n+1)
  "B Y" ->  solve1 xs (n+5)
  "B Z" ->  solve1 xs (n+9)
  "C X" ->  solve1 xs (n+7)
  "C Y" ->  solve1 xs (n+2)
  "C Z" ->  solve1 xs (n+6)

--second part
solve2 :: [String] -> Integer -> Integer
solve2 [] n = n
solve2 (x:xs) n = case x of
  "A X" ->  solve2 xs (n+3)
  "A Y" ->  solve2 xs (n+4)
  "A Z" ->  solve2 xs (n+8)
  "B X" ->  solve2 xs (n+1)
  "B Y" ->  solve2 xs (n+5)
  "B Z" ->  solve2 xs (n+9)
  "C X" ->  solve2 xs (n+2)
  "C Y" ->  solve2 xs (n+6)
  "C Z" ->  solve2 xs (n+7)

-- cabal run adventoofcode2022 < input/inputD2.txt
