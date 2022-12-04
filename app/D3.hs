module D3 where

main :: IO()
main = interact $ show . solve2' . lines

priorityList :: [(Char, Integer)]
priorityList = zip ['a' .. ] [1 .. 26] ++ zip ['A' .. ] [27 .. 52]

--first part
solve1' :: [String] -> Integer
solve1' xs = sum $ solve1 <$> xs

solve1 :: String -> Integer
solve1 xs = head [priority i | i <- a, j <- b, i==j]
  where
    (a, b) = splitAt middle xs
    middle = length xs `div` 2
    priority y = case lookup y priorityList of
      Just p  -> p
      Nothing -> error "priority not found"

--second part
solve2' :: [String] -> Integer
solve2' xs = solve2 xs []

solve2 :: [String] -> [Integer] -> Integer
solve2 [] ps      = sum ps
solve2 (a:b:c:ds) ps =  solve2 ds (priority : ps)
  where
    badge  =  head [ i | i <- a, j <- b, z <- c, i == j && j == z]
    priority = case lookup badge priorityList of
      Just p  -> p
      Nothing -> error "priority not found"
