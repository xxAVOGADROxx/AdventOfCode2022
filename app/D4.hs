module D4 where

main :: IO()
main = interact $ show . solve1 . lines

cleanInput :: String -> [Integer]
cleanInput = map read . words . map repl
  where
   repl '-' = ' '
   repl ',' = ' '
   repl a   = a

-- first part
solve1 :: [String] -> Integer
solve1 xs = sum $ r <$> verification1 xs
 where
   r a = if a then 1 else 0

verification1 :: [String] -> [Bool]
verification1  xs = isElem . cleanInput <$> xs
  where
    isElem :: [Integer] -> Bool
    isElem  (x:y:r:xs)  =
      r `elem` [x .. y] && head xs `elem` [x .. y] ||
      x `elem` [r .. head xs] && y `elem` [r .. head xs]

-- second part
solve2 :: [String] -> Integer
solve2 xs = sum $ r <$> verification2 xs
 where
   r a = if a then 1 else 0

verification2 :: [String] -> [Bool]
verification2  xs = isElem . cleanInput <$> xs
  where
    isElem :: [Integer] -> Bool
    isElem  (x:y:r:xs)  =
      r `elem` [x .. y] || head xs `elem` [x .. y] ||
      x `elem` [r .. head xs] || y `elem` [r .. head xs]
