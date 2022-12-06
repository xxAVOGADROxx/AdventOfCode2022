module D6 where

main :: IO ()
main = interact $ show . solve4 4 --solve4 4 or solve14 14

findRep :: String -> Bool
findRep [] = False
findRep (x : xs)
  | any f xs = True
  | otherwise = findRep xs
  where
    f y = x == y

--first part
solve4 :: Int -> String -> Int
solve4 n xs
  | findRep (take 4 xs) = solve4 (n + 1) (tail xs)
  | otherwise = n

--second part
solve14 :: Int -> String -> Int
solve14 n xs
  | findRep (take 14 xs) = solve14 (n + 1) (tail xs)
  | otherwise = n
