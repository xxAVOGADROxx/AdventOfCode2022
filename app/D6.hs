module D6 where

main :: IO ()
main = interact $ show . solve'
  where
    solve' x = solve4 x 4 -- change 4 or 14

repeat' :: String -> Bool
repeat' [] = False
repeat' (x : xs)
  | any f xs = True
  | otherwise = repeat' xs
  where
    f y = x == y

--first part
solve4 :: String -> Int -> Int
solve4 xs n
  | repeat' (take 4 xs) = solve4 (tail xs) (n + 1)
  | otherwise = n

--second part
solve14 :: String -> Int -> Int
solve14 xs n
  | repeat' (take 14 xs) = solve14 (tail xs) (n + 1)
  | otherwise = n
