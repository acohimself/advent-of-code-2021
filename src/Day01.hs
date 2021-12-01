-- https://adventofcode.com/2021/day/1
module Day01 (solve) where


part1 :: [Int] -> Int
part1 [] = 0
part1 [_] = 0
part1 (x:y:xs)
  | x<y = 1 + part1 (y:xs)
  | otherwise = part1 (y:xs)


part2 :: [Int] -> Int
part2 (x1:x2:x3:x4:xs)
  | x1 < x4 = 1 + part2 (x2:x3:x4:xs)
  | otherwise = part2 (x2:x3:x4:xs)
part2 _ = 0


solve :: String -> IO ()
solve input = putStrLn "--- Day 01 ---" >> print (part1 $ p input) >> print (part2 $ p input)
    where p = map read . lines
