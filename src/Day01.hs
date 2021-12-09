-- Day 1: Sonar Sweep --
-- https://adventofcode.com/2021/day/1
module Day01 (solve) where

-- part1
findIncreases :: [Int] -> Int
findIncreases [] = 0
findIncreases [_] = 0
findIncreases (x:y:xs)
  | x<y = 1 + findIncreases (y:xs)
  | otherwise = findIncreases (y:xs)


part1 :: String -> Int
part1 input = findIncreases $ (map read . lines) input

-- part2
findIncreases2 :: [Int] -> Int
findIncreases2 (x1:x2:x3:x4:xs)
  | x1 < x4 = 1 + findIncreases2 (x2:x3:x4:xs)
  | otherwise = findIncreases2 (x2:x3:x4:xs)
findIncreases2 _ = 0

part2 :: String -> Int
part2 input = findIncreases2 $ (map read . lines) input

solve :: String -> IO ()
solve input = putStrLn "--- Day 01 ---" >> print (part1 input) >> print (part2 input)
