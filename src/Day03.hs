-- https://adventofcode.com/2021/day/3
module Day03 (solve) where

part1 :: String -> String
part1 _ = "Day 3 part 1"

part2 :: String -> String
part2 s = "Day 3 part 2" ++ s

solve :: String -> IO ()
solve input = putStrLn "--- Day 03 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
