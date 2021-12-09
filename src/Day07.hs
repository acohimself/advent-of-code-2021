-- Day 7: The Treachery of Whales --
-- https://adventofcode.com/2021/day/7
module Day07 (solve) where

import Data.List.Split

parse :: String -> [Int]
parse s = map read $ splitOn "," s

-- part1

fuelUsageWithPosition :: Int -> [Int] -> Int
fuelUsageWithPosition p xs = sum $ map (\x -> abs(x-p)) xs

-- minimize by going in the direction of a better solution. If in min, return value
minimizeSolution :: Int -> [Int] -> Int
minimizeSolution guess xs = let fuelDown = fuelUsageWithPosition (guess -1) xs
                                currentFuel = fuelUsageWithPosition guess xs
                             in case fuelDown < currentFuel of
                                  True -> minimizeSolution (guess-1) xs 
                                  False -> case currentFuel > fuelUsageWithPosition (guess+1) xs of
                                             True -> minimizeSolution (guess+1) xs
                                             False -> currentFuel

-- use avg as initial guess
averageValue :: [Int] -> Int
averageValue xs = sum (xs) `div` length xs


part1 :: String -> Int
part1 input = let positions = parse input in
                  minimizeSolution (averageValue positions) positions

-- Part 2 --

fuelUsageWithPosition2 :: Int -> [Int] -> Int
fuelUsageWithPosition2 p xs = sum $ map (\x -> abs(x-p)*(abs(x-p)+1) `div` 2) xs

minimizeSolution2 :: Int -> [Int] -> Int
minimizeSolution2 guess xs = let fuelDown = fuelUsageWithPosition2 (guess -1) xs
                                 currentFuel = fuelUsageWithPosition2 guess xs
                             in case fuelDown < currentFuel of
                                  True -> minimizeSolution (guess-1) xs 
                                  False -> case currentFuel > fuelUsageWithPosition2 (guess+1) xs of
                                             True -> minimizeSolution2 (guess+1) xs
                                             False -> currentFuel

part2 :: String -> Int
part2 input = let positions = parse input in
                  minimizeSolution2 (averageValue positions) positions

solve :: String -> IO ()
solve input = putStrLn "--- Day 07 ---" >> print (part1 input) >> print (part2 input)
