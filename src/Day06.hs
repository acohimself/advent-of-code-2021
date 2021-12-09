-- Day 6: Lanternfish --
-- https://adventofcode.com/2021/day/6
module Day06 (solve) where

import Data.List.Split

parse :: String -> [Int]
parse s = map read (splitOn "," s)

-- a day for a single fish will either reduce its number by 1 or produce a new 8 and reset to 6
dayForFish :: Int -> [Int]
dayForFish 0 = [6, 8]
dayForFish x = [x-1]

-- simulate a number of days 
simulate :: Int -> [Int] -> [Int]
simulate 0 xs = xs
simulate days xs = simulate (days-1) (concat $ map dayForFish xs)

part1 :: String -> Int
part1 input = length $ simulate 80 (parse input)

-- Part 2 --

-- all fishs with same vales can be stored in tuple (days, numberOfFish)
type FishWithXDays = (Int, Int)

prepareModel :: [Int] -> [FishWithXDays]
prepareModel ds = map (\x -> (x, length (filter (==x) ds))) [0..6]

dayForFish2 :: FishWithXDays -> [FishWithXDays] 
dayForFish2 (0, x) = [(8, x), (6, x)]
dayForFish2 (days, x) = [(days-1, x)]

combineDays :: [FishWithXDays] -> [FishWithXDays]
combineDays [] = []
combineDays ((days, number):dayss) = case (filter ((==days).fst) dayss) of
                                     [] -> (days, number) : combineDays dayss
                                     xs -> (days, foldl (\y1 (_,y2) -> y1+y2) number xs) : combineDays (filter ((not.(==days)).fst) dayss) 

simulate2 :: Int -> [FishWithXDays] -> [FishWithXDays]
simulate2 0 xs = xs
simulate2 days xs = simulate2 (days-1) (combineDays $ concat $ map dayForFish2 xs)

sumFishModel :: [FishWithXDays] -> Int
sumFishModel [] = 0
sumFishModel ((days, number):fs) = number + sumFishModel fs 

part2 :: String -> Int
part2 input = sumFishModel $ simulate2 256 (prepareModel (parse input))

solve :: String -> IO ()
solve input = putStrLn "--- Day 06 ---" >> print (part1 input) >> print (part2 input)
