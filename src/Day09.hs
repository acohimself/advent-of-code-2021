-- Day 9: Smoke Basin --
-- https://adventofcode.com/2021/day/9

module Day09 (solve) where
import Data.List ( transpose )

parse :: String -> [[Int]]
parse s = map (map (read . (:""))) $ lines s

-- part1

-- sweep three lines (list of int) to find min in the center one
threeLineSweep :: [Int] -> [Int] -> [Int] -> [Int]
-- general case
threeLineSweep (u0:u1:us) (c0:c1:c2:cs) (l0:l1:ls) = case (c0 > c1 && c2 > c1 && u1 > c1 && l1 > c1) of
                                                      True -> c1:threeLineSweep (u1:us) (c1:c2:cs) (l1:ls)
                                                      False -> threeLineSweep (u1:us) (c1:c2:cs) (l1:ls)
-- right edge
threeLineSweep (u0:u1:_) (c0:c1:[]) (l0:l1:_) = case (c0 > c1 && u1 > c1 && l1 > c1) of
                                                      True -> c1:[]
                                                      False -> []
-- upper right corner
threeLineSweep [] (c0:c1:[]) (l0:l1:_) = case (c0 > c1 && l1 > c1) of
                                              True -> c1:[]
                                              False -> []
-- upper edge
threeLineSweep [] (c0:c1:c2:cs) (l0:l1:ls) = case (c0 > c1 && c2 > c1 && l1 > c1) of
                                                      True -> c1:threeLineSweep [] (c1:c2:cs) (l1:ls)
                                                      False -> threeLineSweep [] (c1:c2:cs) (l1:ls)
-- lower right corner
threeLineSweep (u0:u1:_) (c0:c1:[]) [] = case (c0 > c1 && u1 > c1) of
                                              True -> c1:[]
                                              False -> []
-- lower edge
threeLineSweep (u0:u1:us) (c0:c1:c2:cs) [] = case (c0 > c1 && c2 > c1 && u1 > c1) of
                                              True -> c1:threeLineSweep (u1:us) (c1:c2:cs) []
                                              False -> threeLineSweep (u1:us) (c1:c2:cs) []
threeLineSweep _ _ _ = []

-- left pad the lines with 10s to have threeLineSweep work on left edge
leftPad10s :: [[Int]] -> [[Int]]
leftPad10s l = map (\xs -> (10):xs) l

processLines :: [[Int]] -> [Int]
processLines (l0:l1:[]) = threeLineSweep l0 l1 []
processLines (l0:l1:l2:ls) = ((threeLineSweep l0 l1 l2) ++ processLines (l1:l2:ls))

riskValue :: [Int] -> Int
riskValue xs = sum xs + length xs

part1 :: String -> Int
part1 input = let linesOfInts = leftPad10s $ parse input
               in riskValue( processLines ([]:linesOfInts))

-- Part 2 --
part2 :: String -> Int
part2 input = 0

solve :: String -> IO ()
solve input = putStrLn "--- Day 09 ---" >> print (part1 input) >> print (part2 input)
