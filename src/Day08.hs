-- Day :8 Seven Segment Search -- 
-- https://adventofcode.com/2021/day/8
module Day08 (solve) where

import Data.List
import Data.List.Split
import Data.Function

type UniquePattern = [String]
type FourDigitOutput = [String]

parseLine :: String -> (UniquePattern, FourDigitOutput)
parseLine l = case splitOn "|" l of
                u:f:[] -> (words u, words f)
                _ -> error "Not a nicely formatted line"

parse :: String -> [(UniquePattern, FourDigitOutput)]
parse s = map parseLine $ lines s

countEasyDigits :: FourDigitOutput -> Int
countEasyDigits [] = 0
countEasyDigits (x:xs) = case length x of
                         l
                            | l `elem` [2,3,4,7] -> 1 + countEasyDigits xs
                            | otherwise -> countEasyDigits xs

part1 :: String -> Int
part1 input = sum $ map countEasyDigits $ map snd (parse input)

-- Part 2 --

sortByLength :: UniquePattern -> UniquePattern
sortByLength = sortBy (compare `on` length)

isDigit3 :: String -> String -> Bool
isDigit3 digit1 x = length (x `intersect` digit1) == 2

isDigit5 :: String -> String -> Bool
isDigit5 digit4 x = length (x `intersect` digit4) == 3 

isDigit9 :: String -> String -> Bool
isDigit9 digit4 x = length (x `intersect` digit4) == 4

-- only if 9 is already ruled out
isDigit0 :: String -> String -> Bool
isDigit0 digit1 x = length (x `intersect` digit1) == 2

valueFromDigitOutput :: UniquePattern -> String -> Int
valueFromDigitOutput u s = case length s of 
                             2 -> 1
                             3 -> 7
                             4 -> 4
                             7 -> 8
                             5 -> valueFrom5LongOutput u s
                             6 -> valueFrom6LongOutput u s
                             _ -> error "Invalid"

-- use 1 & 4 strings to help figure out what number we have in 5 and 6 char cases
-- since we sort u (head u) is digit1 and (u !! 2) is digit4
valueFrom5LongOutput :: UniquePattern -> String -> Int
valueFrom5LongOutput u s = case isDigit3 (head u) s of
                             True -> 3
                             False -> case isDigit5 (u !! 2) s of
                                        True -> 5
                                        False -> 2

valueFrom6LongOutput :: UniquePattern -> String -> Int
valueFrom6LongOutput u s = case isDigit9 (u !! 2) s of
                             True -> 9
                             False -> case isDigit0 (head u) s of
                                        True -> 0
                                        False -> 6

valueFromFourDigitOutput :: UniquePattern -> FourDigitOutput -> Int
valueFromFourDigitOutput u f@(f0:f1:f2:f3:[]) = 1000*(valueFromDigitOutput u f0) + 100*(valueFromDigitOutput u f1) + 10*(valueFromDigitOutput u f2) + (valueFromDigitOutput u f3)
valueFromFourDigitOutput _ _ = error "Four digits please"
 

part2 :: String -> Int
part2 input = sum $ map  (\(u,f) -> valueFromFourDigitOutput (sortByLength u) f) (parse input)

solve :: String -> IO ()
solve input = putStrLn "--- Day 08 ---" >> print (part1 input) >> print (part2 input)
