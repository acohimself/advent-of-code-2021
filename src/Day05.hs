-- Day 5: Hydrothermal Venture --
-- https://adventofcode.com/2021/day/5

module Day05 (solve) where

import Data.List.Split

type Point = (Int, Int)
type Line = (Point, Point)

parsePoint :: String -> Point
parsePoint s = case  splitOn "," s of
                 x:y:[] -> (read x,read y)
                 _ -> error "Cannot parse point"

parseLine :: String -> Line
parseLine s = case splitOn " -> " s of
                 x:y:[] -> (parsePoint x,parsePoint y)
                 _ -> error "Cannot parse line"
                

parse :: String -> [Line]
parse s = map parseLine $ lines s


-- my naive idea is to convert lines into the list of points they go tru
transformVertical :: Line -> [Point]
transformVertical ((x1,y1),(x2,y2)) = map (\y -> (x1, y)) [(min y1 y2)..(max y1 y2)] 

transformHorizontal :: Line -> [Point]
transformHorizontal ((x1,y1),(x2,y2)) = map (\x -> (x, y1)) [(min x1 x2)..(max x1 x2)]

transformLineToPoints :: Line -> [Point]
transformLineToPoints l@((x1,y1),(x2,y2)) = case x2-x1 of 
                                            0 -> transformVertical l
                                            _ -> case y2-y1 of
                                                   0 -> transformHorizontal l
                                                   _ -> []

-- in list of points when all lines are transformed, we want to get the list of duplicates
findDuplicates :: [Point] -> [Point]
findDuplicates ((x,y):ps) = case filter (==(x,y)) ps of 
                                  [] -> findDuplicates ps
                                  _ -> (x,y):findDuplicates (filter (not . (==(x,y))) ps) 
findDuplicates _ = []

part1 :: String -> Int
part1 input = length $ findDuplicates $ concat $ map transformLineToPoints $ parse input

-- Part 2 --
transformDiagonal :: Line -> [Point]
transformDiagonal l@((x1,y1), (x2,y2)) = case (x1<x2, (y2-y1) `div` (x2-x1))  of
                                           (True, 1) -> map (\x -> (x, y1 + (x-x1)   )) [x1..x2]
                                           (True, -1) -> map (\x -> (x, y1 - (x-x1)   )) [x1..x2]
                                           (False, 1) ->  map (\x -> (x, y2 + (x-x2) )) [x2..x1]
                                           (False, -1) ->  map (\x -> (x, y2 - (x-x2) )) [x2..x1]

transformLineToPoints2 :: Line -> [Point]
transformLineToPoints2 l@((x1,y1),(x2,y2)) = case x2-x1 of 
                                            0 -> transformVertical l
                                            _ -> case y2-y1 of
                                                   0 -> transformHorizontal l
                                                   _ -> transformDiagonal l


part2 :: String -> Int
part2 input = length $ findDuplicates $ concat $ map transformLineToPoints2 $ parse input

solve :: String -> IO ()
solve input = putStrLn "--- Day 05 ---" >> print (part1 input) >> print (part2 input)
