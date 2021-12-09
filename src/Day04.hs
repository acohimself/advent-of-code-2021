-- Day 4: Giant Squid --
-- https://adventofcode.com/2021/day/4
module Day04 (solve) where

import Data.List.Split
import Data.List
import Data.Maybe

type BingoRow = [Maybe Int]
type BingoBoard = [BingoRow]

parseInput :: String -> ([Int], [BingoBoard]) 
parseInput s = 
    case splitOn "\n\n" s of
      [] -> error "Invalid input"
      drawInput : boardInput -> (parseDraw drawInput, parseBoards boardInput)

parseDraw :: String -> [Int]
parseDraw s = map read (splitOn "," s)

parseBoard :: String -> BingoBoard
parseBoard ss = map parseRow (lines ss)

parseBoards :: [String] -> [BingoBoard]
parseBoards ss = map parseBoard ss

parseRow :: String -> BingoRow
parseRow s = map (Just . read) (words s)

-- check for bingo in a board
-- check each row + transpose the board and check each row ie column.
checkForBingo :: BingoBoard -> Bool
checkForBingo b = any (==True) $ (map checkRowForBingo b) ++ (map checkRowForBingo $ transpose b)

checkRowForBingo :: BingoRow -> Bool
checkRowForBingo r = all isNothing r

-- mark numbers on a row or board with Nothing.
markRow :: Int -> BingoRow -> BingoRow
markRow x r = map (\y -> case (y == Just x) of 
                             True -> Nothing 
                             otherwise -> y) r
 
mark :: Int -> BingoBoard -> BingoBoard
mark x b = map (markRow x) b

-- add all unmarked (not Nothing) numbers in a board.
boardScore :: BingoBoard -> Int
boardScore b = foldr (\x y -> case x of {Nothing -> y; Just z -> z+y}) 0 $ concat b

-- from a list of marked boards, return the ones with bingo.
getWinnerBoards :: [BingoBoard] -> [BingoBoard]
getWinnerBoards (b:bs) = case checkForBingo b of
                           True -> b:getWinnerBoards bs
                           False -> getWinnerBoards bs
getWinnerBoards [] = []

-- play by marking one number at a time, and check for winners after each round of marking.
-- return the first winning board and the winning number.
play :: ([Int], [BingoBoard]) -> (BingoBoard, Int)
play (d:draw, boards) = let markedBoards = map (mark d) boards
                         in case getWinnerBoards markedBoards of
                              [] -> play(draw, markedBoards)
                              (b:_) -> (b, d)


part1 :: String -> Int
part1 input = let (winnerBoard, winnerNumber) = play (parseInput input) 
               in winnerNumber * boardScore winnerBoard
               
-- Part 2 --
removeWinnerBoards :: [BingoBoard] -> [BingoBoard]
removeWinnerBoards (b:bs) = case checkForBingo b of
                              True -> removeWinnerBoards bs
                              False -> b:removeWinnerBoards bs
removeWinnerBoards [] = []

-- keep on removing winning boards until one is left. 
-- then do normal play on that board in order to find the winning number.
play2 :: ([Int], [BingoBoard]) -> (BingoBoard, Int)
play2 (d:draw, boards) = let markedBoards = map (mark d) boards
                         in case removeWinnerBoards markedBoards of
                              finale@(lastWinner:[]) -> play (draw, finale)
                              (b:_) ->  play2(draw, markedBoards)
                              [] -> error ("Multiple last winners")

part2 input = let (lastWinner, winnerNumber) = play2 (parseInput input)
           in winnerNumber * boardScore lastWinner

solve :: String -> IO ()
solve input = putStrLn "--- Day 04 ---" >> print (part1 input) >> print (part2 input)
