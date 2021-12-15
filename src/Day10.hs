-- Day 10: Syntax Scoring --
-- https://adventofcode.com/2021/day/10

module Day10 (solve) where

import Data.Maybe
import Data.List

-- part1

-- parse or string into a list of first corrupted chars, Nothing if invalid
parse1 :: String -> [Maybe Char]
parse1 input = map parseChunk $ lines input

parseChunk :: [Char] -> Maybe Char
parseChunk s = findCorrupted s [] 

findCorrupted :: [Char] -> [Char] -> Maybe Char
findCorrupted [] stack = Nothing
findCorrupted (x:xs) [] = findCorrupted xs [x]
findCorrupted (x:xs) (y:ys)  = if x `elem` "([{<" then findCorrupted xs (x:y:ys) -- if we see a valid opener, place it on the stack
                                             else if (x == ')' && y == '(') ||  
                                                     (x == ']' && y == '[') || -- if correct matches, remove the pair 
                                                     (x == '}' && y == '{') ||
                                                     (x == '>' && y == '<') then findCorrupted xs ys 
                else if x `elem` ")]}>" then Just x 
                else Nothing

-- score our list corrupted chars
scoreSyntax :: [Maybe Char] -> Int
scoreSyntax [] = 0
scoreSyntax (c:cs) = case c of
                       Nothing -> scoreSyntax cs
                       Just ')' -> 3 + scoreSyntax cs
                       Just ']' -> 57 + scoreSyntax cs
                       Just '}' -> 1197 + scoreSyntax cs
                       Just '>' -> 25137 + scoreSyntax cs

part1 :: String -> Int
part1 input = scoreSyntax $ parse1 input

-- Part 2 --
parse2 :: String -> [Maybe String]
parse2 input = map (\l -> autocomplete l []) $ lines input

autocomplete :: [Char] -> [Char] -> Maybe String
autocomplete [] stack = Just (matchWithClosing stack)
autocomplete (x:xs) [] = autocomplete xs [x]
autocomplete (x:xs) (y:ys)  = if x `elem` "([{<" then autocomplete xs (x:y:ys) -- if we see a valid opener, place it on the stack
                                             else if (x == ')' && y == '(') ||  
                                                     (x == ']' && y == '[') || -- if correct matches, remove the pair 
                                                     (x == '}' && y == '{') ||
                                                     (x == '>' && y == '<') then autocomplete xs ys 
                                                     else Nothing

matchWithClosing :: String -> String
matchWithClosing [] = []
matchWithClosing (c:cs) = case c of 
                            '(' -> ')' : matchWithClosing cs
                            '[' -> ']' : matchWithClosing cs
                            '{' -> '}' : matchWithClosing cs
                            '<' -> '>' : matchWithClosing cs

scoreAutocomplete :: [Maybe String] -> [Int]
scoreAutocomplete [] = [] 
scoreAutocomplete (s:ss) = case s of
                             Nothing -> scoreAutocomplete ss
                             Just x -> (scoreString 0 $ x):scoreAutocomplete ss

scoreString :: Int -> String -> Int
scoreString acc [] = acc
scoreString acc (c:cs) = scoreString (acc*5 + charScore c) cs

charScore :: Char -> Int 
charScore ')' = 1
charScore ']' = 2
charScore '}' = 3
charScore '>' = 4
charScore _ = error "Bad char"

part2 :: String -> Int
part2 input = let scores = scoreAutocomplete $ parse2 input
               in sort scores !! (length scores `div` 2)


solve :: String -> IO ()
solve input = putStrLn "--- Day 10 ---" >> print (part1 input) >> print (part2 input)
