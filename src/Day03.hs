-- Day 3: Binary Diagnostic --
-- https://adventofcode.com/2021/day/3

module Day03 (solve) where
import Data.List ( transpose )

-- get the char that's most common ('0' or '1')
mostCommon :: String -> Char
mostCommon s
  | zeros > ones  = '0'
  | ones > zeros  = '1'
  | otherwise     = error "Equal amount of 0s and 1s"
  where zeros = length $ filter (=='0') s
        ones  = length $ filter (=='1') s

-- invert string of 0s and 1s
binInvert :: String -> String
binInvert []        = ""
binInvert ('0':xs)  = '1' : binInvert xs
binInvert ('1':xs)  = '0' : binInvert xs

-- convert string of 0s and 1s representing a binary value to int
binToInt :: String -> Int
binToInt s = sum $ map (\(i,v) -> v * 2^i) indexed
  where indexed = zip [0..] $ map (\c -> if c == '0' then 0 else 1) $ reverse s


part1 :: [String] -> Int
part1 s = do
  let mosts   = map mostCommon $ transpose s
  let gamma   = binToInt mosts
  let epsilon = binToInt $ binInvert mosts
  gamma * epsilon

-- Part 2 --

-- position of bit to consider, and list of string representations of binary numbers
-- filter the list according to criteria, until there's only 1 left
oxygenRating :: Int -> [String] -> String
oxygenRating _ [s] = s
oxygenRating i ss = oxygenRating(i+1) $ filter (\s -> s!!i == most) ss
    where bits    = map (!!i) ss
          zeros   = length $ filter (=='0') bits
          ones    = length $ filter (=='1') bits
          most    | zeros > ones  = '0'
            | otherwise     = '1'

-- position of bit to consider, and list of string representations of binary numbers
-- filter the list according to criteria, until there's only 1 left
co2Rating :: Int -> [String] -> String
co2Rating _ [s] = s
co2Rating i ss  = co2Rating (i+1) $ filter (\s -> s!!i == least) ss
  where bits    = map (!!i) ss
        zeros   = length $ filter (=='0') bits
        ones    = length $ filter (=='1') bits
        least   | ones < zeros  = '1'
                | otherwise     = '0'

part2 :: [String] -> Int
part2 s = binToInt (oxygenRating 0 s) * (binToInt $ co2Rating 0 s)

solve :: String -> IO ()
solve input = putStrLn "--- Day 03 ---" >> print (part1 (lines input)) >> print (part2 (lines input))
