-- https://adventofcode.com/2021/day/2
module Day02 (solve) where
import Data.List

subCommands :: String -> (Int, Int)
subCommands s | Just d <- stripPrefix "down " s = (0, read d)
subCommands s | Just u <- stripPrefix "up " s = (0, -read u)
subCommands s | Just f <- stripPrefix "forward " s = (read f, 0)
subCommands _ = (0, 0)


part1 :: [String] -> Int
part1 commands = (\(a, b) -> a*b) (foldl (\(a1, a2) (b1, b2) -> (a1+b1, a2+b2)) (0,0) (map subCommands commands))


part2 :: [String] -> Int
part2 commands = (\(a,b,c) -> a*b) $ foldl (\(horizontal, depth, aim) command -> let (x,y) = subCommands(command) in (horizontal + x, depth + x*aim, aim + y )) (0,0,0) commands


solve :: String -> IO ()
solve input = putStrLn "--- Day 02 ---" >> print (part1 (lines input)) >> print (part2 (lines input))

