-- Day 11: Dumbo Octopus --
-- https://adventofcode.com/2021/day/11

module Day11 (solve) where
import Data.Map (Map, fromList)
import qualified Data.Map as Map (map, mapAccumWithKey, mapAccum, foldr, adjust)

type Position = (Int, Int)
-- map of energy value and indicator of if it just flashed
type OctopusEnergies = Map Position (Int, Bool)


-- part1

parse :: String -> OctopusEnergies
parse s = fromList $ concat
                   $ map parseRows
                   $ zip (map (map (read . (:""))) $ lines s) [1..]

parseRows (row, y) = map (parseCols y) $ zip row [1..]

parseCols y (energy, x) = ((x, y), (energy, False))

-- get position of all neighbours
neighbours :: Position -> [Position]
neighbours (x, y) = [(x-1,y-1), (x,y-1), (x+1,y-1), (x-1, y), (x+1,y), (x-1,y+1), (x,y+1), (x+1,y+1)]

-- find number of flashed after x steps
step :: Int -> OctopusEnergies -> Int
step 1 o = fst(flash (allPositions, o))
step x o = let (flashes, newEnergies) = flash (allPositions, o)
            in flashes + (step (x-1) newEnergies)

-- grid of all positions, for initial input to flash, which takes a list of positions
allPositions :: [Position]
allPositions = [(x, y)| x<- [1..10], y<-[1..10]]

-- increment energy in the positions and return resulting energy map
incrementPositions :: ([Position], OctopusEnergies) -> OctopusEnergies
incrementPositions ([], o) = o
incrementPositions ((p:ps), o) = incrementPositions (ps, Map.adjust (\(b,f) -> (b+1,f)) p o)

-- after increasing and flashing away, this will reset flashed with (0, False) and return resulting energies
-- couning number of resets, to keep track of number of flashes 
resetFlashed :: OctopusEnergies -> (Int, OctopusEnergies)
resetFlashed o = Map.mapAccum (\a (e,f) -> if f == True then (a+1, (0, False)) else (a, (e,f))) 0 o

-- increase all pos in list. recursive of any increments resulted in a new list of pos to increment
-- finally return number of flashes and resulting energies when no more pos needs increasing 
flash :: ([Position], OctopusEnergies) -> (Int, OctopusEnergies)
flash ([], o) = resetFlashed o
flash (ps, o) = flash $ Map.mapAccumWithKey (\a (x,y) (e,f) -> if (e > 9 && f == False) then (a++neighbours (x,y), (e, True)) else (a, (e,f))) [] $ incrementPositions (ps,o)
 

part1 :: String -> Int
part1 input = step 100 $ parse input

-- Part 2 --

-- step until we get 100 flashes, x counts steps 
stepUntilAllFlashes :: Int -> OctopusEnergies -> Int
stepUntilAllFlashes x o = let (flashes, newEnergies) = flash (allPositions, o)
                           in case flashes of
                                100 -> x
                                _ -> stepUntilAllFlashes (x+1) newEnergies


part2 :: String -> Int
part2 input = stepUntilAllFlashes 1 $ parse input

solve :: String -> IO ()
solve input = putStrLn "--- Day 11 ---" >> print (part1 input) >> print (part2 input)
