{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches c1 c2 = length . filter (\(x,y) -> x == y) $ zip c1 c2

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors c = map (\x -> countColor x c) colors
  where countColor col code = length $ filter (\x -> x == col) code

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches c1 c2 = sum $ zipWith (\x y -> min x y) (countColors c1) (countColors c2) 

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess exact nonexact
  where exact = exactMatches secret guess
        nonexact = matches secret guess - exact

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move guess e n) secret = e == exact && n == nonexact
  where Move _ exact nonexact = getMove secret guess


-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes m c = filter (isConsistent m) c

-- Exercise 6 -----------------------------------------



allCodes :: Int -> [Code]
allCodes 0 = [] 
allCodes 1 = map (\c -> [c]) colors  
allCodes n = concatMap (\c -> code' c) (allCodes (n-1))
  where code' c = map (\col -> col:c) colors 



-- Exercise 7 -----------------------------------------


solve :: Code -> [Move]
solve secret = solve' (allCodes (length secret))
  where solve' [] = []
        solve' (c:cl) = m : solve' (filterCodes m cl) 
          where m = getMove secret c


-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
