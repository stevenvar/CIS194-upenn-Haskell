{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f mx = mx >>= \x -> return (f x)

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV x y v =  liftM2 (\vx vy -> v // [(x,vy),(y,vx)]) (v !? x) (v !?y)

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM _ [] = return []
mapM f (x:xs) = liftM2 (:) (f x) (mapM f xs) 

getElts :: [Int] -> Vector a -> Maybe [a]
getElts il v = mapM (\x -> (v !? x)) il 

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v = (\x -> v !? x) <$> getRandomR (0, V.length v)  

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = V.fromList <$> replicateM n getRandom

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n c = V.fromList <$> replicateM n (getRandomR c)

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle v = aux (V.length v - 1) v
  where aux 0 acc = return acc
        aux i acc = getRandomR (0,i) >>= (\j -> aux (i-1) (swapUnsafe i j acc))
          where
            swapUnsafe x y v' = (\vx vy -> v' // [(x,vy),(y,vx)]) (v' ! x) (v' ! y) 
  

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt v i = (V.filter (< p) v' , p ,  V.filter (>=p) v')
  where p = (v ! i)
        v' = V.ifilter (\j _ -> i /= j) v 

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a

qsort v
  | v == V.empty = V.empty
  | otherwise = let (l,p,r) = partitionAt v 0 in
    (qsort l) V.++ (V.cons p (qsort r))

-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR v
  | V.null v = return V.empty
  | otherwise = getRandomR (0, V.length v -1) >>= \i -> lift (partitionAt v i)
  where lift (l,p,r) = liftM2 (V.++) (qsortR l) (liftM2 (V.cons) (return p) (qsortR r))

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select n v = getRandomR (0, V.length v-1) >>= \i -> aux n (partitionAt v i)
  where aux i (l,_,r)
          | i < (V.length l) = select i l
          | i == (V.length l) = return (v !? i)
          | otherwise = select (i - (V.length l) - 1) r
            
-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = [ Card l s | s <- suits , l <- labels  ] 

newDeck :: Rnd Deck
newDeck =  shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard d
  | V.null d = Nothing
  | otherwise = Just (V.head d, V.tail d) 

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards n d = aux n d []
  where
    aux 0 d' acc = Just (reverse acc,d')
    aux n' d' acc = nextCard d' >>= (\(x,y) -> aux (n'-1) y (x:acc))

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty 

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
