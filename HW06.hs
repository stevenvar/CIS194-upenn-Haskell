{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List


-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2) 

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons x s) = (x:streamToList s)  

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons x s) = (Cons (f x) (fmap f s))   

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = Cons x (sRepeat x)

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons x (sIterate f (f x)) 

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x s) s' = Cons x (sInterleave s' s) 

sTake :: Int -> Stream a -> [a]
sTake n s = take n $ streamToList s

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 0

ruler :: Stream Integer
ruler = sInterleave (sRepeat 0) (fmap (+1) ruler)
  

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand= sIterate (\x -> (1103515245 * x + 12345) `mod` 2147483648)

-- Exercise 8 -----------------------------------------

{- Total Memory in use: 235 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: 1 MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax (x:xs) = aux xs (x,x)
  where aux [] acc  = Just acc
        aux (y:ys) (xmin,xmax) =
          let nmin = min xmin y
              nmax = max xmax y
          in
          nmin `seq` nmax `seq` aux ys $! (nmin,nmax)
  
main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

fastFib :: Int -> Integer
fastFib = undefined
