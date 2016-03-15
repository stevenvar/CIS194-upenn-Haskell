{-# OPTIONS_GHC -Wall #-}
module HW04 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0,1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (P l1) == (P l2) = peq l1 l2
      where peq [] l = all (==0) l 
            peq l [] = all (==0) l
            peq p1 p2 = (all (==0) p1 && all (==0) p2) || (p1 == p2) 
              
              
-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
  show (P l) = drop 3 $ show' ((length l)-1) (reverse l)
      where
        show' _ [] = ""
        show' 0 (0:_) = ""
        show' 0 (h:_) = " + " ++ show h
        show' 1 (-1:t) = " + -x" ++ show' 0 t 
        show' 1 (1:t) = " + x" ++ show' 0 t 
        show' 1 (0:t) = show' 0 t 
        show' 1 (h:t) =  " + " ++ show h ++ "*x" ++ show' 0 t
        show' n (0:t) = show' (n-1) t
        show' n (1:t) = " + x^" ++ (show n) ++ (show' (n-1) t)
        show' n (-1:t) = " + -x^" ++ (show n) ++ (show' (n-1) t)
        show' n (h:t) = " + " ++ (show h) ++"*x^" ++ (show n) ++ (show' (n-1) t)
          

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P l1) (P l2) = P (plus' l1 l2)
  where plus' (h1:t1) (h2:t2) = h1 + h2 : plus' t1 t2
        plus' [] l = l
        plus' l [] = l

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P l1) (P l2) = foldr (+) (P [0]) (times' l1 l2)
  where times' [] _ = [P []]
        times' _ [] = [P []]
        times' (h1:t1) p2 = P (map (*h1) p2)  : times' t1 (0:p2)

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P l) = P (map (\y -> -y) l)
    fromInteger i = P [(fromInteger i)] 
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P p) n = apply' n (length p -1) (reverse p) 0 
  where apply' _ _ [] acc = acc
        apply' v e (h:t) acc = apply' v (e-1) t (h*v^e + acc) 




-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 0 v = v
    nderiv n v = nderiv (n-1) (deriv v)

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv (P p)= P (drop 1 $ (deriv' 0 p))
      where
        deriv' _ [] = []
        deriv' e (c:t) = c*e : deriv' (e+1) t
