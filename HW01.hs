toDigits :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]

toDigitsRev x
  | x <= 0 = []
  | otherwise = x `mod` 10 : toDigitsRev (x `div` 10)

toDigits x = reverse (toDigitsRev x)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther' :: [Integer] -> [Integer]

doubleEveryOther' [] = []
doubleEveryOther' [x] = [x]
doubleEveryOther' (x1:x2:xs) = x1:(x2*2):(doubleEveryOther' xs)

doubleEveryOther x = reverse (doubleEveryOther' (reverse x))

sumDigits :: [Integer] -> Integer
 
sumDigits [] = 0
sumDigits (x:xs) 
  | x < 10 = x + (sumDigits xs)
  | otherwise =  sumDigits (toDigits x) + (sumDigits xs)

validate :: Integer -> Bool

validate x  = (sumDigits ( doubleEveryOther (toDigits x))) `mod` 10  == 0 

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a,b)]
hanoi n a b c  = hanoi (n-1) a c b ++  hanoi 1 a b c ++  hanoi (n-1) c b a

hanoi4 0 _ _ _ _ = []
hanoi4 1 a b _ _ = [(a,b)]
hanoi4 n a b c d =
  hanoi4 k a c b d ++ hanoi (n-k) a b c ++ hanoi4 k c b a d
  where k = n - ((round.sqrt.fromIntegral) (2*n + 1)) + 1 
