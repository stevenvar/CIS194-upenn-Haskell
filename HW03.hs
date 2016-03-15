module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend s v i = (\x -> if x == v then i else (s x))

empty :: State
empty = \x -> 0 

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE s (Var v) = s v
evalE s (Val i) = i
evalE s (Op e1 Plus e2) = (evalE s e1) + (evalE s e2)
evalE s (Op e1 Minus e2) = (evalE s e1) - (evalE s e2)
evalE s (Op e1 Times e2) = (evalE s e1) * (evalE s e2)
evalE s (Op e1 Divide e2) = (evalE s e1) `div` (evalE s e2)
evalE s (Op e1 Gt e2) = if (evalE s e1) > (evalE s e2) then 1 else 0
evalE s (Op e1 Ge e2) = if (evalE s e1) >= (evalE s e2) then 1 else 0
evalE s (Op e1 Lt e2) = if (evalE s e1) < (evalE s e2) then 1 else 0
evalE s (Op e1 Le e2) = if (evalE s e1) <= (evalE s e2) then 1 else 0
evalE s (Op e1 Eql e2) = if (evalE s e1) == (evalE s e2) then 1 else 0 

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign v e) = DAssign v e
desugar (Incr v) = DAssign v (Op (Var v) Plus (Val 1))
desugar (If e s_then s_else) = DIf e (desugar s_then) (desugar s_else)
desugar (While e body) = DWhile e (desugar body) 
desugar (For i c u b) = DSequence
                          (desugar i)
                          (DWhile c (DSequence (desugar b) (desugar u)))
desugar (Sequence s_1 s_2) = DSequence (desugar s_1) (desugar s_2)
desugar Skip = DSkip 

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple s (DAssign v e) = extend s v (evalE s e)
evalSimple s (DIf e ds_then ds_else) =
  if (evalE s e) == 0
  then (evalSimple s ds_else)
  else (evalSimple s ds_then)
evalSimple s w@(DWhile e body) =
  if (evalE s e) == 0
  then s
  else evalSimple (evalSimple s body) w
evalSimple s (DSequence ds_1 ds_2) = evalSimple (evalSimple s ds_1) ds_2
evalSimple s (DSkip) = s 

run :: State -> Statement -> State
run s st = evalSimple s (desugar st)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   Out := 0;
   while (In >= Out * Out) {
     Out++
   };
   Out := Out - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "Out" (Val 0)
                   , While (Op (Var "In") Ge (Op (Var "Out") Times (Var "Out")))
                       (Incr "Out")
                   , Assign "Out" (Op (Var "Out") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 0)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
