{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}

--------------------------------------------
-- Functional Programming (Assignment 3)
--------------------------------------------

-- Name: Md Abdullah Al Mahin
-- UID : 3035767528

module A3 where

import Parsing
import Control.Monad.Trans.State ( get, put, State, evalState )
import Control.Monad ( ap, liftM, liftM2 )
import GHC.Base (liftA)
import Data.Bits ( Bits((.|.), (.&.)) )

---------------------------------
-- IO Interaction
---------------------------------

-- Problem 1

-- Returns a string to output as the game board
boardString :: Int -> [Int] -> [Char]
boardString x []     =  "\n"
boardString x (y:ys) =  replicate y '*' ++ 
                        replicate (length ys + (x - y)) ' ' ++ 
                        " | " ++ show x ++ "\n" ++ 
                        boardString (x + 1) ys

-- Draws the board on the terminal screen
drawBoard :: Int -> [Int] -> IO ()
drawBoard x ys = putStr (boardString x ys)

-- Helper function to give the next player in the game
nextPlayer :: [Char] -> [Char]
nextPlayer "1" = "2"
nextPlayer "2" = "1"
nextPlayer  _  = error "Invalid Player"

-- Returns error string in case the row input is invalid
errorStringRow :: Show a => a -> [Char]
errorStringRow l =  "\nWarning: There are only " ++ 
                    show l ++ " rows in the game. Try again.\n"

-- Returns error string in case the star input is invalid
errorStringStars :: (Show a1, Show a2) => a1 -> a2 -> [Char]
errorStringStars y l =  "\nWarning: There are only " ++ 
                        show y ++ " stars in the row " ++ 
                        show l ++ ". Try again.\n"

-- Gets Inputs from the current player
getInputs :: [Char] -> [Int] -> IO [Int]
getInputs p ys = do
                  putStrLn ("Player " ++ p)
                  putStr "Enter a row number: "
                  input1 <- getLine
                  putStr "Stars to remove: " 
                  input2 <- getLine 
                  -- Casting the inputs to int type
                  let row  = (read input1 :: Int)
                  let star = (read input2 :: Int)
                  let l = length ys
                  if row > l || row < 1 then
                    do  putStrLn (errorStringRow l)
                        getInputs p ys
                  else if star > (ys !! (row - 1)) || star < 1 then
                    do  putStrLn (errorStringStars (ys !! (row - 1)) row)
                        getInputs p ys
                  else 
                    return [row, star]

-- Changes the values of the game array to reflect a played move
playMove :: (Eq t1, Num t1, Num t2) => t1 -> t2 -> [t2] -> [t2]
playMove 1   star (x:xs) = (x - star) : xs
playMove row star (x:xs) = x : playMove (row - 1) star xs
playMove row star []     = error "Incorrect row value!"

-- Checks if winning conditions are met
checkWin :: (Eq a, Num a) => [a] -> Bool
checkWin xs = and [x == 0 | x <- xs]

-- Simulates a players turn
play :: [Char] -> [Int] -> IO ()
play p xs = do  
              drawBoard 1 xs
              input <- getInputs p xs
              let new = playMove (head input) (input !! 1) xs
              if checkWin new then
                    putStrLn ("\nPlayer " ++ p ++ " wins!\n")
              else
                do 
                  putStrLn ""
                  play (nextPlayer p) new

nim :: Int -> IO ()
nim x = play "1" [1..x]

-- Problem 2

-- Find the row with max stars for the AI's move
findRow :: (Ord t, Num a) => a -> t -> a -> [t] -> a
findRow i _   _    [] = i
findRow i max curr (x:xs) | x > max = findRow curr x (curr + 1) xs
                          | otherwise = findRow i max (curr + 1) xs

-- Simulates both the player's and AI's turn
playAI :: [Int] -> IO ()
playAI xs = do  
              -- Player turn
              drawBoard 1 xs
              input <- getInputs "" xs
              let new = playMove (head input) (input !! 1) xs
              if checkWin new then
                putStrLn "\nPlayer wins!\n"
              else
                do
                  -- AI turn
                  drawBoard 1 new
                  putStrLn "AI"
                  let row = findRow 1 0 1 new
                  let star = new !! (row - 1)
                  let new2 = playMove row star new
                  putStrLn ("Enter a row number: " ++ show row)
                  putStrLn ("Stars to remove: " ++ show star)
                  if checkWin new2 then
                    putStrLn "\nAI wins!\n"
                  else
                    do  
                      putStrLn ""
                      playAI new2

nimAI :: Int -> IO ()
nimAI x = playAI [1..x]

---------------------------------
-- Functional Parsing
---------------------------------

data Binop = Add | Sub | Mul | Div | Mod deriving (Eq, Show)

data Expr = Bin Binop Expr Expr
          | Val Int
          | Var String deriving (Eq, Show)

type Env = [(String, Int)]

-- Problem 3

-- Generalizes a pattern 
doOp :: (t1 -> t2 -> a) -> Maybe t1 -> Maybe t2 -> Maybe a
doOp f (Just x) (Just y) = Just (f x y)
doOp f _        _        = Nothing

-- Generalizes a pattern 
doOp' :: (Eq t1, Num t1) => (t2 -> t1 -> a) -> Maybe t2 -> Maybe t1 -> Maybe a
doOp' f (Just x) (Just 0) = Nothing
doOp' f (Just x) (Just y) = Just (f x y)
doOp' f _        _        = Nothing

add :: Num a => Maybe a -> Maybe a -> Maybe a
add = doOp (+)
sub :: Num a => Maybe a -> Maybe a -> Maybe a
sub = doOp (-)
mul :: Num a => Maybe a -> Maybe a -> Maybe a
mul = doOp (*)
div' :: Integral a => Maybe a -> Maybe a -> Maybe a
div' = doOp' div
mod' :: Integral a => Maybe a -> Maybe a -> Maybe a
mod' = doOp' mod

eval :: Env -> Expr -> Maybe Int
eval env (Val x)       = Just x
eval env (Var x)       = lookup x env
eval env (Bin Add x y) = add  (eval env x) (eval env y)
eval env (Bin Sub x y) = sub  (eval env x) (eval env y)
eval env (Bin Mul x y) = mul  (eval env x) (eval env y)
eval env (Bin Div x y) = div' (eval env x) (eval env y)
eval env (Bin Mod x y) = mod' (eval env x) (eval env y)

-- Problem 4

-- expr := term op_term
pExpr :: Parser Expr
pExpr = do  f  <- pTerm
            op <- pOpTerm
            return (op f)

-- op_term := ('+' | '-') term op_term | ''
-- op_term := '+' term op_term | '-' term op_term | ''
pOpTerm :: Parser (Expr -> Expr)
pOpTerm = pAddTerm +++ pSubTerm +++ pEmpty
    where pAddTerm = do token $ char '+'
                        f <- pTerm
                        op <- pOpTerm
                        return (op . apply Add f False)
          pSubTerm = do token $ char '-'
                        f <- pTerm
                        op <- pOpTerm
                        return (op . apply Sub f False)

-- term := factor op_factor
pTerm :: Parser Expr
pTerm = do  f  <- pFactor
            op <- pOpFactor
            return (op f)

-- op_factor := ('*' | '/' | '%') factor op_factor | ''
-- op_factor := '*' factor op_factor | '/' factor op_factor | 
--              '%' factor op_factor | ''
pOpFactor :: Parser (Expr -> Expr)
pOpFactor = pMulFactor +++ pDivFactor +++ pModFactor +++ pEmpty
      where pMulFactor = do token $ char '*'
                            f <- pFactor
                            op <- pOpFactor
                            return (op. apply Mul f False)
            pDivFactor = do token $ char '/'
                            f <- pFactor
                            op <- pOpFactor
                            return (op. apply Div f False)
            pModFactor = do token $ char '%'
                            f <- pFactor
                            op <- pOpFactor
                            return (op . apply Mod f False)

-- factor := '(' expr ')' | integer | identifier
pFactor :: Parser Expr
pFactor = pPara +++ pInt +++ pVar
    where pPara = do  _ <- token $ char '('
                      e <- pExpr
                      _ <- token $ char ')'
                      return e
          pInt  = Val <$> integer
          pVar  = Var <$> identifier

-- Parser for an empty string
-- Var "ret" is a dummy value with no use
pEmpty :: Parser (Expr -> Expr)
pEmpty = do return (apply Mod (Var "ret") True)

-- Returns an expression based on operation and its two sub expressions
apply :: Binop -> Expr -> Bool -> Expr -> Expr
apply _ _ True  prev = prev
apply o f False prev = Bin o prev f

-- Problem 5

-- A dummy value Var "N" is used to represent Nothing in Binop syntax

-- Optimizes add
add'' :: Expr -> Expr
add'' (Bin Add (Var "N") _        ) = Var "N"
add'' (Bin Add _         (Var "N")) = Var "N"
add'' (Bin Add (Val 0)   x        ) = x
add'' (Bin Add x         (Val 0)  ) = x
add'' (Bin Add (Val x)   (Val y)  ) = Val (x + y)
add'' x                           = x

-- Optimizes sub
sub'' :: Expr -> Expr
sub'' (Bin Sub (Var "N") _        ) = Var "N"
sub'' (Bin Sub _         (Var "N")) = Var "N"
sub'' (Bin Sub x         (Val 0)  ) = x
sub'' (Bin Sub (Val x)   (Val y)  ) = Val (x - y)
sub'' x                           = x

-- Optimizes mul
mul'' :: Expr -> Expr
mul'' (Bin Mul (Var "N") _        ) = Var "N"
mul'' (Bin Mul _         (Var "N")) = Var "N"
mul'' (Bin Mul (Val 0)   x        ) = Val 0
mul'' (Bin Mul x         (Val 0)  ) = Val 0
mul'' (Bin Mul (Val x)   (Val y)  ) = Val (x * y)
mul'' x                           = x

-- Optimizes div
div'' :: Expr -> Expr
div'' (Bin Div (Var "N") _        ) = Var "N"
div'' (Bin Div _         (Var "N")) = Var "N"
div'' (Bin Div (Val 0)   x        ) = Val 0
div'' (Bin Div x         (Val 0)  ) = Var "N"
div'' (Bin Div (Val x)   (Val y)  ) = Val (x `div` y)
div'' x                           = x

-- Optimizes mod
mod'' :: Expr -> Expr
mod'' (Bin Mod (Var "N") _        ) = Var "N"
mod'' (Bin Mod _         (Var "N")) = Var "N"
mod'' (Bin Mod (Val 0)   x        ) = Val 0
mod'' (Bin Mod x         (Val 0)  ) = Var "N"
mod'' (Bin Mod (Val x)   (Val y)  ) = Val (x `mod` y)
mod'' x                           = x

optimize :: Expr -> Maybe Expr
optimize e = case optimize' e of  (Var "N") -> Nothing
                                  x         -> Just x

-- Driver code
optimize' :: Expr -> Expr
optimize' (Bin Add x y) = add'' (Bin Add (optimize' x) (optimize' y)) 
optimize' (Bin Sub x y) = sub'' (Bin Sub (optimize' x) (optimize' y)) 
optimize' (Bin Mul x y) = mul'' (Bin Mul (optimize' x) (optimize' y)) 
optimize' (Bin Div x y) = div'' (Bin Div (optimize' x) (optimize' y)) 
optimize' (Bin Mod x y) = mod'' (Bin Mod (optimize' x) (optimize' y)) 
optimize' x             = x 

---------------------------------
-- Programming with Monads
---------------------------------

-- Problem 6

type EvalState = [Int]
type EvalValue = Int

-- Generalizes some evalL function cases
doOperation f next = do x <- get
                        let y = f (head x) (x !! 1) 
                        put $ y : x
                        next 

evalL :: [String] -> State EvalState EvalValue
evalL ("+":xs) = doOperation (+)   (evalL xs) 
evalL ("-":xs) = doOperation (-)   (evalL xs)
evalL ("*":xs) = doOperation (*)   (evalL xs)
evalL ("/":xs) = doOperation div   (evalL xs)
evalL ("&":xs) = doOperation (.&.) (evalL xs)
evalL ("|":xs) = doOperation (.|.) (evalL xs)
evalL ("inc":xs) = do x <- get
                      let h = head x + 1
                      let t = tail x
                      put $ h : t
                      evalL xs
evalL ("dec":xs) = do x <- get
                      let h = head x - 1
                      let t = tail x
                      put $ h : t
                      evalL xs
evalL ("dup":xs) = do x <- get
                      let h = head x
                      let t = tail x
                      put $ h : h : t
                      evalL xs
evalL ("clear":xs) = do put []
                        evalL xs
evalL (x:xs) = do x' <- get
                  put $ read x : x'
                  evalL xs
evalL [] = do x <- get
              if null x then return 0
              else return (head x)

solveRPN :: String -> Int
solveRPN xs = evalState (evalL . words $ xs) []

-- Problem 7

newtype Stack a = Stack {runStack :: [Int] -> ([Int], a)}

instance Functor Stack where
    fmap = liftM

instance Applicative Stack where
    pure :: a -> Stack a
    pure x = Stack $ \s -> (s, x)
    (<*>) = ap

instance Monad Stack where
    return = pure
    m >>= k = Stack $ \s -> case runStack m s of
        (s', x) -> runStack (k x) s'

pop :: Stack Int
pop = Stack (\x -> if null x then error "List is empty, cannot pop." else (tail x, head x))

push :: Int -> Stack Int
push y = Stack (\x -> (y : x, y))

evalStack :: Stack Int -> [Int] -> Int
evalStack m s = snd (runStack m s)

-- Generalizes some evalL' function cases
doOperation' :: (Int -> Int -> Int) -> Stack b -> Stack b
doOperation' f next = do  x <- pop
                          y <- pop
                          push y
                          push x
                          push (f x y)
                          next

evalL' :: [String] -> Stack Int
evalL' ("+":xs) = doOperation' (+)   (evalL' xs) 
evalL' ("-":xs) = doOperation' (-)   (evalL' xs)
evalL' ("*":xs) = doOperation' (*)   (evalL' xs)
evalL' ("/":xs) = doOperation' div   (evalL' xs)
evalL' ("&":xs) = doOperation' (.&.) (evalL' xs)
evalL' ("|":xs) = doOperation' (.|.) (evalL' xs)
evalL' ("inc":xs) = do  x <- pop
                        push x
                        push (x + 1)
                        evalL' xs
evalL' ("dec":xs) = do  x <- pop
                        push x
                        push (x - 1)
                        evalL' xs
evalL' ("dup":xs) = do  x <- pop
                        push x
                        push x
                        evalL' xs
-- Restart evalL' with empty stack
evalL' ("clear":xs) = Stack (\_ -> runStack (evalL' xs) [])
evalL' (x:xs) = do  push $ read x
                    evalL' xs
evalL' [] = do pop

-- Driver function for testing purposes
solveRPN' :: String -> Int
solveRPN' xs = evalStack (evalL' . words $ xs) []

-- Problem 8

-- From question
safeLog :: (Ord a, Floating a) => a -> Maybe a
safeLog x
  | x > 0 = Just (log x)
  | otherwise = Nothing

-- From question
safeSqrt :: (Ord a, Floating a) => a -> Maybe a
safeSqrt x
  | x >= 0 = Just (sqrt x)
  | otherwise = Nothing

safeLogSqrt :: (Ord a, Floating a) => a -> Maybe a
safeLogSqrt x = safeSqrt x >>= safeLog

-- Problem 9

zipL :: [a] -> [b] -> [(a, b)]
zipL a [] = []
zipL [] b = []
zipL (a:as) (b:bs) = return (a, b) ++ zipL as bs

-- Problem 10 (Advanced)

newtype LM a = LM { getLM :: [Maybe a] }
  deriving (Functor, Show)

instance Applicative LM where
  pure = return
  (<*>) = liftM2 ($)

instance Monad LM where
  return :: a -> LM a
  return a = LM [Just a]

  (>>=) :: LM a -> (a -> LM b) -> LM b
  (>>=) i f = LM $ concat' $ map' f (getLM i)  

-- Maps a function to LM
map' :: (a -> LM b) -> [Maybe a] -> [LM b]
map' f [] = []
map' f (Nothing:xs) = LM [Nothing] : map' f xs
map' f ((Just x):xs) = f x : map' f xs

-- Concatenates a list of LMs
concat' :: [LM a] -> [Maybe a]
concat' xs = [ y | x <- xs, y <- getLM x ]

-- A function for testing purposes
testLM :: Num a => a -> LM a
testLM x = LM [Just (x + 1), Just (x-1)] 

------------ Monad Laws Reasoning --------------
{-
FIRST LAW:
return a >>= f
= {By definition of return}
LM [Just a] >>= f
= {By definition of >>=}
LM $ concat' $ map' f [Just a]
= {By definition of map'}
LM $ concat' [LM [Just b]]
= {By definition of concat'}
LM [Just b]
= {By definition of LM and typing of f}
f a

SECOND LAW:
m >>= return
{Induction on m IH: Let the Law is true for some m = LM xs}
{Case analysis on m}
1) Inductive Case: m = LM Nothing : xs
LM Nothing : xs >>= return
= {By definition of >>=}
LM $ concat' $ map' return (getLM i)
= {By definition of getLM}
LM $ concat' $ map' return (Nothing:xs)
= {By definition of map'}
LM $ concat' $ LM [Nothing] : map' return xs
= {By definition of concat'}
LM (Nothing : concat' map' return xs)
= {By I.H}
LM Nothing : xs
2) Inductive Case: m = LM (Just x) : xs
LM (Just x) : xs >>= return
= {By definition of >>=}
LM $ concat' $ map' return (getLM i)
= {By definition of getLM}
LM $ concat' $ map' return ((Just x):xs)
= {By definition of map'}
LM $ concat' $ LM [Just x] : map' return xs
= {By definition of concat'}
LM ((Just x) : concat' map' return xs)
= {By I.H}
LM (Just x) : xs

THIRD LAW:
LM [a,...] >>= (\x -> k x >>= h)
= {By definition of >>=}
LM $ concat' $ map' (\x -> k x >>= h) [Just a]
= {By definition of map'}
LM $ concat' [LM ((\x -> k x >>= h) a),...]
= {By definition of concat'}
LM [((\x -> k x >>= h) a),...]
= {By definition of LM and typing of f}
((\x -> k x >>= h) LM [a,...]
= {simplification}
k LM [a,...] >>= a
= {By definition of >>=}
LM [a,...] >>= k >>= a
-}
-------------------------------------------------

newtype ML a = ML { getML :: Maybe [a] }
  deriving (Functor, Show)

instance Applicative ML where
  pure = return
  (<*>) = liftM2 ($)

instance Monad ML where
  return :: a -> ML a
  return a = ML (Just [a])

  (>>=) :: ML a -> (a -> ML b) -> ML b
  (>>=) m g = case getML m of
                  Nothing -> ML Nothing
                  Just x  -> ML $ concat'' (map g x)

-- Appends two Maybe Lists
append :: Maybe [a] -> Maybe [a] -> Maybe [a]
append Nothing x = x
append x Nothing = x
append (Just x) (Just y) = Just (x ++ y)

-- Concatenates a list of MLs to a Maybe List with all the contents of the list of MLs
concat'' :: [ML a1] -> Maybe [a1]
concat'' [] = Just []
concat'' (x:xs) = case getML x of
                      Nothing -> Nothing 
                      Just y  -> append (Just y) (concat'' xs)

-- A function for testing purposes
testML :: Num a => a -> ML a
testML x = ML (Just [x+1, x+2, x+3, x+4] )

---------- Monad Laws Reasoning ----------------
{-
FIRST LAW:
return a >>= g
= {By definition of return}
ML Just [a] >>= g
= {By definition of >>=}
ML $ concat'' (map g [a])
= {By definition of map}
ML $ concat'' ([g a])
= {By definition of concat'' and append}
ML $ getML (g a)
= {By definition of ML}
g a

SECOND LAW:
m >>= return
{Case analysis on m}
1) Case m = ML Nothing
ML Nothing >>= return
= {By definition of >>=}
ML Nothing
2) case m = ML Just [a,...]
= {By definition of >>=}
ML $ concat'' (map return [a,...])
= {By definition of map}
ML $ concat'' [return a,...]
= {By definition of return}
ML $ concat'' [Just [a],...]
= {By definition of concat''}
ML Just [a,...]

THIRD LAW:
m >>= (\x -> k x >>= h)
{Case analysis on m}
1) Case m = ML Nothing
ML Nothing >>= (\x -> k x >>= h)
= {By definition of >>=}
ML Nothing
= {By definition of >>=}
ML Nothing >>= h
= {By definition of >>=}
(ML Nothing >>= h) >>= k
2) Case m = ML Just [a,...]
ML Just [a,...] >>= (\x -> k x >>= h)
= {By definition of >>=}
ML $ concat'' (map (\x -> k x >>= h) [a,...])
= {By definition of map}
ML $ concat'' [(\x -> k x >>= h) a,...]
= {simplification}
ML $ concat'' [k a >>= h,...]
= {By definition of >>=}
ML $ concat'' [ML $ concat'' (map h (getLM k a)),...]
= {Let k a = Just [b,...] (According to the typing of k)}
ML $ concat'' [ML $ concat'' (map h [b,...]),...]
= {By definition of map}
ML $ concat'' [ML $ concat'' [h b,...],...]
= {Let h b = Just [c,...] (According to the typing of h), and the definition of concat''}
ML $ concat'' [ML Just [c,...],...]
= {From definition of h b}
ML $ concat'' [h b,...]
= {From definition of map}
ML $ concat'' (map h [b,...])
= {From definition of concat''}
ML $ concat'' (map h (concat'' $ [ML Just [b,...],...]))
= {From definition of k a}
ML $ concat'' (map h (concat'' $ [k a,...]))
= {From definition of >>=}
(ML $ concat'' $ [k a,...]) >>= h
= {From definition of map}
(ML $ concat'' (map k [a,...])) >>= h
= {By definition of >>=}
(ML Just [a,...] >>= k) >>= h
-}
-------------------------------------------------