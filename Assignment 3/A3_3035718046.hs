{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

--------------------------------------------
-- Functional Programming (Assignment 3)
--------------------------------------------

-- Name: Syed Muhammad Ali Murtaza Jaffery
-- UID : 3035718046

module A3_3035718046 where

-- download Parsing.hs at the Moodle Lecture 7 (no need to submit this file)

import Control.Monad (ap, liftM, liftM2)
import Control.Monad.Trans.State (State, evalState, get, put)
import Data.Bits (Bits ((.&.), (.|.)))
import GHC.Base (liftA)
import Parsing
import Text.Parsec.Token (GenTokenParser (operator))

---------------------------------
-- IO Interaction
---------------------------------

-- Problem 1
nextPlayerNumber :: Int -> Int
nextPlayerNumber n = if n == 1 then 2 else 1

printBoard :: [Int] -> IO ()
printBoard board = do
  putStr $ unlines [[if x <= stars then '*' else ' ' | x <- [1 .. length board]] ++ " | " ++ show rows | (stars, rows) <- zip board [1 .. length board]]
  putStr "\n"

gameFinished :: [Int] -> Bool
gameFinished = all (== 0)

checkRows :: Int -> [Int] -> Bool
checkRows row board = row <= length board

checkStars :: Int -> Int -> [Int] -> Bool
checkStars row star board = (board !! (row - 1)) >= star

getNewBoard :: Int -> Int -> [Int] -> [Int]
getNewBoard row star oldBoard = [if oldBoardRow == row then oldBoardStar - star else oldBoardStar | (oldBoardStar, oldBoardRow) <- zip oldBoard [1 .. length oldBoard]]

playGame :: [Int] -> Int -> IO ()
playGame board player = do
  printBoard board
  if gameFinished board
    then do
      putStr "Player: "
      putStr (show (nextPlayerNumber player))
      putStrLn " wins!"
    else do
      putStr "Player "
      print player
      putStr "Enter a row number: "
      inputRow <- getLine
      let playerRow = read inputRow
      putStr "Stars to remove: "
      inputStars <- getLine
      putStr "\n"
      let playerStars = read inputStars
      if not (checkRows playerRow board)
        then do
          putStr "Warning: There are only "
          putStr $ show (length board)
          putStrLn " rows in the game. Try again."
          playGame board player
        else
          if not (checkStars playerRow playerStars board)
            then do
              putStr "Warning: There are only "
              putStr $ show (board !! (playerRow - 1))
              putStr " stars in the row "
              putStr $ show playerRow
              putStrLn ". Try again."
              playGame board player
            else playGame (getNewBoard playerRow playerStars board) (nextPlayerNumber player)

nim :: Int -> IO ()
nim n = playGame [1 .. n] 1

-- Problem 2

switchPlayer :: [Char] -> [Char]
switchPlayer player = if player == "Player" then "AI" else "Player"

getAIrow :: Int -> [Int] -> Int
getAIrow stars board = head [rowIndex | (starInEachRow, rowIndex) <- zip board [1 .. length board], starInEachRow == stars]

playWithAI :: [Int] -> [Char] -> IO ()
playWithAI board player = do
  printBoard board
  if gameFinished board
    then do
      putStr $ switchPlayer player
      putStrLn " wins!"
    else do
      putStrLn player
      if player == "Player"
        then do
          putStr "Enter a row number: "
          inputRow <- getLine
          let playerRow = read inputRow
          putStr "Stars to remove: "
          inputStars <- getLine
          putStr "\n"
          let playerStars = read inputStars
          if not (checkRows playerRow board)
            then do
              putStr "Warning: There are only "
              putStr $ show (length board)
              putStrLn " rows in the game. Try again."
              playWithAI board player
            else
              if not (checkStars playerRow playerStars board)
                then do
                  putStr "Warning: There are only "
                  putStr $ show (board !! (playerRow - 1))
                  putStr " stars in the row "
                  putStr $ show playerRow
                  putStrLn ". Try again."
                  playWithAI board player
                else playWithAI (getNewBoard playerRow playerStars board) $ switchPlayer player
        else do
          let playerStars = maximum board
          let playerRow = getAIrow playerStars board
          putStr "Enter a row number: "
          print playerRow
          putStr "Stars to remove: "
          print playerStars
          putStr "\n"
          playWithAI (getNewBoard playerRow playerStars board) $ switchPlayer player

nimAI :: Int -> IO ()
nimAI n = playWithAI [1 .. n] "Player"

---------------------------------
-- Functional Parsing
---------------------------------

data Binop = Add | Sub | Mul | Div | Mod deriving (Eq, Show)

data Expr
  = Bin Binop Expr Expr
  | Val Int
  | Var String
  deriving (Eq, Show)

type Env = [(String, Int)]

-- Problem 3

-- getOperation :: Binop -> a -> a -> a
getOperation :: Integral a => Binop -> a -> a -> a
getOperation Add = (+)
getOperation Sub = (-)
getOperation Mul = (*)
getOperation Div = div
getOperation Mod = mod

performOperation :: Integral a => Binop -> Maybe a -> Maybe a -> Maybe a
performOperation operation left Nothing = Nothing
performOperation operation Nothing right = Nothing
performOperation Div left (Just 0) = Nothing
performOperation Mod left (Just 0) = Nothing
performOperation operation (Just x) (Just y) = Just (getOperation operation x y)

eval :: Env -> Expr -> Maybe Int
eval env (Var x) = lookup x env
eval env (Val x) = Just x
eval env (Bin operation left right) = performOperation operation (eval env left) (eval env right)

-- Problem 4

getBinopFromOperator :: Char -> Binop
getBinopFromOperator '+' = Add
getBinopFromOperator '-' = Sub
getBinopFromOperator '*' = Mul
getBinopFromOperator '/' = Div
getBinopFromOperator '%' = Mod
getBinopFromOperator _ = error "Incorrect Operator"

opTerm :: Expr -> Parser Expr
opTerm expr = do
  operation <- token (char '+') +++ token (char '-')
  let binop = getBinopFromOperator operation
  t <- term
  opTerm (Bin binop expr t) +++ return (Bin binop expr t)

term :: Parser Expr
term = do
  f <- factor
  opFactor f +++ return f

opFactor :: Expr -> Parser Expr
opFactor expr = do
  operation <- token (char '*') +++ token (char '/') +++ token (char '%')
  let binop = getBinopFromOperator operation
  f <- factor
  opFactor (Bin binop expr f) +++ return (Bin binop expr f)

factor :: Parser Expr
factor = parseParenthesis +++ parseInteger +++ parseIdentifier
  where
    parseParenthesis = do
      token $ char '('
      e <- pExpr
      token $ char ')'
      return e
    parseInteger = do
      x <- integer
      return $ Val x
    parseIdentifier = do
      i <- identifier
      return $ Var i

pExpr :: Parser Expr
pExpr = do
  t <- term
  opTerm t +++ return t

-- Problem 5

optimize :: Expr -> Maybe Expr
optimize (Var x) = Just $ Var x
optimize (Val x) = Just $ Val x
optimize (Bin Mul expr (Val 0)) = Just $ Val 0
optimize (Bin Mul (Val 0) expr) = Just $ Val 0
optimize (Bin Add expr (Val 0)) = Just expr
optimize (Bin Add (Val 0) expr) = Just expr
optimize (Bin Sub expr (Val 0)) = Just expr
optimize (Bin Div expr (Val 0)) = Nothing
optimize (Bin Mod expr (Val 0)) = Nothing
optimize (Bin operator (Val x) (Val y)) = Just $ Val (getOperation operator x y)
optimize (Bin operator (Val x) (Var y)) = Just $ Bin operator (Val x) (Var y)
optimize (Bin operator (Var x) (Val y)) = Just $ Bin operator (Var x) (Val y)
optimize (Bin operator (Var x) (Var y)) = Just $ Bin operator (Var x) (Var y)
optimize (Bin operator e1 e2) = case (optimize e1, optimize e2) of
  (Nothing, y) -> Nothing
  (x, Nothing) -> Nothing
  (Just x, Just y) -> if x == e1 && y == e2 then Just (Bin operator e1 e2) else optimize (Bin operator x y)

---------------------------------
-- Programming with Monads
---------------------------------

-- Problem 6

type EvalState = [Int]

type EvalValue = Int

popFromStack :: EvalState -> (Int, EvalState)
popFromStack [] = (0, [])
popFromStack (x : xs) = (x, xs)

pushToStack :: Int -> EvalState -> EvalState
pushToStack n xs = n : xs

-- this function will be handling special operations which require 2 integers
performCalculation :: (Int -> Int -> Int) -> [String] -> State EvalState EvalValue
performCalculation f xs = do
  stack <- get
  let (x, lst) = popFromStack stack
  let y = f x (head lst)
  put (pushToStack y (drop 2 stack))
  evalL xs

evalL :: [String] -> State EvalState EvalValue
evalL ("+" : xs) = performCalculation (+) xs
evalL ("-" : xs) = performCalculation (-) xs
evalL ("*" : xs) = performCalculation (*) xs
evalL ("/" : xs) = performCalculation div xs
evalL ("&" : xs) = performCalculation (.&.) xs
evalL ("|" : xs) = performCalculation (.|.) xs
evalL ("inc" : xs) = do
  stack <- get
  let (x, lst) = popFromStack stack
  let h = x + 1
  put (pushToStack h lst)
  evalL xs
evalL ("dec" : xs) = do
  stack <- get
  let (x, lst) = popFromStack stack
  let h = x - 1
  put (pushToStack h lst)
  evalL xs
evalL ("dup" : xs) = do
  stack <- get
  let (x, lst) = popFromStack stack
  put $ x : x : lst
  evalL xs
evalL ("clear" : xs) = do
  put []
  evalL xs
evalL (num : xs) = do
  stack <- get
  put (pushToStack (read num) stack)
  evalL xs
evalL [] = do
  stack <- get
  let (x, lst) = popFromStack stack
  if null stack
    then return 0
    else return x

solveRPN :: String -> Int
solveRPN xs = evalState (evalL . words $ xs) []

-- Problem 7

newtype Stack a = Stack {runStack :: [Int] -> ([Int], a)}

instance Functor Stack where
  fmap = liftM

instance Applicative Stack where
  pure x = Stack $ \s -> (s, x)
  (<*>) = ap

instance Monad Stack where
  return = pure
  m >>= k = Stack $ \s -> case runStack m s of
    (s', x) -> runStack (k x) s'

-- If stack and string are empty, 0 will be returned else new stack will be returned with previous head
pop :: Stack Int
pop = Stack $ \xs -> case xs of
  [] -> return 0
  (x : xs) -> (xs, x)

push :: Int -> Stack Int
push n = Stack $ \xs -> (n : xs, n)

evalStack :: Stack Int -> [Int] -> Int
evalStack m s = snd (runStack m s)

-- this function will be handling special operations which require 2 integers
performCalculation' :: (Int -> Int -> Int) -> [String] -> Stack Int
performCalculation' f nextOperation = do
  x <- pop
  y <- pop
  push (f x y)
  evalL' nextOperation

evalL' :: [String] -> Stack Int
evalL' ("+" : xs) = performCalculation' (+) xs
evalL' ("-" : xs) = performCalculation' (-) xs
evalL' ("*" : xs) = performCalculation' (*) xs
evalL' ("/" : xs) = performCalculation' div xs
evalL' ("&" : xs) = performCalculation' (.&.) xs
evalL' ("|" : xs) = performCalculation' (.|.) xs
evalL' ("inc" : xs) = do
  x <- pop
  push (x + 1)
  evalL' xs
evalL' ("dec" : xs) = do
  x <- pop
  push (x - 1)
  evalL' xs
evalL' ("dup" : xs) = do
  x <- pop
  push x
  push x
  evalL' xs
evalL' ("clear" : xs) = Stack (\_ -> runStack (evalL' xs) [])
evalL' (num : xs) = do
  push $ read num
  evalL' xs
evalL' [] = pop

solveRPN' :: String -> Int
solveRPN' [] = 0
solveRPN' xs = evalStack (evalL' (words xs)) []

-- Problem 8

safeLog :: (Ord a, Floating a) => a -> Maybe a
safeLog x
  | x > 0 = Just (log x)
  | otherwise = Nothing

safeSqrt :: (Ord a, Floating a) => a -> Maybe a
safeSqrt x
  | x >= 0 = Just (sqrt x)
  | otherwise = Nothing

safeLogSqrt :: (Ord a, Floating a) => a -> Maybe a
safeLogSqrt x = (>>=) (safeSqrt x) safeLog

-- Problem 9
zipL :: [a] -> [b] -> [(a, b)]
zipL x [] = []
zipL [] y = []
zipL (x : xs) (y : ys) = return (x, y) ++ zipL xs ys

-- Problem 10 (Advanced)

newtype LM a = LM {getLM :: [Maybe a]}
  deriving (Functor)

instance Applicative LM where
  pure = return
  (<*>) = liftM2 ($)

instance Monad LM where
  return :: a -> LM a
  return = undefined

  (>>=) :: LM a -> (a -> LM b) -> LM b
  (>>=) = undefined

---------- Monad Laws Reasoning ----------------
-- Write it here
-------------------------------------------------

newtype ML a = ML {getML :: Maybe [a]}
  deriving (Functor)

instance Applicative ML where
  pure = return
  (<*>) = liftM2 ($)

instance Monad ML where
  return :: a -> ML a
  return = undefined

  (>>=) :: ML a -> (a -> ML b) -> ML b
  (>>=) = undefined

---------- Monad Laws Reasoning ----------------
-- Write it here
-------------------------------------------------