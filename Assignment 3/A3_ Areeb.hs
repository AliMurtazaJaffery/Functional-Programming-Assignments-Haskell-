{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}

--------------------------------------------
-- Functional Programming (Assignment 3)
--------------------------------------------

-- Name : Areeb Nadeem
-- UID : 3035666750

module A3_3035666750 where

-- download Parsing.hs at the Moodle Lecture 7 (no need to submit this file)

import Control.Monad (ap, liftM, liftM2)
import Control.Monad.Trans.State (State, evalState, get, put)
import Data.Bits (Bits ((.&.), (.|.)))
import GHC.Base (liftA)
import Parsing

---------------------------------
-- IO Interaction
---------------------------------

-- Problem 1

-- this function makes board based on n value provided by user when calling nim n
makeBoard :: Int -> [Int] -> [String]
makeBoard n xs = [replicate x '*' ++ replicate (n + 1 - x) ' ' ++ "| " ++ show i | (x, i) <- zip xs [1 .. length xs]]

printBoard :: [String] -> IO ()
printBoard [] = putStrLn ""
printBoard (x : xs) = do
  putStrLn x
  printBoard xs

nim :: Int -> IO ()
nim n = do
  printBoard (makeBoard n [1 .. n])
  let board = [1 .. n]
  playNim 1 board

-- if row chosen by user exceeds num of rows or if user choice < 1 then this function returns True
checkRowError :: Int -> Int -> [Int] -> Bool
checkRowError row stars board
  | row < 1 || row > length board = True
  | otherwise = False

-- if num of stars chosen by user exceeds num of stars or if user chooses more stars to remove than present,
-- then this function returns True
checkStarError :: Int -> Int -> [Int] -> Bool
checkStarError row stars board
  | stars > board !! (row -1) = True
  | otherwise = False

-- returns True if every int in list is 0, meaning all stars have been removed.
-- returns False otherwise
checkWin :: [Int] -> Bool
checkWin [0] = True
checkWin (x : xs) = if x /= 0 then False else checkWin xs

changePlayer :: Int -> Int
changePlayer n = if n == 1 then 2 else 1

-- this changes board based on moves of users
modifyBoard :: Int -> Int -> [Int] -> [Int]
modifyBoard row stars board = [if i == row then x - stars else x | (x, i) <- zip board [1 .. length board]]

playNim :: Int -> [Int] -> IO ()
playNim playerNum board = do
  putStrLn ("Player " ++ show playerNum)
  putStr "Enter a row number: "
  row <- getLine
  putStr "Stars to remove: "
  stars <- getLine
  putStrLn ""
  if checkRowError (read row) (read stars) board == True
    then do
      putStrLn ("Warning: There are only " ++ show (length board) ++ " rows in the game. Try again.")
      putStrLn ""
      playNim playerNum board
    else
      if checkStarError (read row) (read stars) board == True
        then do
          putStrLn ("Warning: There are only " ++ show (board !! (read (row) -1)) ++ " stars in the row " ++ row ++ ". Try again.")
          putStrLn ""
          playNim playerNum board
        else do
          let newBoard = modifyBoard (read row) (read stars) board
          printBoard (makeBoard (length newBoard) newBoard)
          if checkWin newBoard
            then putStrLn ("Player " ++ show (playerNum) ++ " wins!")
            else do
              let otherPlayer = changePlayer playerNum
              playNim otherPlayer newBoard

-- Problem 2

nimAI :: Int -> IO ()
nimAI n = do
  printBoard (makeBoard n [1 .. n])
  let board = [1 .. n]
  playNimAI "Player" board

playNimAI :: String -> [Int] -> IO ()
playNimAI player board = do
  putStrLn (player)
  if player == "Player"
    then playerMove player board
    else aIMove player board

-- this implements logic for user's move
playerMove :: String -> [Int] -> IO ()
playerMove player board = do
  putStr "Enter a row number: "
  row <- getLine
  putStr "Stars to remove: "
  stars <- getLine
  putStrLn ""
  if checkRowError (read row) (read stars) board == True
    then do
      putStrLn ("Warning: There are only " ++ show (length board) ++ " rows in the game. Try again.")
      putStrLn ""
      playNimAI player board
    else
      if checkStarError (read row) (read stars) board == True
        then do
          putStrLn ("Warning: There are only " ++ show (board !! (read row - 1)) ++ " stars in the row " ++ row ++ ". Try again.")
          putStrLn ""
          playNimAI player board
        else do
          let newBoard = modifyBoard (read row) (read stars) board
          printBoard (makeBoard (length newBoard) newBoard)
          if checkWin newBoard
            then putStrLn "Player wins!"
            else do
              playNimAI "AI" newBoard

-- this implements logic for AI's move
aIMove :: String -> [Int] -> IO ()
aIMove player board = do
  let row = getMaxRow (zip board [1 .. length board])
  let stars = board !! (row -1)
  putStrLn ("Enter a row number: " ++ show row)
  putStrLn ("Stars to remove: " ++ show stars)
  let newBoard = (modifyBoard row stars board)
  printBoard (makeBoard (length newBoard) newBoard)
  if checkWin newBoard
    then putStrLn "AI wins!"
    else do
      playNimAI "Player" newBoard

-- this gets row num of row with max stars (if more than one such row,
-- then first one is returned)
getMaxRow :: [(Int, Int)] -> Int
getMaxRow [x] = snd x
getMaxRow (x : y : ys) = if fst x >= fst y then getMaxRow (x : ys) else getMaxRow (y : ys)

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

eval :: Env -> Expr -> Maybe Int
eval env (Val x) = Just x
eval env (Var str) = lookup str env
eval env (Bin op expr1 expr2) = operateOnMaybeInts op (eval env expr1) (eval env expr2)

-- this function applies the operation on the values returned from eval
-- if atleast one of the values is Nothing, then Nothing is returned else
-- the operation is applied to values and returned as Just (result)
operateOnMaybeInts op Nothing _ = Nothing
operateOnMaybeInts op _ Nothing = Nothing
operateOnMaybeInts op (Just x) (Just y)
  | op == Add = Just (x + y)
  | op == Sub = Just (x - y)
  | op == Mul = Just (x * y)
  | op == Div = if y == 0 then Nothing else Just (x `div` y)
  | op == Mod = if y == 0 then Nothing else Just (x `mod` y)

-- Problem 4

pExpr :: Parser Expr
pExpr = do
  many (char ' ')
  t <- term
  many (char ' ')
  (do opTerm t) +++ return t

term :: Parser Expr
term = do
  many (char ' ')
  f <- factor
  many (char ' ')
  (do opFactor f) +++ return f

factor :: Parser Expr
factor =
  ( do
      many (char ' ')
      char '('
      many (char ' ')
      exp <- pExpr
      many (char ' ')
      char ')'
      return exp
  )
    +++ getNum
    +++ getVar

getNum :: Parser Expr
getNum = do
  int <- integer
  return (Val int)

getVar :: Parser Expr
getVar = do
  ident <- identifier
  return (Var ident)

opFactor :: Expr -> Parser Expr
opFactor exp =
  ( do
      many (char ' ')
      char '*'
      many (char ' ')
      x <- factor
      many (char ' ')
      (do opFactor (Bin Mul exp x)) +++ return (Bin Mul exp x)
  )
    +++ ( do
            many (char ' ')
            char '/'
            many (char ' ')
            i <- factor
            many (char ' ')
            (do opFactor (Bin Div exp i)) +++ return (Bin Div exp i)
        )
    +++ ( do
            many (char ' ')
            char '%'
            many (char ' ')
            j <- factor
            many (char ' ')
            (do opFactor (Bin Mod exp j)) +++ return (Bin Mod exp j)
        )

opTerm :: Expr -> Parser Expr
opTerm exp =
  ( do
      many (char ' ')
      char '+'
      many (char ' ')
      i <- term
      many (char ' ')
      (do opTerm (Bin Add exp i)) +++ return (Bin Add exp i)
  )
    +++ ( do
            many (char ' ')
            char '-'
            many (char ' ')
            j <- term
            many (char ' ')
            (do opTerm (Bin Sub exp j)) +++ return (Bin Sub exp j)
        )

-- Problem 5

optimize :: Expr -> Maybe Expr
optimize (Val x) = Just (Val x)
optimize (Var x) = Just (Var x)
optimize (Bin Mul (Val 0) _) = Just (Val 0)
optimize (Bin Mul _ (Val 0)) = Just (Val 0)
optimize (Bin Add (Val 0) e) = Just e
optimize (Bin Add e (Val 0)) = Just e
optimize (Bin Sub e (Val 0)) = Just e
optimize (Bin Div _ (Val 0)) = Nothing
optimize (Bin Mod _ (Val 0)) = Nothing
optimize (Bin f e1 e2) = applyOperation f (optimize e1) (optimize e2)

-- this function applies operation on Just values
-- returns Nothing if atleast one of the expression equates to Nothing
-- returns the expressions directly if they cannot be simplified.
applyOperation f Nothing _ = Nothing
applyOperation f _ Nothing = Nothing
applyOperation f (Just (Val x)) (Just (Val y))
  | f == Add = Just (Val (x + y))
  | f == Sub = Just (Val (x - y))
  | f == Mul = Just (Val (x * y))
  | f == Div = Just (Val (x `div` y))
  | f == Mod = Just (Val (x `mod` y))
applyOperation f (Just x) (Just y) = Just (Bin f x y)

---------------------------------
-- Programming with Monads
---------------------------------

-- Problem 6

type EvalState = [Int]

type EvalValue = Int

isDigit :: Char -> Bool
isDigit char = if char >= '0' && char <= '9' then True else False

evalL :: [String] -> State EvalState EvalValue
-- Base case
-- returns head of stack once all the strings have been iterated over
-- returns 0 if stack is empty
evalL [] = do
  x <- get
  if not (null x) then return (head x) else return 0

-- checking operations that require only one element to be popped from stack
-- Also adding integers to stack
-- if none of the above, then we pass it on to applyOtherOperations
evalL (op : xs) = do
  stack <- get
  case op of
    "inc" -> put (pushToStack (head stack + 1) stack)
    "dec" -> put (pushToStack (head stack -1) stack)
    "clear" -> put []
    "dup" -> put (pushToStack (head stack) stack)
    num -> if all isDigit num then put (pushToStack (read num) stack) else applyOtherOperations op
  evalL xs

-- checking operations that require two elements to be popped from stack OR
-- throwing error on invalid input
applyOtherOperations op = do
  stack <- get
  let (x1, lst) = popFromStack stack
  put lst
  do
    newStack <- get
    let (x2, lst2) = popFromStack newStack
    case op of
      "+" -> put (pushToStack (x1 + x2) lst2)
      "-" -> put (pushToStack (x2 - x1) lst2)
      "*" -> put (pushToStack (x1 * x2) lst2)
      "/" -> put (pushToStack (x2 `div` x1) lst2)
      "&" -> put (pushToStack ((.&.) x1 x2) lst2)
      "|" -> put (pushToStack ((.|.) x1 x2) lst2)
      _ -> error "Invalid input"

-- removing element from stack and returning it in form (element, newStack)
popFromStack :: EvalState -> (Int, EvalState)
popFromStack [] = (0, [])
popFromStack (x : xs) = (x, xs)

-- adding element to stack and returning newStack
pushToStack :: Int -> EvalState -> EvalState
pushToStack n xs = n : xs

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
  (>>=) :: Stack a -> (a -> Stack b) -> Stack b
  m >>= k = Stack $ \s -> case runStack m s of
    (s', x) -> runStack (k x) s'

e1 :: Stack Int
e1 = do pop

e2 :: Stack Int
e2 = do
  x <- pop
  push (x + 1)
  pop

e3 :: Stack Int
e3 = do
  clearStack
  pop

-- returning 0 if stack and string both are empty as stated in solution in assignment: eval [] [] = 0
-- returning new stack and previous head if stack is not empty
pop :: Stack Int
pop = Stack $ \xs -> case xs of
  (x : xs) -> (xs, x)
  [] -> return 0

push :: Int -> Stack Int
push n = Stack $ \xs -> (n : xs, n)

evalStack :: Stack Int -> [Int] -> Int
evalStack m s = snd (runStack m s)

solveRPN' :: String -> Int
solveRPN' [] = 0
solveRPN' xs = evalStack (evalL' (words xs)) []

-- emptying stack when 'clear' operation is applied
clearStack :: Stack Int
clearStack = Stack $ \xs -> ([], 0)

evalL' :: [String] -> Stack Int
-- base case
-- return top of stack when all strings have been iterated over
evalL' [] = pop
-- checking operations that require integer to be added to stack OR
-- those that require one element to be popped from stack
-- if neither of above, then applyOtherOperations' is called
evalL' (op : ys)
  | all isDigit op = do
    push (read op)
    evalL' ys
  | otherwise = do
    x <- pop
    case op of
      "inc" -> push (x + 1)
      "dec" -> push (x - 1)
      "clear" -> do clearStack
      "dup" -> do
        push x
        push x
      _ -> applyOtherOperations' op x
    evalL' ys

-- checking operations that require two elements to be popped from stack OR
-- throwing error on invalid input
applyOtherOperations' :: [Char] -> Int -> Stack Int
applyOtherOperations' op x = do
  y <- pop
  case op of
    "+" -> push (x + y)
    "-" -> push (y - x)
    "*" -> push (x * y)
    "/" -> push (y `div` x)
    "&" -> push ((.&.) x y)
    "|" -> push ((.|.) x y)
    _ -> error "Invalid input"

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

zipL = undefined

-- Problem 10 (Advanced)

newtype LM a = LM {getLM :: [Maybe a]}
  deriving (Functor)

instance Applicative LM where
  pure = return
  (<*>) :: LM (a -> b) -> LM a -> LM b
  (<*>) = liftM2 ($)

instance Monad LM where
  return :: a -> LM a
  return a = undefined

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