import Data.Bits

-- Question 1 Pentanacci Number
-- Here we using the recursive idea from fibonaccing number
pentanacci :: Int -> Int
pentanacci 1 = 0
pentanacci 2 = 0
pentanacci 3 = 0
pentanacci 4 = 0
pentanacci 5 = 1
pentanacci n = pentanacci (n - 1) + pentanacci (n - 2) + pentanacci (n - 3) + pentanacci (n - 4) + pentanacci (n - 5)

-- Question 2 solve
-- we are getting the quotient and adding it to the start of the list.
-- This makes sure that the difference between shares of each friend the least
solve :: Int -> Int -> [Int]
solve x 0 = error "The are 0 people"
solve x 1 = [x]
solve 0 y = replicate y 0
solve x y = quot x y : solve (x - quot x y) (y - 1)

-- Question 3 look up name

-- This checksubstring version compares the each characters of the string
checkSubstring :: String -> String -> Bool
checkSubstring [] [] = True
checkSubstring xs [] = True
checkSubstring [] xs = False
checkSubstring (x : xs) (y : ys) = (x == y) && checkSubstring xs ys

-- This function removes hyphens from name
formatHyphen :: String -> String
formatHyphen [] = []
formatHyphen (x : xs)
  | x == '-' = ' ' : formatHyphen xs
  | otherwise = x : formatHyphen xs

--This function compares each name in the list with the given prefix one by one
lookupName :: [String] -> String -> [String]
lookupName [] y = []
lookupName xs [] = [formatHyphen y | y <- xs]
lookupName (x : xs) y = if checkSubstring x y then formatHyphen x : lookupName xs y else lookupName xs y

--Question 4 sort on the basis of a predicate
-- got the code for merge sort from lecture 4 slide
-- just slightly modified that code by adding a dynamic predicate

merge :: [a] -> [a] -> (a -> a -> Bool) -> [a]
merge [] l f = l
merge l [] f = l
merge (x : xs) (y : ys) f = if f x y then x : merge xs (y : ys) f else y : merge (x : xs) ys f

sortp :: [a] -> (a -> a -> Bool) -> [a]
sortp [] f = []
sortp (x : xs) f = merge [x] (sortp xs f) f

--Question 5 adjust fold to account for infinite lists
-- Stop and return the summary value if the condition for predicate is satisfied

foldlp :: (b -> Bool) -> (b -> a -> b) -> b -> [a] -> b
foldlp p f v [] = v
foldlp p f v (x : xs)
  | p (f v x) = foldlp p f (f v x) xs
  | otherwise = v

--Question 6 Twin Paired
-- In this question create a new list of elements in even and odd psoition and check and see if it's sorted in the prescribed manner
isSortedInAscendingOrder :: (Ord a) => [a] -> Bool
isSortedInAscendingOrder [] = True
isSortedInAscendingOrder [x] = True
isSortedInAscendingOrder (x : y : xs) = x <= y && isSortedInAscendingOrder (y : xs)

isSortedInDescendingOrder :: (Ord a) => [a] -> Bool
isSortedInDescendingOrder [] = True
isSortedInDescendingOrder [x] = True
isSortedInDescendingOrder (x : y : xs) = x >= y && isSortedInAscendingOrder (y : xs)

isTwinPaired :: [Int] -> Bool
isTwinPaired xs = isSortedInAscendingOrder (filter even xs) && isSortedInDescendingOrder (filter odd xs)

--Question 7 Reverse Polish Notation (RPN) Calculator
-- In this question we are traversing the given string (by breaking it into a list of words) from left to right and implementing a stack.
-- We pop elements from the stack (1 or 2) depending on the operation and then add them back to the stack

customizedHead :: Num a => [a] -> a
customizedHead (x : _) = x
customizedHead [] = 0

solveRPN :: String -> Int
solveRPN xs = customizedHead (foldl foldingFunction [] (words xs))
  where
    foldingFunction (x : y : xs) "*" = (x * y) : xs
    foldingFunction (x : y : xs) "+" = (x + y) : xs
    foldingFunction (x : y : xs) "-" = (y - x) : xs
    foldingFunction (x : y : xs) "/" = (y `div` x) : xs
    foldingFunction (x : y : xs) "&" = (.&.) x y : xs
    foldingFunction (x : y : xs) "|" = (.|.) x y : xs
    foldingFunction (x : xs) "inc" = x + 1 : xs
    foldingFunction (x : xs) "dec" = x + 1 : xs
    foldingFunction (x : xs) "dup" = x : x : xs
    foldingFunction xs "clear" = []
    foldingFunction xs number = (read number :: Int) : xs