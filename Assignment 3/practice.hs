import Distribution.Simple.Hpc (guessWay)
import Parsing
import System.IO

getCh :: IO Char
getCh =
  do
    hSetEcho stdin False
    x <- getChar
    hSetEcho stdin True
    return x

sgetLine :: IO String
sgetLine =
  do
    x <- getCh
    if x == '\n'
      then do
        putChar '\n'
        return ""
      else do
        putChar '-'
        xs <- sgetLine
        return (x : xs)

hangman :: IO ()
hangman =
  do
    putStrLn "Think of a word: "
    word <- sgetLine
    putStrLn "Try to guess it"
    play word

match :: String -> String -> String
match xs ys = [if x `elem` ys then x else '-' | x <- xs]

play :: String -> IO ()
play word = do
  putStr "> "
  guess <- getLine
  if word == guess
    then putStrLn "You got it"
    else do
      putStrLn $ match word guess
      play word

-- Full grammar:
-- expr → term ('+' expr  ε)
--
-- term → factor ('*' term  ε)
--
-- factor → digit  '(' expr ')‘
--
-- digit  → '0'  '1'  …  '9'

-- Parsing Expressions:

-- Examples: 1, 1 + 1
-- eval "2 * 3 + 4 * 5"

-- "6+3"

-- expr → term ('+' expr  ε)

expr :: Parser Int
expr = do
  t <- term
  ( do
      char '+'
      e <- expr
      return (t + e)
    )
    +++ return t

-- term → factor ('*' term  ε)
term :: Parser Int
term = do
  f <- factor
  ( do
      char '*'
      t <- term
      return (f * t)
    )
    +++ return f

-- factor → '(' expr ')‘ | digit
-- Hint: use natural for parsing digits

factor :: Parser Int
factor =
  ( do
      char '('
      e <- expr
      char ')'
      return e
  )
    +++ natural

eval :: String -> Int
eval xs = case (parse expr xs) of
  [(n, [])] -> n
  [(_, out)] -> error ("unused input " ++ out)
  [] -> error "invalid input"

checking =
  do
    e <- expr
    return e