------------------------------------------------------
-- Functional Programming (Assignment 2)
------------------------------------------------------

-- Name : Syed Muhammad Ali Murtaza Jaffery
-- UID  : 3035718046

-- change the module name to your prefered one
module Template where

import Data.List (nub)

------------------------------------------------------
-- Warm up (15 pts)
------------------------------------------------------

data Tree a = Leaf | Branch a (Tree a) (Tree a) deriving (Show, Eq)

foldTree :: b -> (a -> b -> b -> b) -> Tree a -> b
foldTree e _ Leaf = e
foldTree e n (Branch a n1 n2) = n a (foldTree e n n1) (foldTree e n n2)

-- e is accumulator
-- n is the function

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f = foldTree Leaf (Branch . f)

-- Problem 1 (5 pts)
-- for this problem we let you figure out the type signiture on your own
takeWhileTree :: (a -> Bool) -> Tree a -> Tree a
takeWhileTree p = foldTree Leaf (\x t1 t2 -> if p x then Branch x t1 t2 else Leaf)

-- Problem 2 (10 pts)
-- for this problem we let you figure out the type signiture on your own
mergeTree :: Tree a -> Tree a -> Tree (a, a)
mergeTree Leaf Leaf = Leaf
mergeTree Leaf (Branch a l r) = Leaf
mergeTree (Branch a l r) Leaf = Leaf
mergeTree (Branch a l r) (Branch b l' r') = Branch v lMerge rMerge
  where
    v = (a, b)
    lMerge = mergeTree l l'
    rMerge = mergeTree r r'

zipTree :: (a -> a -> b) -> Tree a -> Tree a -> Tree b
zipTree f xs ys = mapTree (uncurry f) (mergeTree xs ys)

------------------------------------------------------
-- Propositional Logic (25 pts)
------------------------------------------------------

type Name = String

data Prop
  = Var Name
  | F
  | T
  | Not Prop
  | Prop :|: Prop
  | Prop :&: Prop
  | Prop :->: Prop
  | Prop :<->: Prop
  deriving (Show, Eq)

-- Problem 3 (10 pts)

-- Gets name sof all the variables used ina  proposition
getVariableNamesFromProposition :: Prop -> [Name]
getVariableNamesFromProposition (Var x) = [x]
getVariableNamesFromProposition F = []
getVariableNamesFromProposition T = []
getVariableNamesFromProposition (Not p) = getVariableNamesFromProposition p
getVariableNamesFromProposition (p :|: q) = getVariableNamesFromProposition p ++ getVariableNamesFromProposition q
getVariableNamesFromProposition (p :&: q) = getVariableNamesFromProposition p ++ getVariableNamesFromProposition q
getVariableNamesFromProposition (p :->: q) = getVariableNamesFromProposition p ++ getVariableNamesFromProposition q
getVariableNamesFromProposition (p :<->: q) = getVariableNamesFromProposition p ++ getVariableNamesFromProposition q

-- generates combination of different Truth and False Values for the names in a proposition
generatePossibleEnv :: [Name] -> [Env]
generatePossibleEnv [] = [[]]
generatePossibleEnv (x : xs) =
  [(x, True) : e | e <- generatePossibleEnv xs]
    ++ [(x, False) : e | e <- generatePossibleEnv xs]

satisfiable :: Prop -> Bool
satisfiable p = or ([eval env p | env <- generatePossibleEnv (nub (getVariableNamesFromProposition p))])

unsatisfiable :: Prop -> Bool
unsatisfiable p = all (False ==) ([eval env p | env <- generatePossibleEnv (nub (getVariableNamesFromProposition p))])

valid :: Prop -> Bool
valid p = and ([eval env p | env <- generatePossibleEnv (nub (getVariableNamesFromProposition p))])

-- Problem 4 (5 pts)
type Env = [(Name, Bool)]

-- assuming variable will always be there in the Env
getBooleanFromEnv :: Name -> [(Name, Bool)] -> Bool
getBooleanFromEnv z xys = head [y | (x, y) <- xys, x == z]

eval :: Env -> Prop -> Bool
eval env (Var name) = getBooleanFromEnv name env
eval env F = False
eval env T = True
eval env (Not p) = not $ eval env p
eval env (p :|: q) = eval env p || eval env q
eval env (p :&: q) = eval env p && eval env q
eval env (p :->: q) = eval env (Not p :|: q)
eval env (p :<->: q) = eval env ((p :->: q) :&: (q :->: p))

-- Problem 5 (10 pts)

-- get all the environments which give Truth values for the proposition
getPropositionsWithTruthValues :: Prop -> [Env]
getPropositionsWithTruthValues p = [env | env <- generatePossibleEnv (nub (getVariableNamesFromProposition p)), eval env p]

toDNF :: Prop -> [[Prop]]
toDNF p = foldr (\env -> (++) [[if booleanValue then Var name else Not (Var name) | (name, booleanValue) <- env]]) [] (getPropositionsWithTruthValues p)

------------------------------------------------------
-- Trie (30 pts)
------------------------------------------------------

data Trie k v = Trie
  { value :: Maybe v,
    subTrees :: [(k, Trie k v)]
  }
  deriving (Show)

emptyTrie :: Trie k v
emptyTrie = Trie Nothing []

-- Problem 6 (10 pts)
insertTrie :: Eq k => Trie k v -> [k] -> v -> Trie k v
insertTrie (Trie value subtrees) [] valueToInsert = Trie {value = Just valueToInsert, subTrees = subtrees}
insertTrie Trie {value = val, subTrees = subtrees} (x : xs) v = case lookup x subtrees of
  Nothing -> Trie {value = val, subTrees = subtrees ++ [(x, insertTrie emptyTrie xs v)]}
  (Just trieVal) -> Trie {value = val, subTrees = getUnaffectedTrees subtrees ++ [(x, insertTrie trieVal xs v)]}
  where
    getUnaffectedTrees [] = []
    getUnaffectedTrees (subT : subTs) = if fst subT == x then getUnaffectedTrees subTs else subT : getUnaffectedTrees subTs

-- Problem 7 (5 pts)
lookupTrie :: Eq k => [k] -> Trie k v -> Maybe v
lookupTrie [] (Trie value subtrees) = value
lookupTrie (x : xs) (Trie value subtrees) = case lookup x subtrees of
  Nothing -> Nothing
  (Just val) -> lookupTrie xs val

-- Problem 8 (5 pts)

-- A helper function which uses the list of pairs to recursively build a trie structure using the inserTrie function
buildTrieFromList :: Eq k => Trie k v -> [([k], v)] -> Trie k v
buildTrieFromList trie [] = trie
buildTrieFromList trie ((key, val) : pairs) = buildTrieFromList (insertTrie trie key val) pairs

fromList :: Eq k => [([k], v)] -> Trie k v
fromList = buildTrieFromList emptyTrie

-- Problem 9 (10 pts)
-- figure out the type signiture by yourself
fromString :: String -> Trie Char Int
fromString s = fromList (zip (words s) [1 .. (length (words s))])

------------------------------------------------------
-- Functional Data Structure (30 pts)
------------------------------------------------------

flatten :: Ord a => Tree a -> [a]
flatten Leaf = []
flatten (Branch x l r) = x : merge (flatten l) (flatten r)

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
  | x <= y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

-- Problem 10 (10 pts)
heapSort :: Ord a => [a] -> [a]
heapSort = flatten . buildHeap

buildHeap :: Ord a => [a] -> Tree a
buildHeap = heapify . buildTree

splitHalf :: [a] -> ([a], [a])
splitHalf l = splitAt ((length l + 1) `div` 2) l

buildTree :: Ord a => [a] -> Tree a
buildTree [] = Leaf
buildTree (x : xs) = Branch x (buildTree (fst (splitHalf xs))) (buildTree (snd (splitHalf xs)))

heapify :: Ord a => Tree a -> Tree a
heapify Leaf = Leaf
heapify (Branch x l r) = siftDown x (heapify l) (heapify r)

siftDown :: Ord a => a -> Tree a -> Tree a -> Tree a
siftDown x Leaf Leaf = Branch x Leaf Leaf
siftDown x Leaf (Branch a l r)
  | x > a = Branch a Leaf (siftDown x l r)
  | otherwise = Branch x Leaf (Branch a l r)
siftDown x (Branch a l r) Leaf
  | x > a = Branch a (siftDown x l r) Leaf
  | otherwise = Branch x (Branch a l r) Leaf
siftDown x (Branch a la ra) (Branch b lb rb)
  | x <= a && x <= b = Branch x (Branch a la ra) (Branch b lb rb)
  | x > a && x > b = if a < b then Branch a (siftDown x la ra) (Branch b lb rb) else Branch b (Branch a la ra) (siftDown x lb rb)
  | x > a && x <= b = Branch a (siftDown x la ra) (Branch b lb rb)
  | x > b && x <= a = Branch b (Branch a la ra) (siftDown x lb rb)

-- Problem 11 (20 pts)

data PQueue a p
  = Null
  | Fork Order a p (PQueue a p) (PQueue a p)
  deriving (Show, Eq)

type Order = Int

flatMerge :: (Ord b) => [(a, b)] -> [(a, b)] -> [(a, b)]
flatMerge [] ys = ys
flatMerge xs [] = xs
flatMerge (x : xs) (y : ys)
  | snd x <= snd y = x : flatMerge xs (y : ys)
  | otherwise = y : flatMerge (x : xs) ys

flattenQ :: (Ord p) => PQueue a p -> [(a, p)]
flattenQ Null = []
flattenQ (Fork o a p l r) = (a, p) : flatMerge (flattenQ l) (flattenQ r)

getOrder :: PQueue a p -> Int
getOrder Null = 0
getOrder (Fork o a p l r) = o

fork :: a -> p -> PQueue a p -> PQueue a p -> PQueue a p
fork a p Null Null = Fork 1 a p Null Null
fork a p (Fork _ ax px l r) Null = Fork 1 a p leftTree Null
  where
    leftTree = fork ax px l r
fork a p Null (Fork _ ax px l r) = Fork 1 a p Null rightTree
  where
    rightTree = fork ax px l r
fork a p (Fork _ a1 p1 l1 r1) (Fork _ a2 p2 l2 r2) = Fork (1 + min (getOrder leftTree) (getOrder rightTree)) a p leftTree rightTree
  where
    leftTree = fork a1 p1 l1 r1
    rightTree = fork a2 p2 l2 r2

-- used the idea for the merge algorithm of leftist heap from - https://www.youtube.com/watch?v=zJ2yyRfIkXo&t=27s where first I get the right subtree of the smaller node
-- until one of the child node is a leaf and then I join them back together keeping mind the rank/order property of the leftist heap

joinBack :: a -> p -> PQueue a p -> PQueue a p -> PQueue a p
joinBack a p leftTree Null = Fork (getOrder leftTree) a p leftTree Null
joinBack a p Null rightTree = Fork (getOrder rightTree) a p rightTree Null
joinBack a p leftTree@(Fork o1 a1 p1 l1 r1) rightTree@(Fork o2 a2 p2 l2 r2)
  | o1 >= o2 = Fork (1 + min o1 o2) a p leftTree rightTree
  | otherwise = Fork (1 + min o1 o2) a p rightTree leftTree

mergeQ :: Ord p => PQueue a p -> PQueue a p -> PQueue a p
mergeQ leftTree Null = leftTree
mergeQ Null rightTree = rightTree
mergeQ leftTree@(Fork o1 a1 p1 l1 r1) rightTree@(Fork o2 a2 p2 l2 r2)
  | p1 < p2 = joinBack a1 p1 l1 (mergeQ r1 rightTree)
  | otherwise = joinBack a2 p2 l2 (mergeQ r2 leftTree)

insert :: Ord p => a -> p -> PQueue a p -> PQueue a p
insert a p mainTree = mergeQ mainTree (Fork 1 a p Null Null)

delete :: Ord p => PQueue a p -> ((a, p), PQueue a p)
delete Null = error "Cannot delete from empty Priority Queue"
delete (Fork o a p l r) = ((a, p), mergeQ l r)
