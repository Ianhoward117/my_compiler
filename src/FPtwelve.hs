module FPtwelve (origin, left, isYes, flip2, answers, square2, area, mult, copy, safediv, safehead, nat2int, int2nat, multNat, folde) where

    type Pos = (Int,Int)

    origin :: Pos
    origin = (0,0)

    left :: Pos -> Pos
    left (x,y) = (x-1,y)

    type Pair a = (a,a)

    mult :: Pair Int -> Int
    mult (m,n) = m*n

    copy :: a -> Pair a
    copy x = (x,x)

    -- data Bool = False | True

    data Answer = Yes | No | Unknown
        deriving Show

    isYes :: Answer -> Bool
    isYes Yes = True
    isYes _ = False

    flip2 :: Answer -> Answer
    flip2 Yes = No
    flip2 No = Yes
    flip2 Unknown = Unknown

    answers :: [Answer]
    answers = [Yes,No,Unknown]

    data Shape = Circle Float | Rect Float Float
        deriving Show

    square2 :: Float -> Shape
    square2 n = Rect n n

    area :: Shape -> Float
    area (Circle r) = pi * r^2
    area (Rect x y) = x * y

    -- data Maybe a = Nothing | Just a
    safediv :: Int -> Int -> Maybe Int
    safediv _ 0 = Nothing
    safediv m n = Just (m `div` n)

    safehead :: [a] -> Maybe a
    safehead [] = Nothing
    safehead xs = Just (head xs)

    data Nat = Zero | Succ Nat
        deriving Show

    nat2int :: Nat -> Int
    nat2int Zero = 0
    nat2int (Succ n) = 1 + nat2int n

    int2nat :: Int -> Nat
    int2nat 0 = Zero
    int2nat n = Succ (int2nat (n-1))

    add :: Nat -> Nat -> Nat
    add Zero n = n
    add (Succ m) n = Succ (add m n)

    data Expr = Val Int 
              | Add Expr Expr 
              | Mul Expr Expr
        deriving Show

    size :: Expr -> Int
    size (Val n) = 1
    size (Add x y) = size x + size y
    size (Mul x y) = size x + size y

    eval :: Expr -> Int
    eval (Val n) = n
    eval (Add x y) = eval x + eval y
    eval (Mul x y) = eval x * eval y

    -- Exercises

    -- 1. Using recursion and the add function, define a function that multiplies two natural numbers

    multNat :: Nat -> Nat -> Nat
    multNat Zero m = Zero
    multNat (Succ n) m = add (multNat n m) m
    -- (n+1) * m = n*m + m

    -- 2. Define a suitable function folde for expressions and give a few examples of its use

    folde :: (Int -> a) -> (a -> a -> a) -> (a -> a -> a) -> Expr -> a
    folde f _ _ (Val n) = f n
    folde f g h (Add x y) = g (folde f g h x) (folde f g h y)
    folde f g h (Mul x y) = h (folde f g h x) (folde f g h y)

    -- 3. Define a type Tree a of binary trees built from Leaf values of type a using a Node constructor that takes two binary trees as parameters 

    data Tree a = Leaf a
                | Node (Tree a) (Tree a)