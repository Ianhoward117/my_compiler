module FPtwelve (origin, left, isYes, flip2, answers, square2, area, mult, copy, safediv, safehead, nat2int, int2nat, multNat, folde, p1, p2, p3, p4, find, eval_sub, vars, bools, rmdups, substs, isTaut, value, eval_abs, exec, value_abs) where

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
                | Node (Tree a) a (Tree a)

    -- Ch 8

    -- 8.6 Tautalogy checker

    data Prop = Const Bool
              | Var Char
              | Not Prop
              | And Prop Prop
              | Imply Prop Prop
        deriving Show

    p1 :: Prop
    p1 = And (Var 'A') (Not (Var 'A'))

    p2 :: Prop
    p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

    p3 :: Prop
    p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

    p4 :: Prop
    p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

    type Assoc k v = [(k,v)]

    type Subst = Assoc Char Bool

    find :: Eq k => k -> Assoc k v -> v
    find k t = head [v | (k',v) <- t, k == k']

    eval_sub :: Subst -> Prop -> Bool
    eval_sub _ (Const b) = b
    eval_sub s (Var x) = find x s
    eval_sub s (Not p) = not (eval_sub s p)
    eval_sub s (And p q) = eval_sub s p && eval_sub s q
    eval_sub s (Imply p q) = eval_sub s p <= eval_sub s q

    vars :: Prop -> [Char]
    vars (Const _) = []
    vars (Var x) = [x]
    vars (Not p) = vars p
    vars (And p q) = vars p ++ vars q
    vars (Imply p q) = vars p ++ vars q

    bools :: Int -> [[Bool]]
    bools 0 = [[]]
    bools n = map (False:) bss ++ map (True:) bss
        where bss = bools (n-1)

    rmdups :: Eq a => [a] -> [a]
    rmdups [] = []
    rmdups (x:xs) = x : filter (/= x) (rmdups xs)

    substs :: Prop -> [Subst]
    substs p = map (zip vs) (bools (length vs))
        where vs = rmdups (vars p)

    isTaut :: Prop -> Bool
    isTaut p = and [eval_sub s p | s <- substs p]

    -- 8.7 Abstract machine

    value :: Expr -> Int
    value (Val n) = n
    value (Add x y) = value x + value y

    type Cont = [Op]

    data Op = EVAL Expr
            | ADD Int
        deriving Show

    eval_abs :: Expr -> Cont -> Int
    eval_abs (Val n) c = exec c n
    eval_abs (Add x y) c = eval_abs x (EVAL y : c)

    exec :: Cont -> Int -> Int
    exec [] n = n
    exec (EVAL y : c) n = eval_abs y (ADD n : c)
    exec (ADD n : c) m = exec c (n+m)

    value_abs :: Expr -> Int
    value_abs e = eval_abs e []

    -- Exercises

    -- 2. 

    occurs :: Ord a => a -> Tree a -> Bool
    occurs x (Leaf y)     = x == y
    occurs x (Node l y r) = case compare x y of
                                LT -> occurs x l
                                EQ -> True
                                GT -> occurs x r

    -- 3.

    