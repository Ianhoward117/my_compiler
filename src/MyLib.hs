module MyLib (someFunc) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Install/upgrade compiler: "ghc" (Haskell compiler)
-- ghcup tui

-- Create a new project: 
-- cabal init --interactive

-- Build the project:
-- cabal build

-- Run the project:
-- cabal run

-- Interactive REPL:
-- cabal repl

data Expr = Val Int | Add Expr Expr
    deriving Show

data Op = PUSH Int | ADD
    deriving Show

eval (Val n) = n
eval (Add x y) = eval x + eval y

comp (Val n) = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]

exec (PUSH n : c) s = exec c (n:s)
exec (ADD : c) (m:n:s) = exec c (n+m:s)
exec _ s = s

-- Add 2 numbers and print the result
main = print (eval (Add (Val 2) (Val 3)))

qsort [] = []
qsort (x:xs) = qsort ys ++ [x] ++ qsort zs
    where
        ys = [a | a <- xs, a <= x]
        zs = [b | b <- xs, b > x]