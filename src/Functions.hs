module Functions (double, quadruple, factorial, average, n, last_new2, initNew, second, swap, pair, palindrome, twice, quadrupleNew, product1, qsortRev, qsort2, doubleTwice, tail_new, init_new, init_new2, bools, nums, add, copy, apply, second_2, swap_2, pair_2, double_2, palindrome_2, twice_2, safetail, safetail_guarded, safetail_matching) where

-- FP4

-- lecture
    -- double x = x + x

    quadruple x = double (double x)

    factorial n = product [1..n]

    average ns = sum ns `div` length ns

    n = a `div` length xs
        where
            a = 10
            xs = [1,2,3,4,5]

-- exercises

    last_new2 xs = xs !! (length xs - 1)

    initNew xs = take (length xs - 1) xs

-- FP5

-- lecture
    -- fst :: (a,b) -> a
    -- head :: [a] -> a
    -- take :: Int -> [a] -> [a]
    -- zip :: [a] -> [b] -> [(a,b)]
    -- id :: a -> a'

-- exercises

    -- (1)
    -- ['a', 'b', 'c'] :: [Char]
    -- ('a', 'b', 'c') :: (Char, Char, Char)
    -- [(False, '0'), (True, '1')] :: [(Bool, Char)]
    -- ([False, True], ['0', '1']) :: ([Bool], [Char])
    -- [tail, init, reverse] :: [[a] -> [a]]

    -- (2)
    second :: [a] -> a
    second xs = head (tail xs)

    swap :: (a, b) -> (b, a)
    swap (x,y) = (y,x)

    pair :: a -> b -> (a,b)
    pair x y = (x,y)

    double :: Num a => a -> a
    double x = x*2

    palindrome :: Eq a => [a] -> Bool
    palindrome xs = reverse xs == xs

    twice :: (a -> a) -> a -> a
    twice f x = f (f x)
    
    doubleTwice :: Num a => a -> a
    doubleTwice = twice double

-- chapter 1

    -- (1.7)

    -- (1)
    quadrupleNew :: Num a => a -> a
    quadrupleNew x = x*4

    -- (2)
    -- sum [] = 0
    -- sum (x:xs) = x + sum xs

    -- (3)
    product1 :: Num a => [a] -> a
    product1 [] = 1
    product1 (x:xs) = x * product1 xs

    -- (4)
    qsortRev :: Ord a => [a] -> [a]
    qsortRev [] = []
    qsortRev (x:xs) = qsortRev larger ++ [x] ++ qsortRev smaller where
        larger = [b | b <- xs, b > x]
        smaller = [a | a <- xs, a <= x]

    -- (5)
    qsort2 :: Ord a => [a] -> [a]
    qsort2 [] = []
    qsort2 (x:xs) = qsort2 smaller ++ [x] ++ qsort2 larger where
        smaller = [a | a <- xs, a < x]
        larger = [b | b <- xs, b > x]

{- 

chapter 2

    (2.7)
        
        (2)
        
            (2^3)*4

            (2*3)+(4*5)

            2+(3*(4^5))

        (3)

            n = a `div` length xs
                where
                    a = 10
                    xs = [1,2,3,4,5]

        (4)
-}
    tail_new :: [a] -> a
    tail_new xs = head (reverse xs)

--      (5)

    init_new :: [a] -> [a]
    init_new xs = reverse (drop 1 (reverse xs))

    init_new2 :: [a] -> [a]
    init_new2 xs = take (length xs - 1) xs
{- 

chapter 3

    (3.11)

        (1)

            ['a','b','c'] :: [Char]

            ('a','b','c') :: (Char, Char, Char)

            [(False, '0'), (True, '1')] :: [(Bool, Char)]

            [tail, init, reverse] :: [[a] -> [a]]

        (2)
-}
    bools :: [Bool]
    bools = [False, True, False]

    nums :: [[Int]]
    nums = [[1,2], [3,4]]

    add :: Int -> Int -> Int -> Int
    add x y z = x+y+z

    copy :: a -> (a,a)
    copy x = (x,x)

    apply :: (a -> b) -> a -> b
    apply f x = f x

--      (3)

    second_2 :: [a] -> a
    second_2 xs = head (tail xs)

    swap_2 :: (a,b) -> (b,a)
    swap_2 (x,y) = (y,x)

    pair_2 :: a -> b -> (a,b)
    pair_2 x y = (x,y)

    double_2 :: Num a => a -> a
    double_2 x = x*2

    palindrome_2 :: Eq a => [a] -> Bool
    palindrome_2 xs = reverse xs == xs

    twice_2 :: (t -> t) -> t -> t
    twice_2 f x = f (f x)
{-
        (5)

            In general, it is not feasible for function types to be instances of the Eq class because functions can not be compared for equality.

            It is feasible for function types to be instances of the Eq class when comparing a function to itself.

    FP6

        Conditional Expressions

            abs :: Int -> Int
            abs n = if n >= 0 then n else -n

            signum :: Int -> Int
            signum n = if n < 0 then -1 else
                            if n == 0 then 0 else 1

            abs n | n >= 0 = n
                  | otherwise = -n

            signum n | n < 0 = -1
                     | n == 0 = 0
                     | otherwise = 1

        Pattern Matching
        
            not :: Bool -> Bool
            not False = True
            not True = False

            (&&) :: Bool -> Bool -> Bool
            True && True = True
            True && False = False
            False && True = False
            False && False = False

            (&&) :: Bool -> Bool -> Bool
            True && True = True
            _ && _ = False

            (&&) :: Bool -> Bool -> Bool
            False && _ = False
            True && a = a
        
        List Patterns

            head :: [a] -> a
            head (x:_) = x

            tail :: [a] -> [a]
            tail (_:xs) = xs

        Lambda Expressions

            \x = x + x

            add :: Int -> Int -> Int
            add x y = x + y
            
            add :: Int -> (Int -> Int)
            add = \x -> (\y -> x + y)

            odds n = map f [0..n-1]
                where
                    f x = x*2 + 1

            odds n = map (\x -> x*2 + 1) [0..n-1]

        Operator Sections

            1+2 = 3
            (+) 1 2 = 3

            (1+) 2 = 3
            (+2) 1 = 3

            (1+) - successor function
            (1/) - reciprocation function
            (*2) - doubling function
            (/2) - halving function

        Exercises

            (1)
                (a)
-}
    safetail :: [a] -> [a]
    safetail xs = if null xs then [] else tail xs

--              (b)

    safetail_guarded :: [a] -> [a]
    safetail_guarded xs | null xs = []
                        | otherwise = tail xs

--              (c)

    safetail_matching :: [a] -> [a]
    safetail_matching [] = []
    safetail_matching (_:xs) = xs

{-          (2)

    (||) :: Bool -> Bool -> Bool
    False || False = False
    False || True = True
    True || False = True
    True || True = True

    (||) :: Bool -> Bool -> Bool
    False || False = False
    _ || _ = True

    (||) :: Bool -> Bool -> Bool
    _ || True = True
    False || a = a

            (3)

    (&&) :: Bool -> Bool -> Bool
    True && True = True
    _    && _    = False
    
    (&&) :: Bool -> Bool -> Bool
    a && b = if a == True then
                if b == True then True else False
             else False

            (4)

    (&&) :: Bool -> Bool -> Bool
    True && b = b
    False && _ = False

    (&&) :: Bool -> Bool -> Bool
    a && b = if a == True then b else False
-}
