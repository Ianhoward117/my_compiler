module Functions (double, quadruple, factorial, average, n, last_new2, initNew, second, swap, pair, palindrome, twice, quadrupleNew, product1, qsortRev, qsort2, doubleTwice, tail_new, init_new, init_new2, bools, nums, add, copy, apply, second_2, swap_2, pair_2, double_2, palindrome_2, twice_2, safetail, safetail_guarded, safetail_matching, halve, third, third_2, third_3, safetail_2, safetail_3, safetail_4, luhnDouble, luhn, firsts, prime, factors, primes, find, pairs, sorted, positions, lowers, count, pyths, perfects, scalarProduct, squares, grid, square, replicate_new, ex, better_ex, positions_new, fac, product_new, length_new, reverse_new, zip_new, drop_new, qsort_new, and_new, concat_new, replicate_new2, elem_new, insert, isort, merge, msort, fac_neg, sumdown, exp_new, euclid, and_new2, merge_new, msort_new, origin, left, answers, square2, area, isYes) where

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

    Chapter 4

        Exercises

            (1)
-}
    halve :: [a] -> ([a],[a])
    halve xs = (take ((length xs) `div` 2) xs, drop ((length xs) `div` 2) xs)

--          (2)

    third :: [a] -> a
    third xs = head (tail (tail xs))

    third_2 :: [a] -> a
    third_2 xs = xs !! 2

    third_3 :: [a] -> a
    third_3 (_:_:x:_) = x

--          (3)

    safetail_2 :: [a] -> [a]
    safetail_2 xs = if length xs == 0 then [] else tail xs

    safetail_3 :: [a] -> [a]
    safetail_3 xs | length xs == 0 = []
                  | otherwise = tail xs
    
    safetail_4 :: [a] -> [a]
    safetail_4 [] = []
    safetail_4 (xs:x) = x

{-          (4)

                (||) :: Bool -> Bool -> Bool
                True || True = True
                True || False = True
                False || True = True
                False || False = False

                (||) :: Bool -> Bool -> Bool
                False || False = False
                _ || _ = True

                (||) :: Bool -> Bool -> Bool
                False || a = a
                True || _ = True

                (||) :: Bool -> Bool -> Bool
                b || c | b == c    = b
                       | otherwise = True

            (5)

                (&&) :: Bool -> Bool -> Bool
                True && True = True
                _    && _    = False

                (&&) :: Bool -> Bool -> Bool
                a && b = if a == True then
                         if b == True then True else False
                         else False

            (6)

                (&&) :: Bool -> Bool -> Bool
                True && b = b
                False && _ = False

                (&&) :: Bool -> Bool -> Bool
                a && b = if a == True then b else False

            (7)

                mult :: Int -> Int -> Int -> Int
                mult x y z = x*y*z

                mult :: Int -> Int -> Int -> Int
                mult = \x -> (\y -> (\z = x * y * z))

            (8)
-}

    luhnDouble :: Int -> Int
    luhnDouble n | n*2 > 9 = n*2 - 9
                 | otherwise = n*2
    
    luhn :: Int -> Int -> Int -> Int -> Bool
    luhn a b c d = if (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0 then True else False

--  Chapter 5

    firsts :: [(a,b)] -> [a]
    firsts ps = [x | (x,_) <- ps]

    factors :: Int -> [Int]
    factors n = [x | x <- [1..n], n `mod` x == 0]

    prime :: Int -> Bool
    prime n = factors n == [1,n]

    primes :: Int -> [Int]
    primes n = [x | x <- [1..n], prime x]

    find :: Eq a => a -> [(a,b)] -> [b]
    find k t = [v | (k', v) <- t, k == k']

    pairs :: [a] -> [(a,a)]
    pairs xs = zip xs (tail xs)

    sorted :: Ord a => [a] -> Bool
    sorted xs = and [x <= y | (x,y) <- pairs xs]

    positions :: Eq a => a -> [a] -> [Int]
    positions x xs = [i | (x', i) <- zip xs [0..], x == x']

    lowers :: String -> Int
    lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

    count :: Char -> String -> Int
    count x xs = length [x' | x' <- xs, x == x']

--  exercises

--  (1)

    squares :: Int
    squares = sum [x^2 | x <- [1..100]]

-- (2)

    grid :: Int -> Int -> [(Int, Int)]
    grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

-- (3)

    square :: Int -> [(Int, Int)]
    square n = [(x,y) | (x,y) <- grid n n, x /= y]

--  (4)

    replicate_new :: Int -> a -> [a]
    replicate_new n x = [x | _ <- [1..n]]

--  (7)

    ex :: [(Int, Int)]
    ex = [(x,y) | x <- [1,2], y <- [3,4]]

    better_ex :: [(Int, Int)]
    better_ex = concat [[(x,y) | y <- [3,4]] | x <- [1,2]]

--  (8)

    positions_new :: Eq a => a -> [a] -> [Int]
    positions_new x xs = find x (zip xs [0..])

{-  FP 7 exercises

    (1)
-}
    pyths :: Int -> [(Int,Int,Int)]
    pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

--  (2)

    perfects :: Int -> [Int]
    perfects n = [x | x <- [1..n], (sum (factors x)) - x == x]

--  (3)

    scalarProduct :: [Int] -> [Int] -> Int
    scalarProduct xs ys = sum [x*y | (x,y) <- zip xs ys]

--  FP 8

    fac :: (Eq t, Num t) => t -> t
    fac 0 = 1
    fac n = n * fac (n-1)

    product_new :: Num t => [t] -> t
    product_new [] = 1
    product_new (n:ns) = n * product_new ns

    length_new :: [a] -> Int
    length_new [] = 0
    length_new (_:xs) = 1 + length_new xs

    reverse_new :: [a] -> [a]
    reverse_new [] = []
    reverse_new (x:xs) = reverse_new xs ++ [x]

    zip_new :: [a] -> [b] -> [(a,b)]
    zip_new [] _ = []
    zip_new _ [] = []
    zip_new (x:xs) (y:ys) = (x,y) : zip_new xs ys

    drop_new :: Int -> [a] -> [a]
    drop_new 0 xs = xs
    drop_new _ [] = []
    drop_new n (_:xs) = drop_new (n-1) xs

    qsort_new :: Ord a => [a] -> [a]
    qsort_new [] = []
    qsort_new (x:xs) = qsort_new larger ++ [x] ++ qsort_new smaller where
        larger = [a | a <- xs, a > x]
        smaller = [b | b <- xs, b <= x]

--  Exercises

--  (1)

    and_new :: [Bool] -> Bool
    and_new [] = True
    and_new (b:bs) = b && and_new bs

    concat_new :: [[a]] -> [a]
    concat_new [] = []
    concat_new (xs:xss) = xs ++ concat_new xss

    replicate_new2 :: Int -> a -> [a]
    replicate_new2 0 x = []
    replicate_new2 n x = x : replicate_new2 (n-1) x
{- 
    (!!) :: [a] -> Int -> a
    (!!) (x:_) 0 = x
    (!!) (_:xs) n = xs !! (n-1)
-}

    elem_new :: Eq a => a -> [a] -> Bool
    elem_new x [] = False
    elem_new x (y:ys) | x == y = True
                      | otherwise = elem_new x ys

    insert :: Int -> [Int] -> [Int]
    insert x [] = [x]
    insert x (y:ys) | x <= y = x : y : ys
                    | otherwise = y : insert x ys

    isort :: [Int] -> [Int]
    isort [] = []
    isort (x:xs) = insert x (isort xs)

    merge :: [Int] -> [Int] -> [Int]
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                        | otherwise = y : merge (x:xs) ys
    
    msort :: [Int] -> [Int]
    msort [] = []
    msort [x] = [x]
    msort xs = merge (msort (take ((length xs) `div` 2) xs)) (msort (drop ((length xs) `div` 2) xs))

--  Chapter 6

--  Exercises

--  (1)

    fac_neg :: Int -> Int
    fac_neg 0 = 1
    fac_neg n | n > 0 = n * fac_neg (n-1)

--  (2)

    sumdown :: Int -> Int
    sumdown 0 = 0
    sumdown n = n + sumdown (n-1)

--  (3)

    exp_new :: Int -> Int -> Int
    exp_new m 0 = 1
    exp_new m n = m * (exp_new m (n-1))

--  (4)

    euclid :: Int -> Int -> Int
    euclid m n | m == n = m
               | m < n = euclid m (n-m)
               | n < m = euclid (m-n) n

{-  (5)

    length :: [a] -> Int
    length [] = 0
    length (_:xs) = 1 + length xs

    length [1,2,3]
        = 1 + length [2,3]
        = 1 + 1 + length [3]
        = 1 + 1 + 1 + length []
        = 1 + 1 + 1 + 0
        = 3

    drop :: Int -> [a] -> [a]
    drop 0 xs = xs
    drop _ [] = []
    drop n (_:xs) = drop (n-1) xs

    drop 3 [1,2,3,4,5]
        = drop 2 [2,3,4,5]
        = drop 1 [3,4,5]
        = drop 0 [4,5]
        = [4,5]

    init :: [a] -> [a]
    init [_] = []
    init (x:xs) = x : init xs

    init [1,2,3]
        = 1 : init [2,3]
        = 1 : 2 : init [3]
        = 1 : 2 : []
        = [1,2]
-}
--  (6)

    and_new2 :: [Bool] -> Bool
    and_new2 [] = True
    and_new2 (x:xs) | x == False = False
               | x == True = and_new2 xs

--  (7)

    merge_new :: Ord a => [a] -> [a] -> [a]
    merge_new xs [] = xs
    merge_new [] ys = ys
    merge_new (x:xs) (y:ys) | x <= y = x : merge_new xs (y:ys)
                            | y < x = y : merge_new ys (x:xs)

-- (8)

    msort_new :: Ord a => [a] -> [a]
    msort_new [] = []
    msort_new [x] = [x]
    msort_new xs = merge_new (msort_new (take ((length xs) `div` 2) xs)) (msort_new (drop ((length xs) `div` 2) xs))

-- FP 10: Higher Order Functions

{-
    map :: (a -> b) -> [a] -> [b]
    map f xs = [f x | x <- xs]

    map f [] = []
    map f (x:xs) = f x : map f xs

    filter :: (a -> Bool) -> [a] -> [a]
    filter p xs = [x | x <- xs, p x]

    filter p [] = []
    filter p (x:xs) | p x = x : filter p xs
                    | otherwise = filter p xs


    sum [] = 0
    sum (x:xs) = x + sum xs

    product [] = 1
    product (x:xs) = x * product xs

    and [] = True
    and (x:xs) = x && and xs


    sum = foldr (+) 0

    product = foldr (*) 1

    or = foldr (||) false

    and = foldr (&&) true

    foldr :: (a -> b -> b) -> b -> [a] -> b
    foldr f v [] = v
    foldr f v (x:xs) = f x (foldr f v xs)


    length :: [a] -> Int
    length [] = 0
    length (_:xs) = 1 + length xs

    length = foldr (\_ n -> 1+n) 0

    reverse :: [a] -> [a]
    reverse [] = []
    reverse (x:xs) = reverse xs ++ [x]

    reverse = foldr (\x xs -> xs ++ [x]) []

    (++ ys) = foldr (:) ys


    (.) :: (b -> c) -> (a -> b) -> (a -> c)
    f . g = \x -> f (g x)

    odd :: Int -> Bool
    odd = not . even

    all :: (a -> Bool) -> [a] -> Bool
    all p xs = and [p x | x <- xs]

    all even [2,4,6,8,10]
        True

    any :: (a -> Bool) -> [a] -> Bool
    any p xs = or [p x | x <- xs]

    any (== ' ') "abc def"
        True

    takeWhile :: (a -> Bool) -> [a] -> [a]
    takeWhile p [] = []
    takeWhile p (x:xs) | p x = x : takeWhile p xs
                       | otherwise = []

    takeWhile (/= ' ') "abc def"
        "abc"

    dropWhile :: (a -> Bool) -> [a] -> [a]
    dropWhile p [] = []
    dropWhile p (x:xs) | p x = dropWhile p xs
                       | otherwise = x:xs

    dropWhile (== ' ') "   abc"
        "abc"
-}

{-  
    Exercises

    (1)

    Higher order functions that return functions as results are better known as curried functions.

    (2)

    [f x | x <- xs, p x]
    ==
    map f (filter p xs)

    (3)

    map :: (a -> b) -> [a] -> [b]
    map f [] = []
    map f (x:xs) = f x : map f xs

    map f (x:xs) = foldr (\x xs -> f x : xs) [] (x:xs)
-}

{-
    Chapter 7: Higher Order Functions

    sumsqreven :: [Int] -> Int
    sumsqreven ns = sum (map (^2) (filter even ns))
-}

{-
    Exercises

    (1)

    [f x | x <- xs, p x]

    map f (filter p xs)

    (2)

    all :: (a -> Bool) -> [Bool] -> Bool
    all p = and . map p

    any :: (a -> Bool) -> [Bool] -> Bool
    any p = or . map p

    takeWhile :: (a -> Bool) -> [a] -> [a]
    takeWhile _ [] = []
    takeWhile p (x:xs) | p x = x : takeWhile p xs
                       | otherwise = []

    dropWhile :: (a -> Bool) -> [a] -> [a]
    dropWhile _ [] = []
    dropWhile p (x:xs) | p x = dropWhile p xs
                       | otherwise = x:xs
    
    (3)

    map :: (a -> b) -> [a] -> [b]
    map f [] = []
    map f (x:xs) = f x : map f xs

    map f = foldr (\x xs -> f x : xs) []


    filter p [] = []
    filter p (x:xs) | p x = x : filter p xs
                    | otherwise = filter p xs

    filter p = foldr (\x xs -> if p x then x:xs else xs) []

    (4)

    dec2int :: [Int] -> Int
    dec2int = foldl (\x y -> 10*x + y) 0

    (5)

    curry :: ((a,b) -> c) -> a -> b -> c
    curry f = \x y -> f (x,y)

    uncurry :: (a -> b -> c) -> (a,b) -> c
    uncurry f = \(x,y) -> f x y
-}

{-
    FP 12 - Declaring Types and Classes

    type String = [Char]
-}

    type Pos = (Int,Int)

    origin :: Pos
    origin = (0,0)

    left :: Pos -> Pos
    left (x,y) = (x-1,y)

    -- type Pair a = (a,a)

    -- mult :: Pair Int -> Int
    -- mult (m,n) = m*n

    -- copy :: a -> Pair a
    -- copy x = (x,x)

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