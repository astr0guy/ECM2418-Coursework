import Data.List

x_generator1 :: Int
x_generator1 =
    length [t | t <- ts , t `elem ` g]
    where
    g = generator1
    ts =
        [ ( 2 ,15 ,14 ,11)
        , ( 4 ,31 ,27 , 9)
        , ( 6 ,47 ,10 , 8)
        , ( 9, 3,23, 6)
        , (11 ,19 , 6, 5)
        , (13 ,35 ,19 , 3)
        , (15 ,51 , 2, 2)
        , (18 , 6 ,16 ,12)
        , (20 ,22 ,29 ,10)
        , (22 ,38 ,11 , 9)
        ]
        
x_tester1 :: Int
x_tester1 =
    length [t | t <- ts , tester1 t]
    where
    ts =
        [ ( 6 ,59 ,17 ,24)
        , ( 6 ,59 ,17 ,34)
        , ( 6 ,59 ,27 ,14)
        , ( 6 ,59 ,27 ,41)
        , ( 8 ,59 ,12 ,46)
        , (16 ,59 , 7 ,24)
        , (16 ,59 , 7 ,42)
        , (16 ,59 , 7 ,43)
        , (16 ,59 ,27 ,40)
        , (18 ,59 , 2 ,46)
        ]

generator1 :: [(Int,Int,Int,Int)]

generator1 
    = [ (a,b,c,d)
    | a <- [1..23]
    , b <- [1..59]
    , c <- [1..31]
    , d <- [1..12]
    ]
    
next_day :: (Int,Int) -> [(Int,Int,Int,Int)]

next_day (c, d)
    = [ (a,b,c1,d)
    | a <- [1..23]
    ,  b <- [1..59]
    ,  c1 <- [c+1]
    , d <- [d]
    ]

predicate1 :: (Int,Int,Int,Int) -> Bool

predicate1 (a,b,c,d)
    = nodups (toList (a,b,c,d))
    && prime (total_lights_displayed (toList (a,b,c-1,d)))


amid :: (Int,Int,Int)-> Bool

amid (a, b, c)
    |b == c && a == b = True
    |b < c && c - a == a - b = True
    |b > c && b - a == a - c = True
    |otherwise = False


next_minute :: (Int,Int,Int,Int) -> (Int,Int,Int,Int)

next_minute (a,b,c,d)
    | b+1 < 60 = (a,b+1,c,d)
    | otherwise = (a+1,0,c,d)

prime :: Int -> Bool

prime
    = not . factorisable 2

factorisable :: Int -> Int -> Bool

factorisable f n
    |f * f <= n = n `mod` f == 0 || factorisable (f+1) n
    |otherwise = False


toList :: (Int, Int, Int, Int) -> [Int]

toList (a,b,c,d)
    = [a1, a2, b1, b2, c1, c2, d1, d2]
    where
        (a1, a2) = divMod a 10
        (b1, b2) = divMod b 10
        (c1, c2) = divMod c 10
        (d1, d2) = divMod d 10


total_lights_displayed :: [Int] -> Int

total_lights_displayed [] = 0

total_lights_displayed (x:xs)
    = lights_displayed x + total_lights_displayed xs


lights_displayed :: Int -> Int

lights_displayed a
    | a == 0 = 6
    | a == 1 = 2
    | a == 2 = 5
    | a == 3 = 5
    | a == 4 = 4
    | a == 5 = 5
    | a == 6 = 6
    | a == 7 = 3
    | a == 8 = 7
    | a == 9 = 6
    |otherwise = 0

nodups :: Eq a => [a] -> Bool

nodups s
    = s == nub s

memberSet :: Int -> [Int] -> Bool

memberSet e [] = False
memberSet e (x:xs)
    | e == x = True
    |otherwise = memberSet e xs


tester1 :: (Int,Int,Int,Int) -> Bool

tester1 (a,b,c,d)
    = nodups (toList(a,b,c,d))
    && prime (total_lights_displayed (toList (a,b,c,d)))
    &&  amid ( total_lights_displayed ( toList ( next_minute  (a,b,c+1,d  )))  , ( total_lights_displayed ( toList ( a,b,c+1,d ))),  total_lights_displayed ( toList ( a,b,c,d )))

main :: IO()
main
    = putStrLn (show  (x_tester1))
