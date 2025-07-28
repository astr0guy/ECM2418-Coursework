 import Data.List

generator2 :: [([Char],[Char],[Char],[Char],[Char])]
generator2
    = (map to_strings a)
    where a = int_generator2
    
int_generator2 :: [(Int,Int,Int,Int,Int)]
int_generator2 
    = [ (a,b,c,d,e)
    | a <- [123..987]
    , not ( 0 `elem` (digits a))
    , digits a == nub (digits a)
    , b <- map invert_digits (permutations (init (digits a)) ++ permutations (tail (digits a)) ++ permutations([head (digits a), last (digits a)]))
    , not (last (digits b) == last (digits a))
    , c <- map invert_digits (permutations (digits a))
    , d <- map invert_digits (permutations (init (digits a)) ++ permutations (tail (digits a)) ++ permutations([head (digits a), last (digits a)]))
    , e <- map invert_digits (permutations (digits a))
    ]

to_strings :: (Int,Int,Int,Int,Int) -> ([Char],[Char],[Char],[Char],[Char])
to_strings (a,b,c,d,e)
    = ((show a), (show b), (show c), (show d), (show e))

digits :: Int -> [Int]
digits x
    |x < 10
    = [x]
    
digits x
    = b : digits a
    where (a,b) = divMod x 10
    
invert_digits :: [Int] -> Int
invert_digits [] = 0

invert_digits (x:xs)
    = x * 10 ^ (length xs) + invert_digits xs
    
tester2 :: ([Char],[Char],[Char],[Char],[Char]) -> Bool
tester2 (a,b,c,d,e)
    = (read a) - (read b) == (read c)
    && (read c) - (read d) == (read e)
    && (read a) + (read c) + (read e) < 2000
    
x_generator2 :: Int
x_generator2
    = length [t | t <- ts, t `elem` g]
    where
        g = generator2 
        ts = [ ("123","21","123","12","123") , ("162","26","261","12","621") , ("219","19","912","21","291") , ("329","92","932","32","239") , ("439","94","394","43","394") , ("549","95","945","95","945") , ("568","68","586","56","586") , ("769","67","679","97","796") , ("879","79","897","98","789") , ("987","79","789","79","789") ]
    
x_tester2 :: Int
x_tester2 =
    length [t | t <- ts, tester2 t]
    where ts = [ ("138","01","137","50","87"), ("143","01","142","52","90") , ("171","02","169","79","90") , ("152","03","149","54","95") , ("159","04","155","61","94") , ("161","05","156","63","93") , ("182","06","176","80","96") , ("151","07","144","57","87") , ("165","08","157","64","93") , ("174","09","165","71","94") ]
    
main :: IO()
main
    = print (filter tester2 generator2)
