module Chapter7_Higher_order_functions.Main where
 
add     :: Int -> Int -> Int
add x y = x + y

add' :: Int -> (Int -> Int)
add' = \x -> (\y -> x + y)

twice     :: (a -> a) -> a -> a
twice f x = f (f x)

-- 7.2
map'      :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

-- recursive functions are better for reasoning
mapr          :: (a -> b) -> [a] -> [b]
mapr f []     = []
mapr f (x:xs) = f x : mapr f xs

filter'      :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]

filterr :: (a -> Bool) -> [a] -> [a]
filterr _ [] = []
filterr p (x:xs) | p x       = x : filterr p xs
                 | otherwise = filter p xs
                 
sumsqreven    :: [Int] -> Int
sumsqreven xs = sum (map (^2) (filter even xs))

all'      :: (a -> Bool) -> [a] -> Bool
all' p xs = and [p x | x <- xs] 

allr                      :: (a -> Bool) -> [a] -> Bool
allr p []                 = True
allr p (x:xs) | p x       = allr p xs
              | otherwise =  False

any' :: (a -> Bool) -> [a] -> Bool
any' p xs = or [p x | x <- xs]

anyr                      :: (a -> Bool) -> [a] -> Bool
anyr _ []                 = False
anyr p (x:xs) | p x       = True
              | otherwise = anyr p xs
              
takeWhiler             :: (a -> Bool) -> [a] -> [a]
takeWhiler _ []        = []
takeWhiler p (x:xs)
           | p x       = x : takeWhiler p xs
           | otherwise = []
           
dropWhiler             :: (a -> Bool) -> [a] -> [a]
dropWhiler _ []        = []
dropWhiler p (x:xs)
           | p x       = dropWhiler p xs
           | otherwise = xs
           
-- 7.6
sumf :: Num a => [a] -> a
sumf [] = 0
sumf (x:xs) = x + sumf xs

productf :: Num a => [a] -> a
productf [] = 1
productf (x:xs) = x * productf xs

orf :: [Bool] -> Bool
orf [] = False
orf (x:xs) = x || orf xs

andf :: [Bool] -> Bool
andf [] = True
andf (x:xs) = x && andf xs

sumf' = foldr (+) 0
-- sumf' xs = foldr (+) 0 xs
productf' = foldr (*) 1
orf' = foldr (||) False
andf' = foldr (&&) True

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)

length' = foldr' (\ _ n -> 1 + n) 0
reverse' = foldr' (\x xs -> xs ++ [x]) []

--7.4
suml = sum' 0
        where
          sum' v [] = v
          sum' v (x:xs) = sum' (v + x) xs
          
sumfl = foldl (+) 0
productfl = foldl (*) 1
orfl = foldl (||) False
andfl = foldl (&&) True

foldl'            :: (a -> b -> a) -> a -> [b] -> a
foldl' f v []     = v
foldl' f v (x:xs) = foldl' f (f v x) xs 
                    
--7.5 as composition is already defined, .|.
(.|.) :: (b -> c) -> (a -> b) -> (a -> c)
f .|. g = \x -> f (g x)

compose :: [(a -> a)] -> (a -> a)
compose = foldr (.) id
