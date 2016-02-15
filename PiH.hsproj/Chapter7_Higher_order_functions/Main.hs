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
                    