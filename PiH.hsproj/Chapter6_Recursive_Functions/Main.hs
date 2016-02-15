module Chapter6_Recursive_Functions.Main where
  
factorial :: Int -> Int
factorial n = product [1.. n]

factorial' :: Integer -> Integer
factorial' 0 = 1
factorial' (n) = (n) * factorial' (n - 1)

mult :: Int -> Int -> Int
mult m 0 = 0
mult m n = m + (m `mult` (n - 1))

product' :: Num a => [a] -> a
product' [] = 1
product' (n : ns) = n * product' ns

length' :: [a] -> Int
length' [] = 0
length' (_ : ns) = 1 + length' ns

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

append :: [a] -> [a] -> [a]
append [] ys = ys
(x:xs) `append` ys = x : xs `append` ys
--append (x:xs) ys = x : xs `append` ys

insert                      :: Ord a => a -> [a] -> [a]
insert x []                 = [x]
insert x (y:ys) | x <= y    = x : y : ys
                | otherwise = y : (insert x ys)

isort          :: Ord a => [a] -> [a]
isort []       = []
isort (x : xs) = insert x (isort xs)

-- 6.3
zip'               :: [a] -> [b] -> [(a,b)]
zip' [] _          = []
zip' _ []          = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

drop'          :: Int -> [a] -> [a]
drop' 0 xs     = xs
drop' _ []     = []
drop' n (x:xs) = drop' (n - 1) xs

-- 6.4

fibonacci   :: Int -> Int
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

qsort        :: Ord a => [a] -> [a]
qsort [] = []
qsort (x: xs) = qsort smaller ++ [x] ++ qsort bigger
                where smaller = [x' | x' <- xs, x' <= x]
                      bigger = [x' | x' <- xs, x' > x]

-- 6.5 Mutual recursion

even'   :: Int -> Bool
even' 0 = True
even' n = odd' (n - 1)

odd'   :: Int -> Bool
odd' 0 = False
odd' n = even' (n - 1)

evens        :: [a] -> [a]
evens []     = []
evens (x:xs) = x : odds xs

odds        :: [a] -> [a]
odds []     = []
odds (_:xs) = evens xs
