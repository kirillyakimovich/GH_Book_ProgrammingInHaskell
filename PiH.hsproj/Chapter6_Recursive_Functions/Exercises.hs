module Chapter6_Recursive_Functions.Exercises where
  
-- 1
(|^)     :: (Integral a, Integral b)  => a -> b -> a
n |^ 0 = 1
n |^ p = n * n |^ (p - 1)

-- 2
and'            :: [Bool] -> Bool
and' []         = True
and' (True: xs) = and' xs
and' (False: _) = False
--and' (x : xs) | x == True = and' xs
--              | otherwise = False

concat'          :: [[a]] -> [a]
concat' []       = []
concat' (x : xs) = x ++ concat' xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x

(!!!)        :: [a] -> Int -> a
[]     !!! _ = error "Index too large"
(x:_)  !!! 0 = x
(_:xs) !!! n = xs !!! (n - 1)
--(!!!) (x: xs) n | n == 0 = x
--                | otherwise = xs !!! (n - 1)
                
elem'                      :: Eq a => a -> [a] -> Bool
elem' _ []                 = False
elem' x (y:ys) | x == y    = True
               | otherwise = elem' x ys
         
-- 5
     
merge                           :: Ord a => [a] -> [a] -> [a]
merge xs []                     = xs
merge [] ys                     = ys
merge (x:xs) (y:ys) | x <= y    = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys
                    
halve    :: Ord a => [a] -> ([a], [a])
halve [] = ([], [])
halve xs = (take mid xs, drop mid xs) 
        where mid = (length xs) `div` 2

msort         :: Ord a => [a] -> [a]
msort []      = []
msort [x] = [x]
msort xs      = merge (msort (fst halved)) (msort (snd halved))
            where halved = halve xs

-- 6
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum xs

take'          :: Int -> [a] -> [a]
take' 0 _      = []
take' n []     = error "Index too large"
take' n (x:xs) = x: take' (n-1) xs

last' :: [a] -> a
last' [] = error "Empty list"
last' [x] = x
last' (_:xs) = last' xs





                    