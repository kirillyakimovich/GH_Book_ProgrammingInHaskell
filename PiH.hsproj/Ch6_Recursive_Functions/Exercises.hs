module Ch6_Recursive_Functions.Exercises where
  
-- 1
(|^)     :: (Integral a, Integral b)  => a -> b -> a
(|^) n 0 = 1
(|^) n p = n * n |^ (p - 1)

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
              

