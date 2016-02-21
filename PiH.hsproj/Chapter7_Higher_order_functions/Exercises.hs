module Chapter7_Higher_order_functions.Exercises where
  
--2
all' :: (a -> Bool) -> [a] -> Bool
all' p  = (foldr (&&) True) . map p

any' :: (a -> Bool) -> [a] -> Bool
any' p = (foldr (||) False) . map p

takeWhile'   :: (a -> Bool) -> [a] -> [a]
takeWhile' p = fst . (foldl f ([], True))
              where f (xs, acc) x | p x && acc = (xs ++ [x], True)
                                  | otherwise = (xs, False)
                                  

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p = fst . (foldl f ([], True))
              where f (xs, acc) x | p x && acc = (xs, True)
                                  | otherwise = (xs ++ [x], False)
                                  
-- 3 Redefine the functions map f and filter p using foldr .
mapf   :: (a -> b) -> [a] -> [b]
mapf f = foldr (\x ys -> f x : ys ) []

filterf :: (a -> Bool) -> [a] -> [a]
filterf p = foldr (\x ys -> if p x then x : ys else ys) []

-- 4
dec2int :: [Int] -> Int
dec2int = foldl (\base x -> base * 10 + x) 0

-- 5
curry'        :: ((a,b) -> c) -> (a -> b -> c)
curry' f x y = f (x, y)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f pair = f (fst pair) (snd pair)

-- 7
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

chop8' :: [Int] -> [[Int]]
chop8' = unfold null (take 8) (drop 8)

map'   :: (a -> b) -> [a] -> [b]
map' f = unfold null (f . head) tail

iterate'   :: (a -> a) -> a -> [a]
iterate' f =  unfold (const False) id f
