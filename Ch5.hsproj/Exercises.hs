module Exercises where
  
replicate' :: Int -> a -> [a]
replicate' n x = [ x | x' <- [1..n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x * x + y * y == z *z]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], isPerfect x]
  where isPerfect n = (sum (init (factors n)) == n)