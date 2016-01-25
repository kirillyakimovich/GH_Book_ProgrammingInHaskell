module Exercises where
  
replicate' :: Int -> a -> [a]
replicate' n x = [ x | x' <- [1..n]]