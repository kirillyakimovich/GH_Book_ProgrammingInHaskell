module Chapter7_Higher_order_functions.Ex8 where
  
import Chapter7_Higher_order_functions.String_transmitter
import Chapter7_Higher_order_functions.Exercises
import Data.Char

parity :: [Bit] -> [Bit]
parity xs = xs ++ [mod (sum xs) 2]

--checkparity    :: [Bit] -> [Bit]


encodep :: String -> [Bit]
encodep = concat . map (parity . make8 . int2bin . ord)

chop      :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

--decodep :: [Bit] -> String
  
