module Chapter7_Higher_order_functions.Ex8 where
  
import Chapter7_Higher_order_functions.String_transmitter hiding (encode, decode)
import Chapter7_Higher_order_functions.Exercises
import Data.Char

parityBit :: [Bit] -> Bit
parityBit xs = sum xs `mod` 2

parity :: [Bit] -> [Bit]
parity xs = xs ++ [parityBit xs]

encode :: String -> [Bit]
encode = concat . map (parity . make8 . int2bin . ord)

chop      :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

checkParity xs | parityBit (init xs) == last xs = init xs
               | otherwise = error "Does not match"

decode :: [Bit] -> String
decode = map (chr . bin2int . checkParity ). (chop 9)

faultyChannel :: [Bit] -> [Bit]
faultyChannel = tail

faultyTransmit :: String -> String
faultyTransmit = decode . faultyChannel . encode


