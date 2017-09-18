-----------------------------------------
-- Hutton  7.6  Binary string transmitter
-----------------------------------------

module BST where

import Data.Char


type Bit = Int

-- binary numbers are represented as lists of bits
-- in a reversed order

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

-- alternative straightforward implementation
-- bin2int bits = sum [w*b | (w,b) <- zip weights bits]
--   where weights = iterate (*2) 1

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = (n `mod` 2) : int2bin (n `div` 2)

-- make all the numbers eight bits
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)


-----------------------------------------------

encode :: String -> [Bit]
encode = concatMap (make8 . int2bin . ord)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

-- simulate perfect channel
channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String
transmit = decode . channel . encode