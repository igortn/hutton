--------------------------------
-- Hutton 7.6  Voting algorithms
--------------------------------

module Voting where

import Data.List

--------------------------------------------------------
-- Larger number of votes wins.

-- how many times an item is contained
-- in the list
count :: Eq a => a -> [a] -> Int
count x = length . filter (x ==)

removeDupes :: Eq a => [a] -> [a]
removeDupes [] = []
removeDupes (x:xs) = x : filter (x /=) (removeDupes xs)

result :: Ord a => [a] -> [(Int, a)]
result votes = sort [(count c votes, c) | c <- removeDupes votes]

winner :: Ord a => [a] -> a
winner = snd . last . result

