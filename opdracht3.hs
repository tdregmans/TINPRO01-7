module Opdracht3 where

-- import Data.List for Opdracht 3.2
import Data.List

-- Opdracht 3.1a
differentieer :: (Double -> Double) -> Double -> Double -> Double
differentieer f p x = (f (x+p) - f x) / p

-- Opdracht 3.1b
xis :: Double -> Double -> Double -> [Double]
xis a b p = [a+((b-a)*p*i)|i<-[0..(1/p)-1]]
-- xis return all xi values that correspond with a, b and p

integreer :: (Double -> Double) -> Double -> Double -> Double -> Double
integreer f a b p =
    let dxi = (b-a)*p
    in sum [(f xi) * dxi|xi<-(xis a b p)]

-- Opdracht 3.2
dubbelen :: Eq a => [a] -> [a]
dubbelen s = nub (s \\ (nub s))

-- Opdracht 3.3


allPossibilities :: [[Integer]]
allPossibilities =  [[a,b,c,d,e]|a<-[1..6],b<-[1..6],c<-[1..6],d<-[1..6],e<-[1..6]]

example :: [[Integer]]
example = [[1,2,3,4,5],[2,2,2,2,2],[3,3,3,3,5],[4,4,4,5,1],[4,4,4,3,3],[3,5,4,3,5],[1,2,2,3,4],[6,5,4,3,2],[6,5,4,3,1]]

noIndices :: Ord b => [b] -> [(Int, b)]
noIndices x = zip (map length (group (sort x))) (nub ((sort x))) -- results in [(no. of indices, eyes)]


filterIndices :: Ord b => [b] -> Int -> [(Int, b)]
filterIndices x y = filter (\(p,_) -> p==y) (noIndices x) -- results in [(y, eyes)]

-- separate function/filter for straight 
straightFilter x = if sort x == [1..5] || sort x == [2..6] then True else False

poker list = filter (\x -> length (filterIndices x 5) == 1) list
fourOfAKind list = filter (\x -> length (filterIndices x 4) == 1) list
threeOfAKind list = filter (\x -> length (filterIndices x 3) == 1 && length (filterIndices x 2) == 0) list
fullHouse list = filter (\x -> length (filterIndices x 2) == 1 && length (filterIndices x 3) == 1) list
twoPair list = filter (\x -> length (filterIndices x 2) == 2) list
onePair list = filter (\x -> length (filterIndices x 2) == 1 && length (filterIndices x 3) == 0) list
straight list = filter straightFilter list
bust list = filter (\x -> length ((poker [x]) ++ (fourOfAKind [x]) ++ (threeOfAKind [x]) ++ (fullHouse [x]) ++ (twoPair [x]) ++ (onePair [x]) ++ (straight [x])) == 0) list

