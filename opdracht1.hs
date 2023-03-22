module Opdracht1 where

-- Import Data.Bits for Opdracht 1.4b and Opdracht 1.5b
import Data.Bits

-- Opdracht 1.1a
faca :: Int -> Int
faca 0 = 1  
faca n = n * faca (n - 1)  

-- Opdracht 1.1b
facb :: Int -> Int
facb x
  | x <= 0 = 1
  | otherwise = (facb (x-1)) * x

-- Opdracht 1.2a
nulpuntena :: Double -> Double -> Double -> [Double]
nulpuntena a b c = [(-1 * b + sqrt(b^2 +(-4 * a * c))) / (2 * a),(-1 * b - sqrt(b^2 +(-4 * a * c))) / (2 * a)]

-- Opdracht 1.2b
nulpuntenb :: Double -> Double -> Double -> [Double]
nulpuntenb a b c
  | disc > 0 = [x1, x2]
  | disc == 0 = [x1]
  | otherwise = []
  where
    disc = b^2 - 4*a*c
    x1 = (-b + sqrt(disc)) / (2 * a)
    x2 = (-b - sqrt(disc)) / (2 * a)

-- Opdracht 1.2c
diceIsMod5 :: [(Integer, Integer, Integer)]
diceIsMod5 = [(x,y,z)|x<-[1..6],y<-[1..6],z<-[1..6],(x+y+z) `mod` 5 == 0] 

-- Opdracht 1.2d
diceIsModn :: Integer -> [(Integer, Integer, Integer)]
diceIsModn n = [(x,y,z)|x<-[1..6],y<-[1..6],z<-[1..6],(x+y+z) `mod` n == 0] 

-- Opdracht 1.3
puzzle :: [(Integer, Integer, Integer)]
puzzle = [(a,b,c)|a<-[-100..100],b<-[-100..100],c<-[-100..100],a==2*(b-c),b==a*c,c*2==a+c] 

-- Opdracht 1.4a
mult :: Integer -> Integer -> Integer
mult x 0 = 0
mult x y = x + mult x (y-1)

-- Opdracht 1.4b
fastmult :: Integer -> Integer -> Integer
fastmult x y
  | y == 0 = 0
  | even y = c
  | otherwise = x + c
  where c = fastmult (shiftL x 1) (shiftR y 1)

-- Opdracht 1.5a
pow :: Integer -> Integer -> Integer
pow x 0 = 1
pow x p = x * pow x (p-1)

-- Opdracht 1.5b
fastpow :: Integer -> Integer -> Integer
fastpow x p
  | p == 0 = 1
  | even p = c
  | otherwise = x * c
  where c = fastpow (x * x) (shiftR p 1)
