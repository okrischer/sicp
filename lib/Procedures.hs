module Procedures where

import Control.Monad ( replicateM_ )
import Control.Monad.ST ( runST, ST )
import Data.STRef ( newSTRef, readSTRef, writeSTRef )

sqrtHeron :: Double -> Double 
sqrtHeron x = iter 1
  where 
    satisfies guess = abs (guess^2 - x) < 0.001
    improve guess = (guess + (x / guess)) / 2
    iter guess =
      if satisfies guess then guess
      else iter $ improve guess

facRec :: Integer -> Integer
facRec n = case n of
  1 -> 1
  n -> n * facRec (n-1)

facIter :: Integer -> Integer
facIter = iter 1
  where
    iter p n = case n of
      1 -> p
      n -> iter (p*n) (n-1)

fibRec :: Integer -> Integer
fibRec n
  | n < 2 = n
  | otherwise = fibRec (n-1) + fibRec (n-2)

fibIter :: Integer -> Integer
fibIter = iter 0 1
  where
    iter a b n = case n of
      0 -> a
      n -> iter b (a+b) (n-1)

fibST :: Int -> ST s Integer
fibST n = do
  a <- newSTRef 0
  b <- newSTRef 1
  replicateM_ n (do 
    x <- readSTRef a
    y <- readSTRef b
    writeSTRef a y
    writeSTRef b $! (x+y))
  readSTRef a

fibImper :: Int -> Integer 
fibImper n = runST (fibST n)