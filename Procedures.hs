module Procedures where
import Control.Monad ( replicateM_ )
import Control.Monad.ST ( runST, ST )
import Data.STRef ( newSTRef, readSTRef, writeSTRef )

fibRec :: Integer -> Integer
fibRec n | n < 2 = n
         | otherwise = fibRec (n-1) + fibRec (n-2)

fibIter :: Integer -> Integer 
fibIter n = fst (iter n)
  where 
    iter 0 = (0,1)
    iter n = (b, a+b)
      where (a,b) = iter (n-1)

fibImp :: Int -> Integer 
fibImp n = runST (fibMut n)

fibMut :: Int -> ST s Integer
fibMut n = do
  a <- newSTRef 0
  b <- newSTRef 1
  replicateM_ n (do 
    x <- readSTRef a
    y <- readSTRef b
    writeSTRef a y
    writeSTRef b $! (x+y))
  readSTRef a