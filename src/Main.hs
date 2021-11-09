module Main where

import Criterion.Main
import Procedures

main :: IO ()
main = defaultMain 
  [bgroup "fib"
    [ bench "iter"  $ whnf fibIter 100
    , bench "imper" $ whnf fibImper 100]]