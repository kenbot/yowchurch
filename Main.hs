{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Main where
  
import Criterion.Main
import Ex4_ChurchFree
import Control.Monad.Free(liftF, MonadFree)
import Control.Monad
import Control.Monad.State
import Data.Monoid

data AdderF a = Add Int a | Total (Int -> a) 
  deriving Functor
  
type AdderLike = MonadFree AdderF
type Adder = Free AdderF
type CAdder = Free AdderF

add :: (AdderLike m) => Int -> m ()
add n = liftF $ Add n () 

total :: (AdderLike m) => m Int
total = liftF $ Total id

runAdderF :: AdderF a -> State Int a
runAdderF (Add i a) = modify (+i) >> pure a
runAdderF (Total f) = get >>= pure . f 


bigAdder :: (AdderLike m) => Int -> m Int 
bigAdder depth = loop depth $ pure () 
  where 
    loop 0 m = m >> total
    loop depth m = loop (depth - 1) (m >> add 1)
    

program :: Int -> Int
program depth = fst (runState state 0)
  where
    state = foldFree runAdderF (bigAdder depth)

cProgram :: Int -> Int
cProgram depth = fst (runState state 0)
  where
    state = cFoldFree runAdderF (bigAdder depth)


{-

type NestedListLike = MonadFree []
type NestedList = Free []
type CNestedList = CFree []




list123 :: NestedListLike m => m Int
list123 = liftF [1,2,3]


nextList :: NestedListLike m => Int -> m Int
nextList i = liftF [i, i*2, i*3] 

bigTree :: NestedListLike m => m Int -> Int -> m Int
bigTree result 0 = result
bigTree lists depth  = bigTree (lists >>= nextList) (depth - 1)


trans :: [a] -> [a]
trans l = head l : []


main :: IO ()
main = defaultMain 
  [ bgroup "Recursive free monad" 
                  [ bench "100 depth" $ nf program 100
                  , bench "200 depth" $ nf program 200                  
                  , bench "300 depth" $ nf program 300
                  ]
  , bgroup "Church-encoded free monad" 
                  [ bench "100 depth" $ nf cProgram 10
                  , bench "200 depth" $ nf cProgram 100
                  , bench "300 depth" $ nf cProgram 1000
                  ]
  ]
    
-}

    
    

{-
tree1000 = fullTree 1000
tree2000 = fullTree 2000
tree3000 = fullTree 3000
tree4000 = fullTree 4000
tree5000 = fullTree 5000
tree6000 = fullTree 6000


main :: IO ()
main = defaultMain [
  bgroup "zigzag" [ bench "1k"  $ nf zigzag tree1000
                  , bench "2k"  $ nf zigzag tree2000
                  , bench "3k"  $ nf zigzag tree3000
                  , bench "4k"  $ nf zigzag tree4000
                  , bench "5k"  $ nf zigzag tree5000
                  , bench "6k"  $ nf zigzag tree6000
                  ]
  ]
  

data TreeF a = TreeF a a
  deriving (Functor, Show)
  
type Tree a = Free TreeF a
  
zigzag :: Tree Int -> Int 
zigzag = zig
  where 
    zig (Pure n) = n
    zig (Wrap (TreeF t1 t2)) = zag t1 
    zag (Pure n) = n
    zag (Wrap (TreeF t1 t2)) = zig t2 

fullTree :: Int -> Tree Int
fullTree 1 = Pure 1
fullTree n = 
  do
    i <- fullTree (n - 1)
    Wrap $ TreeF (Pure (n - i - 1)) (Pure (i + 1))


tree = fullTree 2000
-}
