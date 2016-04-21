module Main where
  
import Test.QuickCheck(quickCheck)

import qualified Test.Ex1Tests as Ex1
import qualified Test.Ex2aTests as Ex2a
import qualified Test.Ex2bTests as Ex2b
import qualified Test.Ex3Tests as Ex3

import Control.Monad

main :: IO ()
main = forM_ allProps quickCheck 
  where
    allProps = 
      Ex1.properties ++ 
      Ex2a.properties ++ 
      Ex2b.properties ++ 
      Ex3.properties
