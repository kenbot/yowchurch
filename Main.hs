module Main where
  
import Test.QuickCheck(quickCheck)

import qualified Test.Ex1Tests as Ex1
import qualified Test.Ex2aTests as Ex2a
import qualified Test.Ex2bTests as Ex2b
import qualified Test.Ex3Tests as Ex3
import qualified Test.Ex4Tests as Ex4


import Control.Monad

main :: IO ()
main = do
  putStrLn "Ex1: Church booleans"
  putStrLn "---------------"
  forM_ Ex1.properties quickCheck 
  
  putStrLn "\nEx2a: Peano natural numbers"
  putStrLn "---------------"
  forM_ Ex2a.properties quickCheck 
  
  putStrLn "\nEx2b: Church natural numbers"
  putStrLn "---------------"
  forM_ Ex2b.properties quickCheck 
  
  putStrLn "\nEx3: Church lists"
  putStrLn "---------------"
  forM_ Ex3.properties quickCheck 

  putStrLn "\nEx4: Church free monads"
  putStrLn "---------------"
  forM_ Ex4.properties quickCheck 
