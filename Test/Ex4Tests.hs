{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Test.Ex4Tests where
  
import Test.QuickCheck
import Test.QuickCheck.Function
import Control.Monad.Free(MonadFree, liftF)
import Ex4_ChurchFree
import NaiveFree


genFree :: (MonadFree [] m) => Gen (m Int)   
genFree = 
  let 
    f i = liftF [i*2, i+7]
    loop 0 fr = fr
    loop n fr = loop (n - 1) (fr >>= f)
 in fmap (\n -> loop n (pure n)) (elements [1..5]) 
    

instance (MonadFree [] m) => Arbitrary (m Int) where 
  arbitrary = genFree


properties :: [Property]
properties = 
  [ bindAssoc
  , leftId
  , rightId
  , foldFr
  , iso1
  , iso2
  ]
  
type BindFn = Fun Int (CFree [] Int)

uc :: CFree f a -> NaiveFree f a
uc (CFree f) = f Wrap Pure


bindAssoc :: Property
bindAssoc = counterexample ">>= should be associative" prop_bindAssoc 

leftId :: Property
leftId = counterexample "(pure n >>= f) should equal (f n)" prop_leftId 

rightId :: Property
rightId = counterexample "(m >>= pure) should equal (m)" prop_leftId 

foldFr :: Property
foldFr = counterexample "Folding cFree should give the same result as the naive free" prop_cFoldFree

iso1 :: Property
iso1 = counterexample "church . unchurch should be id" prop_churchIso

iso2 :: Property
iso2 = counterexample "unchurch . church should be id" prop_unchurchIso

prop_cFoldFree :: CFree [] Int -> Bool 
prop_cFoldFree free = naiveFoldFree id (uc free) == cFoldFree id free


prop_bindAssoc :: CFree [] Int -> BindFn -> BindFn -> Bool
prop_bindAssoc free (Fun _ f) (Fun _ g) = 
  uc (free >>= f >>= g) == uc (free >>= (\a -> f a >>= g))

prop_leftId :: Int -> BindFn -> Bool
prop_leftId n (Fun _ f) = uc (pure n >>= f) == uc (f n)

prop_rightId :: CFree [] Int -> Bool
prop_rightId free = uc (free >>= pure) == uc free

prop_churchIso :: CFree [] Int -> Bool
prop_churchIso x = uc ((church . unchurch) x) == uc x

prop_unchurchIso :: NaiveFree [] Int -> Bool
prop_unchurchIso x = (unchurch . church) x == x
