{-# LANGUAGE RankNTypes #-}

module Test.Ex3Tests where
  
import Test.QuickCheck
import Ex3_ChurchList

genCList :: Arbitrary a => Gen (CList a)   
genCList = fmap c $ listOf arbitrary
  where c [] = CList (\_ -> id)
        c (a:as) = CList (\x z -> x a (cFoldr (c as) x z))

instance Arbitrary a => Arbitrary (CList a) where 
  arbitrary = genCList


properties :: [Property]
properties = 
  [ consLength
  , appendLength
  , lengthCommutes
  , iso1
  , iso2
  ]
  

uc :: CList a -> [a]
uc (CList f) = f (:) []


consLength = counterexample "Length of a cons should be one more than its tail" prop_consLength

appendLength = counterexample "Length of an appended list should be the sum of the two lists' lengths" prop_appendLength

lengthCommutes = counterexample "Length should commute with respect to unchurch" prop_lengthCommutes



iso1 :: Property
iso1 = counterexample "church . unchurch should be id" prop_churchIso

iso2 :: Property
iso2 = counterexample "unchurch . church should be id" prop_unchurchIso

prop_consLength :: CList Int -> Int -> Bool
prop_consLength list n = length (uc (n .: list)) == 1 + (length (uc list)) 

prop_appendLength :: CList Int -> CList Int -> Bool
prop_appendLength l1 l2 = length (uc (l1 .++ l2)) == length (uc l1) + length (uc l2) 


prop_lengthCommutes :: CList Int -> Bool
prop_lengthCommutes list = (cLength list) == length (uc list)


prop_churchIso :: CList Int -> Bool
prop_churchIso x = uc ((church . unchurch) x) == uc x

prop_unchurchIso :: [Int] -> Bool
prop_unchurchIso x = (unchurch . church) x == x
