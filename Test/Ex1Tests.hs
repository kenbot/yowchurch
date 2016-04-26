{-# LANGUAGE RankNTypes #-}

module Test.Ex1Tests where
  
import Test.QuickCheck
import Ex1_ChurchBool


genCBool :: Gen CBool
genCBool = oneof [pure cTrue, pure cFalse]

instance Arbitrary CBool where 
  arbitrary = genCBool


uc (CBool f) = f True False 

properties :: [Property]
properties = 
  [ orComm
  , c "x .|| cTrue should be true!" prop_orTrueisTrue
  , c "x .|| cFalse should be x!" prop_orFalseIsX
  , c "cNot . cNot should be id!" prop_notNotIsId
  , c "cNot should negate a CBool!" prop_notIsNot
  , andComm
  , c "x .&& cTrue should be x!" prop_andTrueisX
  , c "x .&& cFalse should be false!" prop_andFalseIsFalse
  , iso1
  , iso2
  ] where c = counterexample


orComm :: Property
orComm = counterexample ".|| should be commutative!" prop_orIsCommutative

andComm :: Property
andComm = counterexample ".&& should be commutative!" prop_andIsCommutative

iso1 :: Property
iso1 = counterexample "church . unchurch should be id!" prop_churchIso

iso2 :: Property
iso2 = counterexample "unchurch . church should be id!" prop_unchurchIso

prop_orIsCommutative :: CBool -> CBool -> Bool
prop_orIsCommutative a b = (a .|| b) == (b .|| a)

prop_orTrueisTrue :: CBool -> Bool
prop_orTrueisTrue x = uc (x .|| cTrue)
  
prop_orFalseIsX :: CBool -> Bool
prop_orFalseIsX x = uc (x .|| cFalse) == uc x
  

prop_notNotIsId :: CBool -> Bool
prop_notNotIsId x = (uc . cNot . cNot) x == uc x

prop_notIsNot :: CBool -> Bool
prop_notIsNot x = (uc . cNot) x == (not . uc) x

prop_andIsCommutative :: CBool -> CBool -> Bool
prop_andIsCommutative a b = (a .&& b) == (b .&& a)

prop_andTrueisX :: CBool -> Bool
prop_andTrueisX x = uc (x .&& cTrue) == uc x
  
prop_andFalseIsFalse :: CBool -> Bool
prop_andFalseIsFalse x = uc (x .&& cFalse) == False

prop_churchIso :: CBool -> Bool
prop_churchIso x = (church . unchurch) x == x

prop_unchurchIso :: Bool -> Bool
prop_unchurchIso x = (unchurch . church) x == x

