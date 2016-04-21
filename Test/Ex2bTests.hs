{-# LANGUAGE RankNTypes #-}

module Test.Ex2bTests where
  
import Test.QuickCheck
import Ex2b_ChurchNat

genCNat :: Gen CNat
genCNat = elements $ fmap c [0..3] 
  where 
    c 0 = CNat (\_ z -> z)
    c n = CNat (\f -> f . cFold (c (n-1)) f)  
    
instance Arbitrary CNat where 
  arbitrary = genCNat

properties :: [Property]
properties = 
  [ plusComm
  , plusAssoc
  , plusZero
  , multComm
  , multAssoc
  , multOne
  , multDistrib
  , expOne
  , expZero
  , expDistrib1
  , expDistrib2
  , expDistrib3
  , iso1
  , iso2
  ]
  

uc :: CNat -> Int
uc (CNat f) = f (+1) 0

plusComm :: Property
plusComm = counterexample "Addition should be commutative!" prop_plusIsCommutative

plusAssoc :: Property
plusAssoc = counterexample "Addition should be associative!" prop_plusIsAssociative

plusZero :: Property
plusZero = counterexample "Adding zero should have no effect!" prop_plusZeroIsIdentity

multComm :: Property
multComm = counterexample "Multiplication should be commutative!" prop_multIsCommutative

multAssoc :: Property
multAssoc = counterexample "Multiplication should be associative" prop_multIsAssociative

multOne :: Property
multOne = counterexample "Multiplying one should have no effect!" prop_multOneIsIdentity

multDistrib :: Property
multDistrib = counterexample "Multiplication should distribute over addition" prop_multDistrib

expOne :: Property
expOne = counterexample "Exponent of one should be identity" prop_expOneIsIdentity

expZero :: Property
expZero = counterexample "Exponent of zero should be one" prop_expZeroIsOne

expDistrib1 :: Property
expDistrib1 = counterexample "(x * y)^m should equal x^m * y^m" prop_expDistrib1

expDistrib2 :: Property
expDistrib2 = counterexample "x^m * x^n should equal x^(m + n)" prop_expDistrib2

expDistrib3 :: Property
expDistrib3 = counterexample "(x^m)^n should equal x^(m * n)" prop_expDistrib3

iso1 :: Property
iso1 = counterexample "church . unchurch should be id!" prop_churchIso

iso2 :: Property
iso2 = counterexample "unchurch . church should be id!" prop_unchurchIso



prop_plusIsCommutative :: CNat -> CNat -> Bool
prop_plusIsCommutative a b = uc (a .+ b) == uc (b .+ a)

prop_plusIsAssociative :: CNat -> CNat -> CNat -> Bool
prop_plusIsAssociative a b c = uc ((a .+ b) .+ c) == uc (a .+ (b .+ c))

prop_plusZeroIsIdentity :: CNat -> Bool
prop_plusZeroIsIdentity c = uc (c .+ c0) == uc c && uc (c0 .+ c) == uc c

prop_multIsCommutative :: CNat -> CNat -> Bool
prop_multIsCommutative a b = uc (a .* b) == uc (b .* a)

prop_multIsAssociative :: CNat -> CNat -> CNat -> Bool
prop_multIsAssociative a b c = uc ((a .* b) .* c) == uc (a .* (b .* c))

prop_multOneIsIdentity :: CNat -> Bool
prop_multOneIsIdentity c = uc (c .* c1) == uc c && uc (c1 .* c) == uc c

prop_expOneIsIdentity :: CNat -> Bool
prop_expOneIsIdentity c = uc (c .^ c1) == uc c

prop_expZeroIsOne :: CNat -> Bool
prop_expZeroIsOne c = uc (c .^ c0) == 1

prop_expDistrib1 :: CNat -> CNat -> CNat -> Bool
prop_expDistrib1 x y m = uc ((x .^ m) .* (y .^ m)) == uc ((x .* y) .^ m)

prop_expDistrib2 :: CNat -> CNat -> CNat -> Bool
prop_expDistrib2 a b c = uc (a .^ (b .+ c)) == uc ((a .^ b) .* (a .^ c)) 

prop_expDistrib3 :: CNat -> CNat -> CNat -> Bool
prop_expDistrib3 a b c = uc ((a .^ b) .^ c) == uc (a .^ (b .* c)) 


prop_multDistrib :: CNat -> CNat -> CNat -> Bool
prop_multDistrib a b c = uc (a .* (b .+ c)) == uc ((a .* b) .+ (a .* c))


prop_churchIso :: CNat -> Bool
prop_churchIso x = uc ((church . unchurch) x) == uc x

prop_unchurchIso :: Int -> Bool
prop_unchurchIso rawX = (unchurch . church) x == x 
  where x = abs rawX
