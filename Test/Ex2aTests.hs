{-# LANGUAGE RankNTypes #-}

module Test.Ex2aTests where
  
import Test.QuickCheck
import Ex2a_PeanoNat

genPNat :: Gen PNat 
genPNat = elements $ fmap p [0..3]
  where 
    p 0 = Zero
    p n = Succ $ p (n-1)
    
instance Arbitrary PNat where 
  arbitrary = genPNat
   

properties :: [Property]
properties = 
  [ plusComm
  , plusAssoc
  , plusZeroId
  , multComm
  , multAssoc
  , multOneId
  , multDistrib
  , expDistrib1
  , expDistrib2
  , expDistrib3
  , iso1
  , iso2
  ]
  

up (Succ x) = up x + 1
up Zero = 0

plusComm :: Property
plusComm = counterexample ".+ should be commutative!" prop_plusIsCommutative

plusAssoc :: Property
plusAssoc = counterexample ".+ should be associative" prop_plusIsAssociative

plusZeroId :: Property
plusZeroId = counterexample "(.+ 0) or (0 .+) should be id" prop_plusZeroIsIdentity

multComm :: Property
multComm = counterexample ".* should be commutative!" prop_multIsCommutative

multAssoc :: Property
multAssoc = counterexample ".* should be associative" prop_multIsAssociative

multOneId :: Property
multOneId = counterexample "(.* 1) or (1 .*) should be id" prop_multOneIsIdentity

multDistrib :: Property
multDistrib = counterexample ".* should distribute over .+" prop_multDistrib

expDistrib1 :: Property
expDistrib1 = counterexample "(x * y)^m should equal x^m * y^m" prop_expDistrib1

expDistrib2 :: Property
expDistrib2 = counterexample "x^m * x^n should equal x^(m + n)" prop_expDistrib2

expDistrib3 :: Property
expDistrib3 = counterexample "(x^m)^n should equal x^(m * n)" prop_expDistrib3

iso1 :: Property
iso1 = counterexample "peano . unpeano should be id!" prop_peanoIso

iso2 :: Property
iso2 = counterexample "unpeano . peano should be id!" prop_unpeanoIso

prop_expDistrib1 :: PNat -> PNat -> PNat -> Bool
prop_expDistrib1 x y m = up ((x .^ m) .* (y .^ m)) == up ((x .* y) .^ m)

prop_expDistrib2 :: PNat -> PNat -> PNat -> Bool
prop_expDistrib2 a b c = up (a .^ (b .+ c)) == up ((a .^ b) .* (a .^ c)) 

prop_expDistrib3 :: PNat -> PNat -> PNat -> Bool
prop_expDistrib3 a b c = up ((a .^ b) .^ c) == up (a .^ (b .* c)) 


prop_plusIsCommutative :: PNat -> PNat -> Bool
prop_plusIsCommutative a b = up (a .+ b) == up (b .+ a)

prop_plusIsAssociative :: PNat -> PNat -> PNat -> Bool
prop_plusIsAssociative a b c = up ((a .+ b) .+ c) == up (a .+ (b .+ c))

prop_plusZeroIsIdentity :: PNat -> Bool
prop_plusZeroIsIdentity p = up (p .+ p0) == up p && up (p0 .+ p) == up p

prop_multIsCommutative :: PNat -> PNat -> Bool
prop_multIsCommutative a b = up (a .* b) == up (b .* a)

prop_multIsAssociative :: PNat -> PNat -> PNat -> Bool
prop_multIsAssociative a b c = up ((a .* b) .* c) == up (a .* (b .* c))

prop_multOneIsIdentity :: PNat -> Bool
prop_multOneIsIdentity p = up (p .* p1) == up p && up (p1 .* p) == up p


prop_multDistrib :: PNat -> PNat -> PNat -> Bool
prop_multDistrib a b c = up (a .* (b .+ c)) == up ((a .* b) .+ (a .* c))


prop_peanoIso :: PNat -> Bool
prop_peanoIso x = up ((peano . unpeano) x) == up x

prop_unpeanoIso :: Int -> Bool
prop_unpeanoIso rawX = (unpeano . peano) x == x 
  where x = abs rawX
