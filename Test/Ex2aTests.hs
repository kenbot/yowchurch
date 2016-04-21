{-# LANGUAGE RankNTypes #-}

module Test.Ex2aTests where
  
import Test.QuickCheck
import Ex2a_PeanoNat

genPNat :: Gen PNat 
genPNat = elements $ fmap p [0..100]
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
  , distrib
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

distrib :: Property
distrib = counterexample ".* should distribute over .+" prop_distrib

iso1 :: Property
iso1 = counterexample "peano . unpeano should be id!" prop_peanoIso

iso2 :: Property
iso2 = counterexample "unpeano . peano should be id!" prop_unpeanoIso



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


prop_distrib :: PNat -> PNat -> PNat -> Bool
prop_distrib a b c = up (a .* (b .+ c)) == up ((a .* b) .+ (a .* c))


prop_peanoIso :: PNat -> Bool
prop_peanoIso x = up ((peano . unpeano) x) == up x

prop_unpeanoIso :: Int -> Bool
prop_unpeanoIso rawX = (unpeano . peano) x == x 
  where x = abs rawX
