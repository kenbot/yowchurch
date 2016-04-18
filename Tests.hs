
module Tests where
  
import Test.QuickCheck
import qualified Ex1_ChurchBool as Ex1
import qualified Ex2a_PeanoNat as Ex2a
import qualified Ex2b_ChurchNat as Ex2b
import qualified Ex3_ChurchList as Ex3
import qualified Ex4_ChurchFree as Ex4


genCBool = oneof [pure Ex1.cTrue, pure Ex1.cFalse]


genCNat = elements $ fmap c [0..100] 
  where 
    c 0 = (\_ z -> z)
    c n = (\f -> f . (c (n-1)) f)  
    
genPNat = elements $ fmap p [0..100]
  where 
    p 0 = Ex2a.Zero
    p n = Ex2a.Succ $ p (n-1)
        
genCList = fmap c $ listOf (arbitrary :: Gen Int)
  where c [] = Ex3.CList (\_ -> id)
        c (a:as) = Ex3.CList (\x z -> x a (Ex3.cFoldr (c as) x z))



