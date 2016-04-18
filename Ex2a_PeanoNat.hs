module Ex2a_PeanoNat where

data PNat = Zero | Succ PNat 

p0, p1, p2, p3, p4 :: PNat

p0 = Zero
p1 = Succ p0
p2 = Succ p1
p3 = Succ p2
p4 = Succ p3

infixl 6 .+
(.+) :: PNat -> PNat -> PNat
(.+) Zero b = b 
(.+) (Succ x) b = x .+ (Succ b) 

infixl 7 .*
(.*) :: PNat -> PNat -> PNat
(.*) Zero _ = Zero  
(.*) (Succ x) b = b .+ (x .* b)

unpeano :: PNat -> Int
unpeano Zero = 0
unpeano (Succ n) = 1 + unpeano n 

peano :: Int -> PNat
peano 0 = Zero
peano n = Succ (peano (n - 1))
  
pShow :: PNat -> String
pShow = show . unpeano

instance Show PNat where
  show = pShow