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
pn1 .+ pn2 = undefined


infixl 7 .*
(.*) :: PNat -> PNat -> PNat
pn1 .* pn2 = undefined

unpeano :: PNat -> Int
unpeano = undefined

peano :: Int -> PNat
peano = undefined

instance Show PNat where
  show = show . unpeano