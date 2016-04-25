module Ex2a_PeanoNat where

-- Peano numbers represent the natural numbers, by taking them
-- as either 0 or n+1. Doing anything with them is usually very
-- inefficient.
data PNat = Zero | Succ PNat 

p0, p1, p2, p3, p4 :: PNat

p0 = Zero
p1 = Succ p0
p2 = Succ p1
p3 = Succ p2
p4 = Succ p3


-- Ex 2a.1: Implement Peano addition
infixl 6 .+
(.+) :: PNat -> PNat -> PNat
(.+) Zero b = b 
(.+) (Succ x) b = x .+ (Succ b) 

-- Ex 2a.2: Implement Peano multiplication
infixl 7 .*
(.*) :: PNat -> PNat -> PNat
(.*) Zero _ = Zero  
(.*) (Succ x) b = b .+ (x .* b)

-- Ex 2a.3: Implement Peano exponential
infixr 8 .^
(.^) :: PNat -> PNat -> PNat
(.^) _ Zero = p1  
(.^) a (Succ x) = a .* (a .^ x)

-- Ex 2a.4: Convert a Peano number to an integer
unpeano :: PNat -> Int
unpeano Zero = 0
unpeano (Succ n) = 1 + unpeano n 

-- Ex 2a.5: Convert a non-negative integer to a Peano number
peano :: Int -> PNat
peano 0 = Zero
peano n = Succ (peano (n - 1))



-- Instance boilerplate
instance Show PNat where
  show = ("peano " ++) . show . unpeano
  
instance Eq PNat where 
  a == b = unpeano a == unpeano b 
