{-# LANGUAGE RankNTypes #-}

module Ex2b_ChurchNat where
  
-- Apply to predecessor: r -> r
-- Return if zero: r
type CNat = forall r. (r -> r) -> r -> r 

c0, c1, c2, c3, c4 :: CNat 

c0 f z = z 
c1 f z = f z
c2 f z = f (f z)
c3 f z = f (f (f z))
c4 f z = f (f (f (f z)))

cSucc :: CNat -> CNat
cSucc = undefined 

infixl 6 .+
(.+) :: CNat -> CNat -> CNat
cn1 .+ cn2 = undefined

infixl 7 .*
(.*) :: CNat -> CNat -> CNat
cn1 .* cn2 = undefined

unchurch :: CNat -> Int
unchurch = undefined

-- assuming integers >= 0
church :: Int -> CNat
church = undefined

cShow :: CNat -> String 
cShow = show . unchurch

