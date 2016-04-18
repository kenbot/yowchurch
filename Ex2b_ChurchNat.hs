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
cSucc cn f = f . cn f 

infixl 6 .+
(.+) :: CNat -> CNat -> CNat
cn1 .+ cn2 = \f -> cn1 f . cn2 f 

infixl 7 .*
(.*) :: CNat -> CNat -> CNat
(.*) = (.)

unchurch :: CNat -> Int
unchurch cn = cn (+1) 0

-- assuming integers >= 0
church :: Int -> CNat
church 0 = c0
church n = cSucc (church (n - 1))


cShow :: CNat -> String 
cShow = show . unchurch

