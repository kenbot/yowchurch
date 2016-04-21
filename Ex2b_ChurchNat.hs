{-# LANGUAGE RankNTypes #-}

module Ex2b_ChurchNat where
  
  
-- "r -> r" handles a successor to another nat. "Do this N times..."
-- "r" handles zero.  "...starting from here"
newtype CNat = CNat 
  { cFold :: forall r. (r -> r) -> r -> r 
  }
  
c0, c1, c2, c3, c4 :: CNat 

c0 = CNat $ \f -> id 
c1 = CNat $ \f -> f 
c2 = CNat $ \f -> f . f
c3 = CNat $ \f -> f . f . f
c4 = CNat $ \f -> f . f . f . f

cSucc :: CNat -> CNat
cSucc (CNat cn) = CNat $ \f -> f . cn f 

infixl 6 .+
(.+) :: CNat -> CNat -> CNat
(CNat nTimes) .+ (CNat mTimes) = CNat $ \f -> mTimes f . nTimes f 

infixl 7 .*
(.*) :: CNat -> CNat -> CNat
(CNat nTimes) .* (CNat mTimes) = CNat $ nTimes . mTimes

infixr 8 .^
(.^) :: CNat -> CNat -> CNat
n .^ (CNat mTimes) = mTimes (.* n) c1

unchurch :: CNat -> Int
unchurch (CNat cn) = cn (+1) 0

-- assuming integers >= 0
church :: Int -> CNat
church 0 = c0
church n = cSucc (church (n - 1))

instance Show CNat where
  show = show . unchurch
  
instance Eq CNat where
  a == b = unchurch a == unchurch b  


