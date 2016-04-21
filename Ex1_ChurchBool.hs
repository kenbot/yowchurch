{-# LANGUAGE RankNTypes #-}

module Ex1_ChurchBool where

-- The 1st "r" handles truth.
-- The 2nd "r" handles falsity.
newtype CBool = CBool 
  {  cFold :: forall r. r -> r -> r
  }

cTrue :: CBool
cTrue = CBool $ \x y -> x

cFalse :: CBool
cFalse = CBool $ \x y -> y

cNot :: CBool -> CBool
cNot (CBool f) = f cFalse cTrue

infixr 3 .&&
(.&&) :: CBool -> CBool -> CBool
(CBool ifA) .&& b = ifA b cFalse

infixr 2 .||
(.||) :: CBool -> CBool -> CBool
(CBool ifA) .|| b = ifA cTrue b

unchurch :: CBool -> Bool
unchurch (CBool f) = f True False

church :: Bool -> CBool
church b = if b then cTrue else cFalse


instance Show CBool where 
  show = show . unchurch

instance Eq CBool where 
  a == b = unchurch a == unchurch b 