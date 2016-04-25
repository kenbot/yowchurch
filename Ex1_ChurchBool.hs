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


-- Ex 1.1: Implement boolean not.
cNot :: CBool -> CBool
cNot (CBool f) = f cFalse cTrue

-- Ex 1.2: Implement boolean and.
infixr 3 .&&
(.&&) :: CBool -> CBool -> CBool
(CBool ifA) .&& b = ifA b cFalse

-- Ex 1.3: Implement boolean or
infixr 2 .||
(.||) :: CBool -> CBool -> CBool
(CBool ifA) .|| b = ifA cTrue b

-- Ex 1.4: Turn a church boolean back into a boolean
unchurch :: CBool -> Bool
unchurch (CBool f) = f True False

-- Ex 1.5: Lift a boolean into a church boolean
church :: Bool -> CBool
church b = if b then cTrue else cFalse



-- Instance boilerplate
instance Show CBool where 
  show (CBool f) = f "cTrue" "cFalse"

instance Eq CBool where 
  a == b = unchurch a == unchurch b 