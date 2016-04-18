{-# LANGUAGE RankNTypes #-}

module Ex1_ChurchBool where

-- Return if true: r (the 1st one)
-- Return if false: r (the 2nd one)
type CBool = forall r. r -> r -> r

cTrue :: CBool
cTrue  x y = x

cFalse :: CBool
cFalse x y = y

cNot :: CBool -> CBool
cNot cb = undefined


infixr 3 .&&
(.&&) :: CBool -> CBool -> CBool
cb1 .&& cb2 = undefined

infixr 2 .||
(.||) :: CBool -> CBool -> CBool
cb1 .|| cb2 = undefined

unchurch :: CBool -> Bool
unchurch = undefined

church :: Bool -> CBool
church = undefined

cShow :: CBool -> String
cShow = show . unchurch

