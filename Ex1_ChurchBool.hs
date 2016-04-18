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
cNot cb = cb cFalse cTrue

infixr 3 .&&
(.&&) :: CBool -> CBool -> CBool
cb1 .&& cb2 = cb1 cb2 cFalse

infixr 2 .||
(.||) :: CBool -> CBool -> CBool
cb1 .|| cb2 = cb1 cTrue cb2

unchurch :: CBool -> Bool
unchurch cb = cb True False

church :: Bool -> CBool
church b = if b then cTrue else cFalse

cShow :: CBool -> String
cShow = show . unchurch

