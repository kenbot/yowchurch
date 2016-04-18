{-# LANGUAGE RankNTypes #-}

module Ex3_ChurchList where

-- Apply to head and tail:  a -> r -> r
-- Return if empty: r
newtype CList a = CList 
  { cFoldr :: (forall r. (a -> r -> r) -> r -> r)
  }


cNil :: CList a  
cNil = undefined


infixr 5 .:
(.:) :: a -> CList a -> CList a
a .: (CList f) = undefined

-- Hint: Consider how you might implement
-- append :: [a] -> [a] -> [a]

infixr 5 .++
(.++) :: CList a -> CList a -> CList a
x .++ y = undefined

unchurch :: CList a -> [a]
unchurch = undefined

church :: [a] -> CList a
church = undefined


instance Show a => Show (CList a) where 
  show = show . unchurch
  
