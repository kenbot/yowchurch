{-# LANGUAGE RankNTypes #-}

module Ex3_ChurchList where

-- "a -> r -> r" handles a head element and tail list  
-- "r" handles an empty list
newtype CList a = CList 
  { cFoldr :: (forall r. (a -> r -> r) -> r -> r)
  }

-- Ex 3.1: Implement the empty Church list
cNil :: CList a  
cNil = undefined

-- Ex 3.2: Implement Church "cons", which 
-- joins a head value to a tail list, forming a new list.
infixr 5 .:
(.:) :: a -> CList a -> CList a
(.:) a (CList f) = undefined

-- Ex 3.3: Implement Church append, joining two lists together. 
-- Hint: Consider how you might implement the non-church append
-- (++) :: [a] -> [a] -> [a]
infixr 5 .++
(.++) :: CList a -> CList a -> CList a
(.++) x y = undefined

-- Ex 3.4: Take the length of a Church list.
cLength :: CList a -> Int
cLength = undefined

-- 3.5: Convert a Church list to a regular list.
unchurch :: CList a -> [a]
unchurch = undefined

-- 3.6: Convert a regular list to a Church list.
church :: [a] -> CList a
church = undefined



-- Instances boilerplate
instance Show a => Show (CList a) where 
  show = ("church " ++) . show . unchurch   

instance Eq a => Eq (CList a) where
  a == b = unchurch a == unchurch b 
