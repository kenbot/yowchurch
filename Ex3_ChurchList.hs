{-# LANGUAGE RankNTypes #-}

module Ex3_ChurchList where

-- "a -> r -> r" handles a head element and tail list  
-- "r" handles an empty list
newtype CList a = CList 
  { cFoldr :: (forall r. (a -> r -> r) -> r -> r)
  }


cNil :: CList a  
cNil = CList (\_ nil -> nil)


infixr 5 .:
(.:) :: a -> CList a -> CList a
a .: (CList f) = CList (\cons nil -> cons a (f cons nil))

-- Hint: Consider how you might implement
-- append :: [a] -> [a] -> [a]
infixr 5 .++
(.++) :: CList a -> CList a -> CList a
x .++ y = cFoldr x (.:) y


cLength :: CList a -> Int
cLength (CList f) = f (\_ -> (+ 1)) 0

unchurch :: CList a -> [a]
unchurch (CList f) = f (:) [] 

church :: [a] -> CList a
church [] = cNil
church (a : as) = a .: (church as)


instance Show a => Show (CList a) where 
  show = show . unchurch   

instance Eq a => Eq (CList a) where
  a == b = unchurch a == unchurch b 