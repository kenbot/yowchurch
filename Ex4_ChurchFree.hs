{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Ex4_ChurchFree where
  
import NaiveFree
import Control.Monad.Free (MonadFree, wrap)
import Control.Applicative (Applicative(..))
import Control.Monad (ap, join)
import Prelude.Extras


-- "f r -> r" handles the nested functor.
-- "a -> r" handles the pure value.
newtype CFree f a = CFree 
  { cFold :: forall r. (f r -> r) -> (a -> r) -> r
  }
   

-- Ex4.1: Implement cPure, which lifts a value into a 
-- Church-encoded free monad.
cPure :: a -> CFree f a
cPure a = CFree (\_ onPure -> onPure a)

-- Ex4.2: Implement cWrap, which wraps a functor of a free into a new layer.
cWrap :: Functor f => f (CFree f a) -> CFree f a
cWrap ffa = CFree $ \onWrap onPure -> 
  let 
    foldIt free = cFold free onWrap onPure
    fr = fmap foldIt ffa
  in onWrap fr

-- Ex4.3: Implement cFoldFree, which reduces a CFree to a 
-- monad m x, given natural transformation f ~> m.
cFoldFree :: Monad m => (forall x. f x -> m x) -> CFree f a -> m a
cFoldFree f (CFree foldIt) = foldIt (join . f) pure

-- Ex 4.4: Convert a naive Free implementation into the Church-encoded equivalent.
church :: Functor f => NaiveFree f a -> CFree f a
church (Pure a) = cPure a
church (Wrap ffa) = cWrap (fmap church ffa)

-- Ex 4.5: Convert from a Church-encoded free to a naive one.
unchurch :: CFree f a -> NaiveFree f a
unchurch (CFree foldIt) = foldIt Wrap Pure

{-
   Bind and map for CFree are completely different from NaiveFree's;
   We don't need to know anything about the f type constructor, because we
   don't touch it. It doesn't even need to be a functor.
-}

-- Ex 4.6: Implement fmap for a Church-encoded free monad.
-- Consider: 
--   Bind and map for CFree are completely different from 
--   NaiveFree's; we don't need to know anything about f, because we
--   don't touch it. It doesn't even need to be a functor.
cMap :: (a -> b) -> CFree f a -> CFree f b
cMap f (CFree foldIt) = CFree $ \onWrap onPure ->
  foldIt onWrap (onPure . f)

-- Exc 4.7: Implement bind for a Church-encoded free monad.
cBind :: CFree f a -> (a -> CFree f b) -> CFree f b
cBind (CFree foldIt) f = CFree $ \onWrap onPure -> 
  foldIt onWrap (\a -> cFold (f a) onWrap onPure)


-- Instance boilerplate. Note that f need not be a Functor!
instance Functor (CFree f) where
  fmap = cMap

instance Applicative (CFree f) where
  (<*>) = ap  
  pure = cPure

instance Monad (CFree f) where
  (>>=) = cBind

instance Functor f => MonadFree f (CFree f) where 
  wrap = cWrap

instance (Eq a, Eq (f Bool)) => Eq (CFree f a) where 
  (CFree foldX) == (CFree foldY) = 
    foldX 
      (\x -> foldY (== x) (const False)) 
      (\a -> foldY (const False) (== a))
  
instance (Show a, Show1 f) => Show (CFree f a) where
  show = ("church $ " ++) . show . unchurch
