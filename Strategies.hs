{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}

module Strategies where 
-- 
import Elevate
import Control.Applicative
 
-- Basic Combinators
data Seq' p where Seq' :: (Strategy a p, Strategy b p) => a -> b -> Seq' p
instance Strategy (Seq' p) p where
    Seq' f s $$ x = (f $$ x) >>= (\y -> s $$ y)

data LChoice' p where LChoice' :: (Strategy s1 p, Strategy s2 p) => s1 -> s2 -> LChoice' p
instance Strategy (LChoice' p) p where
    LChoice' f s $$ p = (f $$ p) <|> (s $$ p)

-- Basic Strategies
data Try' p where Try' :: Strategy s p => s -> Try' p
instance Strategy (Try' p) p where
    Try' s $$ x = lChoice' s id' $$ x where
        lChoice' = \a -> \b -> LChoice' a b :: LChoice' p
        id' = Id' :: Id' p

data Repeat' p where Repeat' :: Strategy s p => s -> Repeat' p
instance Strategy (Repeat' p) p where
    Repeat' s $$ p = try' (seq' s (repeat' s)) $$ p where
        try'    = \a -> Try' a :: Try' p
        seq'    = \a -> \b -> Seq' a b :: Seq' p
        repeat' = \a -> Repeat' a :: Repeat' p

-- Traversable
class Traversable' p where
    all' :: (Strategy s1 p1, Strategy s2 p2) => s1 -> s2
    one' :: (Strategy s1 p1, Strategy s2 p2) => s1 -> s2
    -- some' :: Strategy p -> Strategy p 

-- Complete Traversals
-- oncetd :: Traversable' p => Strategy p -> Strategy p
-- oncetd s = s <+ (one' . oncetd $ s)

oncetd :: forall a b p . (Strategy a p, Strategy b p) => a -> b
oncetd s = LChoice' s (one' (oncetd s)) :: b

-- oncebu :: Traversable' p => Strategy p -> Strategy p
-- oncebu s = (one' . oncebu $ s) <+ s

-- topdown :: Traversable' p => Strategy p -> Strategy p
topdown s = Seq' s (all' . topdown $ s)

-- Normalize
-- normalize :: Show p => Traversable' p => Strategy p -> Strategy p
normalize s = Repeat' (oncetd s)

-- Simplification
-- idSimpl :: Strategy p -> Strategy p
-- idSimpl (seq' id' id') = id'
