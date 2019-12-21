{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Strategies where 
-- 
import Elevate
-- import Control.Applicative
-- 
-- -- Basic Combinators
-- data Seq' where Seqq' :: (Strategy a q, Strategy b r) => a -> b -> Seq'
-- instance Strategy Seq' p where
--     Seqq' f s $$ p = (f $$ p) >>= (\x -> s $$ x)
-- 
-- data LChoice' where LChoice' :: (Strategy s1 p, Strategy s2 p) => s1 -> s2 -> LChoice'
-- instance Strategy LChoice' p where
--     LChoice' f s $$ p = (f $$ p) <|> (s $$ p)
-- 
-- -- Basic Strategies
-- data Try' where Try' :: Strategy s p => s -> Try'
-- instance Strategy Try' p where
--     Try' s $$ x = (LChoice' s Id') $$ x
-- 
-- data Repeat' where Repeat' :: Strategy s p => s -> Repeat'
-- instance Strategy Repeat' p where
--     Repeat' s $$ p = (Try' (Seq' s (Repeat' s))) $$ p
-- 
-- 
-- Traversable
class Traversable' p where
    all' :: (Strategy s1 p, Strategy s2 p) => s1 -> s2
    one' :: (Strategy s1 p, Strategy s2 p) => s1 -> s2
    -- some' :: Strategy p -> Strategy p 
-- 
-- -- Complete Traversals
-- -- oncetd :: Traversable' p => Strategy p -> Strategy p
-- -- oncetd s = s <+ (one' . oncetd $ s)
-- 
-- oncetd :: (Strategy s1 p, Strategy s2 p) => s1 -> s2
-- oncetd s = LChoice' s (one' (oncetd s))
-- 
-- -- oncebu :: Traversable' p => Strategy p -> Strategy p
-- -- oncebu s = (one' . oncebu $ s) <+ s
-- 
-- -- topdown :: Traversable' p => Strategy p -> Strategy p
-- topdown s = Seq' s (all' . topdown $ s)
-- 
-- -- Normalize
-- -- normalize :: Show p => Traversable' p => Strategy p -> Strategy p
-- normalize s = Repeat' (oncetd s)
-- 
-- -- Simplification
-- -- idSimpl :: Strategy p -> Strategy p
-- -- idSimpl (seq' id' id') = id'
