{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Strategies where 
 
import Elevate
import Control.Applicative
 
-- Basic Combinators
data Seq' p where Seq' :: (Strategy a p, Strategy b p) => a -> b -> Seq' p
instance Strategy (Seq' p) p where
    Seq' f s $$ x = (f $$ x) >>= (\y -> s $$ y)

(~>>) :: (Strategy a p, Strategy b p) => a -> b -> Seq' p
a ~>> b = Seq' a b

data LChoice' p where LChoice' :: (Strategy s1 p, Strategy s2 p) => s1 -> s2 -> LChoice' p
instance Strategy (LChoice' p) p where
    LChoice' f s $$ p = (f $$ p) <|> (s $$ p)

(<+) :: (Strategy a p, Strategy b p) => a -> b -> LChoice' p
a <+ b = LChoice' a b


-- Basic Strategies
data Try' p where Try' :: Strategy s p => s -> Try' p
instance Strategy (Try' p) p where
    Try' s $$ x = s <+ Id' $$ x 

-- this is what I had to do without FunctionalDependencies
-- MultiParamTypeClasses mess up type inference
-- todo: try out TypeFamilies instead
data Repeat' p where Repeat' :: Strategy s p => s -> Repeat' p
instance Strategy (Repeat' p) p where
    Repeat' s $$ p = try' (seq' s (repeat' s)) $$ p where
        try'    = \a -> Try' a :: Try' p
        seq'    = \a -> \b -> Seq' a b :: Seq' p
        repeat' = \a -> Repeat' a :: Repeat' p

-- Traversable
class Traversable' x where
    all' :: Strategy a x => a -> x -> Rewrite x
    one' :: Strategy a x => a -> x -> Rewrite x

data FunToStrategy p where FunToStrategy :: (p -> Rewrite p) -> FunToStrategy p
instance Strategy (FunToStrategy p) p where
    FunToStrategy f $$ p = f p

data All' p where All' :: (Strategy s p, Traversable' p) => s -> All' p
instance Strategy (All' p) p where
    All' s $$ p = FunToStrategy (all' s) $$ p

data One' p where One' :: (Strategy s p, Traversable' p) => s -> One' p
instance Strategy (One' p) p where
    One' s $$ p = FunToStrategy (one' s) $$ p


-- Complete Traversals
oncetd s = s <+ (One' . oncetd $ s) 

topdown s = s ~>> (All' . topdown $ s)

normalize s = Repeat' . oncetd $ s

-- Simplification
-- idSimpl :: Seq' p -> 
-- idSimpl (Seq' (Id' :: Id' p) (Id' :: Id' p)) = Id'
-- idSimpl (Seq' _ _) = Id'

data IdSimpl p = IdSimpl
instance Strategy (IdSimpl p) (Seq' p) where
    IdSimpl $$ Seq' Id' Id' = Success Id'
    IdSimpl $$ _            = Failure 
