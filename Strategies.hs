{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Strategies where 
 
import Elevate
import Control.Applicative

data Elevate p where
    Id'' :: Elevate p
    Fail'' :: Elevate p
    Seq'' :: (Strategy a p, Strategy b p) => a -> b -> Elevate p
    LChoice'' :: (Strategy a p, Strategy b p) => a -> b -> Elevate p
    Try'' :: Strategy s p => s -> Elevate p
    Repeat'' :: Strategy s p => s -> Elevate p

instance Strategy (Elevate p) p where
    Id''          $$ p = Success p
    Fail''        $$ p = Failure
    Seq'' f s     $$ p = (f $$ p) >>= (\y -> s $$ y)
    LChoice'' f s $$ p = (f $$ p) <|> (s $$ p)
    Try'' s       $$ p = s <+ Id' $$ p 
    Repeat'' s    $$ p = Try'' (Seq'' s (Repeat'' s)) $$ p 

(~>>) :: (Strategy a p, Strategy b p) => a -> b -> Elevate p
a ~>> b = Seq'' a b

(<+) :: (Strategy a p, Strategy b p) => a -> b -> Elevate p
a <+ b = LChoice'' a b

-- -- Basic Combinators
-- data Seq' p where Seq' :: (Strategy a p, Strategy b p) => a -> b -> Seq' p
-- instance Strategy (Seq' p) p where
--     Seq' f s $$ x = (f $$ x) >>= (\y -> s $$ y)
-- 
-- (~>>) :: (Strategy a p, Strategy b p) => a -> b -> Seq' p
-- a ~>> b = Seq' a b
-- 
-- data LChoice' p where LChoice' :: (Strategy s1 p, Strategy s2 p) => s1 -> s2 -> LChoice' p
-- instance Strategy (LChoice' p) p where
--     LChoice' f s $$ p = (f $$ p) <|> (s $$ p)
-- 
-- (<+) :: (Strategy a p, Strategy b p) => a -> b -> LChoice' p
-- a <+ b = LChoice' a b
-- 
-- 
-- -- Basic Strategies
-- data Try' p where Try' :: Strategy s p => s -> Try' p
-- instance Strategy (Try' p) p where
--     Try' s $$ x = s <+ Id' $$ x 
-- 
-- -- this is what I had to do without FunctionalDependencies
-- -- MultiParamTypeClasses mess up type inference
-- -- todo: try out TypeFamilies instead
-- data Repeat' p where Repeat' :: Strategy s p => s -> Repeat' p
-- instance Strategy (Repeat' p) p where
--     Repeat' s $$ p = try' (seq' s (repeat' s)) $$ p where
--         try'    = \a -> Try' a :: Try' p
--         seq'    = \a -> \b -> Seq' a b :: Seq' p
--         repeat' = \a -> Repeat' a :: Repeat' p

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

normalize s = Repeat'' . oncetd $ s

-- Simplification
-- idSimpl :: Seq' p -> 
-- idSimpl (Seq' (Id' :: Id' p) (Id' :: Id' p)) = Id'
-- idSimpl (Seq' _ _) = Id'

data IdSimpl s = IdSimpl
instance Strategy (IdSimpl (Elevate p)) (Elevate p) where
--        s $$ p -> Rewrite p
    --IdSimpl $$ (Seq'' (Id'' :: Elevate p) (Id'' :: Elevate p) :: Elevate p) = Success (Id'' :: Elevate p)
    IdSimpl $$ (Seq'' a b) = Success Id''
    IdSimpl $$ _ = Failure 
    
