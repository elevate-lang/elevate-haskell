{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module UntypedLambdaCalculus.Strategies where

import Elevate
import Strategies
import UntypedLambdaCalculus.Core

import Control.Applicative

-- Lambda Calculus Rules

data BetaReduction = BetaReduction
instance Strategy BetaReduction Expr where
    BetaReduction $$ r@(App (Abs x e) y) = Success (substitute x y e)
    BetaReduction $$ _                   = Failure -- BetaReduction

-- todo create fresh name
data EtaAbstraction = EtaAbstraction
instance Strategy EtaAbstraction Expr where
    EtaAbstraction $$ p = Success (Abs "η" (App p (Var "η")))
    
-- todo check that it's free
data EtaReduction = EtaReduction
instance Strategy EtaReduction Expr where
    EtaReduction $$ Abs x e = Success e 
    EtaReduction $$ _       = Failure -- EtaReduction

-- Evaluation Strategies
normalOrder = normalize BetaReduction

callByName = Repeat'' (BetaReduction <+ ((Function callByName) <+ (Argument callByName)))

callByValueStep = ((Function callByValueStep) <+ (Argument callByValueStep)) <+ BetaReduction
callByValue = Repeat'' callByValueStep

-- Lambda Calculus Traversable Instance
instance Traversable' Expr where
    all' s = \p -> case p of 
        (Var x) -> Success (Var x) 
        (Abs x e) -> (\g -> Abs x g) <$> (s $$ e)
        (App f e) -> (s $$ f) >>= (\a -> (\b -> App a b) <$> (s $$ e))  
        
    one' s = \p -> case p of 
        (Var x)   -> Failure -- (one' s)
        (Abs x e) -> ((\g -> Abs x g) <$> (s $$ e))
        (App f e) -> ((\b -> App b e) <$> (s $$ f)) <|> ((\b -> App f b) <$> (s $$ e))

-- Lambda Calculus Traversals

-- this requires FlexibleContexts
data Body where Body :: Strategy s Expr => s -> Body
instance Strategy Body Expr where
    Body s $$ (Abs x e) = (\p -> (Abs x p)) <$> (s $$ e)
    Body s $$ _         = Failure

data Function where Function :: Strategy s Expr => s -> Function
instance Strategy Function Expr where
    Function s $$ (App f e) = (\x -> (App x e)) <$> (s $$ f)
    Function s $$ _         = Failure

data Argument where Argument :: Strategy s Expr => s -> Argument
instance Strategy Argument Expr where
    Argument s $$ (App f e) = (\x -> (App x e)) <$> (s $$ e)
    Argument s $$ _         = Failure
