module UntypedLambdaCalculus.Strategies where

import ElevateMaybe
import Strategies
import UntypedLambdaCalculus.Core

import Control.Applicative

-- Lambda Calculus Rules

betaReduction :: Strategy Expr
betaReduction = Strategy (\p -> case p of
    r@(App (Abs x e) y) -> Success (substitute x y e)
    _                   -> Failure) "betaReduction"

-- todo create fresh name
etaAbstraction :: Strategy Expr
etaAbstraction = Strategy (\p -> Success (Abs "η" (App p (Var "η")))) "etaAbstraction"

-- todo check that it's free
etaReduction :: Strategy Expr
etaReduction = Strategy (\p -> case p of
    r@(Abs x e) -> Success e
    _           -> Failure) "etaReduction"

-- Evaluation Strategies
normalOrder :: Strategy Expr
normalOrder = normalize betaReduction

callByName :: Strategy Expr
callByName = repeat' (betaReduction <+ ((function callByName) <+ (argument callByName)))

callByValueStep :: Strategy Expr
callByValueStep = ((function callByValueStep) <+ (argument callByValueStep)) <+ betaReduction

callByValue :: Strategy Expr
callByValue = repeat' callByValueStep

someOtherOrder :: Strategy Expr
someOtherOrder = repeat' (oncebu betaReduction)

-- Lambda Calculus Traversable Instance
instance Traversable' Expr where
    all' s = Strategy (\p -> case p of 
        r@(Var x) -> Success (Var x)
        (Abs x e) -> (\g -> Abs x g) <$> (s $$ e)
        (App f e) -> (s $$ f) >>= (\a -> (\b -> App a b) <$> (s $$ e))  
        ) "all'"

    one' s = Strategy (\p -> case p of 
        (Var x)   -> Failure 
        (Abs x e) -> (\g -> Abs x g) <$> (s $$ e)
        (App f e) -> ((\b -> App b e) <$> (s $$ f)) <|> ((\b -> App f b) <$> (s $$ e))
             
        ) "one'"


-- Lambda Calculus Traversals
body :: Strategy Expr -> Strategy Expr
body s = Strategy (\p -> case p of 
    (Abs x e) -> (\p -> (Abs x p)) <$> (s $$ e)
    _         -> Failure 
    ) "body"

function :: Strategy Expr -> Strategy Expr
function s = Strategy (\p -> case p of 
    (App f e) -> (\x -> (App x e)) <$> (s $$ f)
    _         -> Failure 
    ) "function"

argument :: Strategy Expr -> Strategy Expr
argument s = Strategy (\p -> case p of
    (App f e) -> (\x -> (App f x)) <$> (s $$ e)
    _         -> Failure 
    ) "argument"
