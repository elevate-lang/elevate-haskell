module UntypedLambdaCalculus.Strategies where

import Elevate
import Strategies
import UntypedLambdaCalculus.Core

-- Lambda Calculus Rules

betaReduction :: Strategy Expr
betaReduction = Strategy (\p -> case p of
    r@(App (Abs x e) y) -> success r betaReduction (substitute x y e)
    _                   -> Failure betaReduction) "betaReduction"

-- todo create fresh name
etaAbstraction :: Strategy Expr
etaAbstraction = Strategy (\p -> success p etaAbstraction (Abs "η" (App p (Var "η")))) "etaAbstraction"

-- todo check that it's free
etaReduction :: Strategy Expr
etaReduction = Strategy (\p -> case p of
    r@(Abs x e) -> success r etaReduction e
    _           -> Failure etaReduction) "etaReduction"

-- Evaluation Strategies
normalOrder :: Strategy Expr
normalOrder = normalize betaReduction

callByName :: Strategy Expr
callByName = repeat' (lChoice' betaReduction (lChoice' (function callByName) (argument callByName)))

callByValueStep :: Strategy Expr
callByValueStep = lChoice' (lChoice' (function callByValueStep) (argument callByValueStep)) betaReduction

callByValue :: Strategy Expr
callByValue = repeat' callByValueStep

someOtherOrder :: Strategy Expr
someOtherOrder = repeat' (oncebu betaReduction)

-- Lambda Calculus Traversable Instance
instance Traversable' Expr where
    all' s = Strategy (\p -> case p of 
        r@(Var x) -> success r (all' s) (Var x)
        (Abs x e) -> mapSuccess (\g -> Abs x g) (apply s e)
        (App f e) -> flatMapSuccess (
            Strategy (\a -> mapSuccess (\b -> App a b) (apply s e)) ""
            ) (apply s f)
        ) "all'"

    one' s = Strategy (\p -> case p of 
        (Var x)   -> Failure (one' s)
        (Abs x e) -> mapSuccess (\g -> Abs x g) (apply s e)
        (App f e) -> flatMapFailure (
            \_ -> mapSuccess (\b -> App f b) (apply s e)
            ) (apply s f)
        ) "one'"


-- Lambda Calculus Traversals
body :: Strategy Expr -> Strategy Expr
body s = Strategy (\p -> case p of 
    (Abs x e) -> mapSuccess (\p -> (Abs x p)) (apply s e)
    _         -> Failure (body s)
    ) "body"

function :: Strategy Expr -> Strategy Expr
function s = Strategy (\p -> case p of 
    (App f e) -> mapSuccess (\x -> (App x e)) (apply s f)
    _         -> Failure (function s)
    ) "function"

argument :: Strategy Expr -> Strategy Expr
argument s = Strategy (\p -> case p of
    (App f e) -> mapSuccess (\x -> (App f x)) (apply s e)
    _         -> Failure (argument s)
    ) "argument"