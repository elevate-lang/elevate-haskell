module UntypedLambdaCalculus.Strategies where

import Elevate
import Strategies
import UntypedLambdaCalculus.Core

-- Lambda Calculus Rules

betaReduction :: Strategy Expr
betaReduction (App (Abs x e) y) = success (substitute x y e)
betaReduction _                 = Failure betaReduction

-- todo create fresh name
etaAbstraction :: Strategy Expr
etaAbstraction p = success (Abs "η" (App p (Var "η")))

-- todo check that it's free
etaReduction :: Strategy Expr
etaReduction (Abs x e) = success e
etaReduction _         = Failure etaReduction


-- Evaluation Strategies
normalOrder :: Strategy Expr
normalOrder = normalize betaReduction

callByName :: Strategy Expr
callByName = repeat' (lChoice' betaReduction (lChoice' (function callByName) (argument callByName)))

callByValueStep :: Strategy Expr
callByValueStep = lChoice' (lChoice' (function callByValueStep) (argument callByValueStep)) betaReduction

callByValue :: Strategy Expr
callByValue = repeat' callByValueStep
--callByValue = repeat' (lChoice' (lChoice' (function callByValue) (argument callByValue)) betaReduction)

someOtherOrder :: Strategy Expr
someOtherOrder = repeat' (oncebu betaReduction)

-- Lambda Calculus Traversable Instance
instance Traversable' Expr where
    all' s (Var x)   = success (Var x)
    all' s (Abs x e) = mapSuccess (\g -> Abs x g) (s e)
    all' s (App f e) = flatMapSuccess (\a -> mapSuccess (\b -> App a b) (s e)) (s f)

    one' s (Var x)   = Failure (one' s)
    one' s (Abs x e) = mapSuccess (\g -> Abs x g) (s e)
    one' s (App f e) = flatMapFailure (\_ -> mapSuccess (\b -> App f b) (s e)) (s f)


-- Lambda Calculus Traversals
body :: Strategy Expr -> Strategy Expr
body s (Abs x e) = mapSuccess (\p -> (Abs x p)) (s e)
body s _         = Failure (body s)

function :: Strategy Expr -> Strategy Expr
function s (App f e) = mapSuccess (\x -> (App x e)) (s f)
function s _         = Failure (function s)

argument :: Strategy Expr -> Strategy Expr
argument s (App f e) = mapSuccess (\x -> (App f x)) (s e)
argument s _         = Failure (argument s)
