module UntypedLambdaCalculus.Strategies where

import Elevate
import Strategies
import UntypedLambdaCalculus.Core

-- Lambda Calculus Rules

betaReduction :: Strategy Expr
betaReduction (App (Abs x e) y) = Success (substitute x y e)
betaReduction _                 = Failure betaReduction

-- todo create fresh name
etaAbstraction :: Strategy Expr
etaAbstraction p = Success (Abs "η" (App p (Var "η")))

-- todo check that it's free
etaReduction :: Strategy Expr
etaReduction (Abs x e) = Success e
etaReduction _         = Failure etaReduction


-- Evaluation Strategies
normalOrder :: Strategy Expr
normalOrder = normalize betaReduction

-- Lambda Calculus Traversable Instance
instance Traversable' Expr where
    all' s (Var x)   = Success (Var x)
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
