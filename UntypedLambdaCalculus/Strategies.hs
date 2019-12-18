module UntypedLambdaCalculus.Strategies where

import Elevate
import Strategies
import UntypedLambdaCalculus.Core

import Control.Applicative

-- Lambda Calculus Rules

betaReduction :: Strategy Expr
betaReduction r@(App (Abs x e) y) = Success (substitute x y e) (addTrace r "betaReduction" (substitute x y e))
betaReduction _                   = Failure betaReduction

-- todo create fresh name
etaAbstraction :: Strategy Expr
etaAbstraction = \p -> Success (Abs "η" (App p (Var "η"))) ["etaAbstraction"]

-- todo check that it's free
etaReduction :: Strategy Expr
etaReduction r@(Abs x e) = Success e ["etaReduction"]
etaReduction _           = Failure etaReduction 

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
    all' s = \p -> case p of 
        r@(Var x) -> Success (Var x) ["all"]
        (Abs x e) -> (\g -> Abs x g) <$> (s e)
        (App f e) -> (s f) >>= (\a -> (\b -> App a b) <$> (s e))  
        
    one' s = \p -> case p of 
        (Var x)   -> Failure (one' s)
        (Abs x e) -> appendTrace ["abs"] ((\g -> Abs x g) <$> (s e))
        (App f e) -> appendTrace ["app"] (((\b -> App b e) <$> (s f)) <|> ((\b -> App f b) <$> (s e)))

-- Lambda Calculus Traversals
body :: Strategy Expr -> Strategy Expr
body s = \p -> case p of 
    (Abs x e) -> (\p -> (Abs x p)) <$> (s e)
    _         -> Failure (body s)

function :: Strategy Expr -> Strategy Expr
function s = \p -> case p of 
    (App f e) -> (\x -> (App x e)) <$$> (s f)
    _         -> Failure (function s)

argument :: Strategy Expr -> Strategy Expr
argument s = \p -> case p of
    (App f e) -> (\x -> (App f x)) <$$> (s e)
    _         -> Failure (function s)
