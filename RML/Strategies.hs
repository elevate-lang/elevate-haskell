module RML.Strategies where 

import RML.Core
import Elevate
import Strategies

dead1 :: Strategy Exp
dead1 l@(Let (Vdec t x e1) e2) | 
    toBool ((not' (contains' e1) $ e2) >>= (\_ -> isSafe $ e1)) = 
    Success e2 (trace l "dead1" e2)
dead1 _                        = Failure dead1

-- todo implement isSafe properly
isSafe :: Strategy Exp
isSafe p = Success p freshTrace

instance Traversable' Exp where
    all' s = \p -> case p of 
        (Simple x)   -> Success (Simple x) freshTrace
        (Record x)   -> Success (Record x) freshTrace
        (Select i e) -> Success (Select i e) freshTrace
        (Papp n s)   -> Success (Papp n s) freshTrace
        (MLApp f e)  -> Success (MLApp f e) freshTrace
        (Let v e)    -> (\g -> Let v g) <$$> (s e)
        (Letrec v e) -> (\g -> Letrec v g) <$$> (s e)
        
    one' s = \p -> case p of 
        (Simple x)   -> Failure (one' s)
        (Record x)   -> Failure (one' s)
        (Select i e) -> Failure (one' s)
        (Papp n x)   -> Failure (one' s)
        (MLApp f e)  -> Failure (one' s)
        (Let v e)    -> (\g -> Let v g) <$$> (s e)
        (Letrec v e) -> (\g -> Letrec v g) <$$> (s e)
