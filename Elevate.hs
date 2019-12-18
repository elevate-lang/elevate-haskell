module Elevate where 

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Maybe
import Data.List

-- Strategy
type Strategy p = p -> Rewrite p

-- Rewrite 
data Rewrite p = Success p Trace
               | Failure (Strategy p)

-- Trace: [(Redex, Rule, Result)]
-- Trace is stored "backwards": (head t) is the most recent step
type Trace = [RewriteStep]
type RewriteStep = (String, String, String)

-- Naive Strategies
id' :: Show p => Strategy p 
id' = \p -> Success p (trace p "id'" p)

fail' :: Strategy p
fail' = \_ -> Failure fail'

-- Trace Utils
trace :: Show p => p -> String -> p -> Trace 
trace redex rule result = [(show redex, rule, show result)]

appendTrace :: Trace -> Rewrite b -> Rewrite b
appendTrace t1 (Success p t2) = Success p (t2 ++ t1)
appendTrace _  (Failure s)   = Failure s


-- Make everything look nice
instance Show p => Show (Rewrite p) where
    show (Success p t) = "Success " ++ show p ++ "\nTrace:\n" ++ printTrace t
    show (Failure s)   = "Failure" 


-- instances: Functor / Applicative / Monad / Alternative / MonadPlus
instance Functor Rewrite where
    fmap f (Success p t) = Success (f p) t
    fmap _ (Failure _)   = Failure fail' -- we just give up

-- I would like to have this in fmap as well but there p might not implement show
updateTrace :: Show p => Rewrite p -> Rewrite p
updateTrace (Success p ((r,s,e):ts)) = Success p ((r,s,show p):ts)
updateTrace (Failure s)        = Failure s
(<$$>) :: Show b => (a -> b) -> Rewrite a -> Rewrite b
(<$$>) f x = updateTrace (f <$> x)

-- are we acutally using this?
freshTrace = [("","FRESH","")]
instance Applicative Rewrite where
    pure = \p -> Success p freshTrace
    (Success f t) <*> m = f <$> m
    (Failure s)   <*> m = Failure fail'

instance Monad Rewrite where
    return = pure
    (Success p t) >>= b = appendTrace t (b p) -- former 'flatMapSuccess'
    (Failure s)   >>= b = Failure fail'

instance Alternative Rewrite where
    empty = Failure fail'
    (Success p t) <|> _ = Success p t -- former 'flatMapFailure'
    (Failure s)   <|> m = m

instance MonadPlus Rewrite where
    mzero = Failure fail'
    mplus = (<|>)


-- UTILS 

-- print trace
printRewriteStep :: RewriteStep -> String
printRewriteStep (r,s,p) = r ++ " " ++ s ++ " " ++ p

printTrace :: Trace -> String
printTrace [x] = printRewriteStep x
printTrace (x:xs)    = printTrace xs ++ "\n" ++ printRewriteStep x

--
-- Pretty printing the derivation
--
-- Arguments: p         : initial expression which was rewritten
--            Rewrite p : the result of rewriting that expression
generateDerivation :: Show p => p -> Rewrite p -> String
generateDerivation _ (Failure _)   = "Failure"
generateDerivation p (Success _ t) = 
    let trace = reverse t                             -- "fix" order again
        maxLength = maxExprLength p trace             -- what's the longest expression in the derivation?
        wsToRule = \x -> whitespaceToRule x maxLength -- how much space to add until rule label for expr 'x'
        foldOp = foldOpGen wsToRule                   -- bind that function to foldOp
    in (fst (foldl foldOp ("", show p) trace)) ++ 
    (case (last trace) of
        (r,s,p) -> p)

-- What's the longest resulting expression in the derivation (including the input expression)?
maxExprLength :: Show p => p -> Trace -> Int
maxExprLength p t = foldl (\acc x -> case x of
    (_,_,e) -> max acc (length (show e))
        ) (length (show p)) t

-- Given an expression, add enough whitespace before we print rule label
whitespaceToRule :: String -> Int -> String
whitespaceToRule p l = replicate (l - (length p) + 2) ' '

-- pretty prints a single step for the derivation
--           wsToRule              (deriv,  parent)    y              (finalDeriv, lastResult)
foldOpGen :: (String -> String) -> (String, String) -> RewriteStep -> (String, String)
foldOpGen wsToRule (deriv, parent) (redex, rule, result) = (
    -- add parent + ' some-whitespace ' + [ruleLabel]
    deriv ++ parent ++ (wsToRule parent) ++ "[" ++ rule ++ "]\n" ++
    -- underline redex from current step in parent
    (underline (fromJust (findString redex parent)) (length redex)) ++ "\n",
    result)

underline :: Int -> Int -> String
underline ws dash = replicate ws ' ' ++ replicate dash '-'

findString :: (Eq a) => [a] -> [a] -> Maybe Int
findString search str = findIndex (isPrefixOf search) (tails str)
