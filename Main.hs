import UntypedLambdaCalculus.Core
import UntypedLambdaCalculus.Strategies
import UntypedLambdaCalculus.Definitions
import UntypedLambdaCalculus.Parser
-- import Elevate
-- import Strategies

expr1 = App (Abs "y" (Var "y")) (Var "a")

-- (λx.x) ((λx.x) (λz. (λx.x) z))
idExpr = Abs "x" (Var "x")
expr = App idExpr (App idExpr (Abs "z" (App idExpr (Var "z"))))
-- strategy = repeat' (function strategy)

-- simplified = normalOrder expr

bools = App (App (App test tru) $ Var "v") $ Var "w"
bools2 = App (App and' tru) tru

pairs = App snd' (App (App pair (Var "v")) (Var "w"))

main :: IO ()
main = do
    putStrLn ""
    putStrLn $ show simplified
