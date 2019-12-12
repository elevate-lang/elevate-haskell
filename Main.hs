import UntypedLambdaCalculus.Core
import UntypedLambdaCalculus.Strategies
import Elevate
import Strategies

expr1 = App (Abs "y" (Var "y")) (Var "a")

-- (λx.x) ((λx.x) (λz. (λx.x) z))
idExpr = Abs "x" (Var "x")
expr = App idExpr (App idExpr (Abs "z" (App idExpr (Var "z"))))
strategy = repeat' (function strategy)

simplified = apply normalOrder expr

main :: IO ()
main = putStrLn (show simplified)
