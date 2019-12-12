import UntypedLambdaCalculus.Core
import UntypedLambdaCalculus.Strategies
import Elevate
import Strategies

expr1 = App (Abs "y" (Var "y")) (Var "a")

-- (λx.x) ((λx.x) (λz. (λx.x) z))
idExpr = Abs "x" (Var "x")
expr = App idExpr (App idExpr (Abs "z" (App idExpr (Var "z"))))
strategy = repeat' (function strategy)

simplified = apply callByValue expr

main :: IO ()
main = do
    putStrLn ""
    putStrLn ((show "input: ") ++ (show expr))
    putStrLn ""
    putStrLn (show "NormalOrder:")
    putStrLn (show (apply normalOrder expr))
    putStrLn ""
    putStrLn (show "CallByValue:")
    putStrLn (show (apply callByValue expr))
    putStrLn ""
    putStrLn (show "CallByName:")
    putStrLn (show (apply callByName expr))


