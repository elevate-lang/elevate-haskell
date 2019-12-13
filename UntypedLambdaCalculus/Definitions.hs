module UntypedLambdaCalculus.Definitions where

import UntypedLambdaCalculus.Core

tru = Abs "t" (Abs "f" (Var "t"))
fls = Abs "t" (Abs "f" (Var "f"))

test = Abs "l" (Abs "m" (Abs "n" (App (App (Var "l") (Var "m")) (Var "n"))))