module RML.Core where

data TExp = Funtype [TExp] TExp
          | Recordtype [TExp]
          | Primtype String
          deriving (Show, Eq)

data Vdec = Vdec TExp String Exp deriving (Show, Eq)

data Fdec = Fdec TExp String [String] Exp deriving (Show, Eq)

data Se = Const TExp String
        | MLVar String
        deriving (Show, Eq)

data Exp = Simple Se
         | Record [Se]
         | Select Int Se
         | Papp String [Se]
         | MLApp Se [Se]
         | Let Vdec Exp
         | Letrec [Fdec] Exp
         deriving (Show, Eq)
