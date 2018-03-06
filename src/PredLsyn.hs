module PredLsyn where

import Data.List

type Name = String
type Index = [Int]
data Term = Var Name Index | Struct Name [Term] deriving (Eq,Ord)

instance Show Term where
  show (Var name [])  = name
  show (Var name [i]) = name ++ show i
  show (Var name is) = name ++ showInts is
     where showInts []     = ""
           showInts [i]    = show i
           showInts (i:is) = show i ++ "_" ++ showInts is
  show (Struct s []) = s
  show (Struct s ts) = s ++ show ts

x, y, z :: Term
x = Var "x" []
y = Var "y" []
z = Var "z" []

data Formula = Atom Name [Term] | Eq Term Term | Neg Formula | Impl Formula Formula | Equi Formula Formula | Conj [Formula] | Disj [Formula] | Forall Term Formula | Exists Term Formula deriving Eq

instance Show Formula where
  show (Atom s [])   = s
  show (Atom s xs)   = s ++ show xs
  show (Eq t1 t2)    = show t1 ++ " = " ++ show t2
  show (Neg form)    = "~ " ++ (show form)
  show (Impl f1 f2)  = "(" ++ show f1 ++ " => " ++ show f2 ++ ")"
  show (Equi f1 f2)  = "(" ++ show f1 ++ " <==> " ++ show f2 ++ ")"
  show (Conj [])     = "True"
  show (Conj fs)     = "& " ++ show fs
  show (Disj [])     = "False"
  show (Disj fs)     = "V " ++ show fs
  show (Forall v f)  = "A " ++  show v ++ (' ' : show f)
  show (Exists v f)  = "E " ++  show v ++ (' ' : show f)

isVar :: Term -> Bool
isVar (Var _ _) = True
isVar _ = False

varsInTerm :: Term -> [Term]
varsInTerm v@(Var name index) = [v]
varsInTerm (Struct s ts) = varsInTerms ts

varsInTerms :: [Term] -> [Term]
varsInTerms = nub . concat . map varsInTerm
