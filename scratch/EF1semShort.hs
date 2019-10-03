module EF1semShort where

import Data.List
import EF1synShort
import PredLsyn
import PredLsem
import Model

lfSent :: Sent -> Formula
lfSent (Sent np vp) = (lfNP np) (lfVP vp)

lfNP :: NP -> (Term -> Formula) -> Formula
lfNP SNOWWHITE     = \ p -> p (Struct "SnowWhite"  [])
lfNP ALICE         = \ p -> p (Struct "Alice"      [])
lfNP DOROTHY       = \ p -> p (Struct "Dorothy"    [])
lfNP GOLDILOCKS    = \ p -> p (Struct "Goldilocks" [])
lfNP LITTLEMOOK    = \ p -> p (Struct "LittleMook" [])
lfNP ATREYU        = \ p -> p (Struct "Atreyu"     [])
lfNP (NP1 det cn)  = (lfDET det) (lfCN cn)
lfNP (NP2 det rcn) = (lfDET det) (lfRCN rcn)

lfCN :: CN -> Term -> Formula
lfCN Girl     = \ t -> Atom "girl"     [t]
lfCN Boy      = \ t -> Atom "boy"      [t]
lfCN Princess = \ t -> Atom "princess" [t]
lfCN Dwarf    = \ t -> Atom "dwarf"    [t]
lfCN Giant    = \ t -> Atom "giant"    [t]
lfCN Wizard   = \ t -> Atom "wizard"   [t]
lfCN Sword    = \ t -> Atom "sword"    [t]
lfCN Dagger   = \ t -> Atom "dagger"   [t]

lfTV :: TV -> Term -> Term -> Formula
lfTV Loved    = \ t2 -> \ t1 -> Atom "love"   [t1,t2]
lfTV Admired  = \ t2 -> \ t1 -> Atom "admire" [t1,t2]
lfTV Helped   = \ t2 -> \ t1 -> Atom "help"   [t1,t2]
lfTV Defeated = \ t2 -> \ t1 -> Atom "defeat" [t1,t2]


lfVP :: VP -> Term -> Formula
lfVP Laughed   = \ t -> Atom "laugh"   [t]
lfVP Cheered   = \ t -> Atom "cheer"   [t]
lfVP Shuddered = \ t -> Atom "shudder" [t]
lfVP Smiled    = \ t -> Atom "smile"   [t]
lfVP (VP1 tv np) = \ subj -> lfNP np (\ obj -> lfTV tv obj subj)


bInLF :: Formula -> [Int]
bInLF (Atom _ _)                  = []
bInLF (Eq _ _)                    = []
bInLF (Neg lf)                    = bInLF lf
bInLF (Impl lf1 lf2)              = bInLFs [lf1,lf2]
bInLF (Equi lf1 lf2)              = bInLFs [lf1,lf2]
bInLF (Conj lfs)                  = bInLFs lfs
bInLF (Disj lfs)                  = bInLFs lfs
bInLF (Forall (Var _ is) f) =
    case is of [] -> bInLF f
               (i : _) -> i : bInLF f
bInLF (Exists (Var _ is) f) =
    case is of [] -> bInLF f
               (i : _) -> i : bInLF f

bInLFs :: [Formula] -> [Int]
bInLFs = nub . concat . map bInLF

freshIndex  :: [Formula] -> Int
freshIndex lfs = i+1
       where i = maximum (0:(bInLFs lfs))

fresh :: [Term -> Formula] -> Int
fresh preds   = freshIndex (map ($ dummy) preds)
  where dummy = Struct "" []


lfDET :: DET -> (Term -> Formula) -> (Term -> Formula) -> Formula
lfDET Some  p q = Exists v (Conj [p v, q v])
        where v = Var "x" [fresh[p,q]]
lfDET A     p q = Exists v (Conj [p v, q v])
        where v = Var "x" [fresh[p,q]]
lfDET Every p q = Forall v (Impl (p v) (q v))
        where v = Var "x" [fresh[p,q]]
lfDET No    p q = Neg (Exists v (Conj [p v,q v]))
        where v = Var "x" [fresh[p,q]]
lfDET The p q = Exists v1 (Conj [Forall v2 (Equi (p v2) (Eq v1 v2)), q v1])
      where i  = fresh[p,q]
            v1 = Var "x" [i]
            v2 = Var "x" [i+1]

lfRCN :: RCN -> Term -> Formula
lfRCN (RCN1 cn _ vp)    = \ t -> Conj [lfCN cn t, lfVP vp t]
lfRCN (RCN2 cn _ np tv) = \ t -> Conj [lfCN cn t, lfNP np (\ subj -> lfTV tv t subj)]

lf1 = lfSent (Sent (NP1 Some Dwarf) (VP1 Defeated (NP1 Some Giant)))
lf2 = lfSent (Sent (NP2 The (RCN2 Wizard That DOROTHY Admired)) Laughed)
lf3 = lfSent (Sent (NP2 The (RCN1 Princess That (VP1 Helped ALICE))) Shuddered)
