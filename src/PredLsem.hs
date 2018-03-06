module PredLsem where

import Data.List
import PredLsyn
import Model

type Interp a = Name -> [a] -> Bool

int0 :: Interp Entity
int0 "laugh"   = \ [x]     -> laugh x
int0 "cheer"   = \ [x]     -> cheer x
int0 "shudder" = \ [x]     -> shudder x
int0 "smile"   = \ [x]     -> smile x
int0 "love"    = \ [x,y]   -> love y x
int0 "admire"  = \ [x,y]   -> admire y x
int0 "help"    = \ [x,y]   -> help y x
int0 "defeat"  = \ [x,y]   -> defeat y x
int0 "give"    = \ [x,y,z] -> give z y x
-- the following are needed only for EF1, not for PredL/FOL
int0 "girl"       = \ [x]     -> girl x
int0 "boy"        = \ [x]     -> boy x
int0 "princess"   = \ [x]     -> princess x
int0 "dwarf"      = \ [x]     -> dwarf x
int0 "giant"      = \ [x]     -> giant x
int0 "wizard"     = \ [x]     -> wizard x
int0 "sword"      = \ [x]     -> sword x
int0 "dagger"     = \ [x]     -> dagger x

type Assignment a = Term -> a

change :: Assignment a -> Term -> a -> Assignment a
change g x d
    | isVar x = \ v -> if x == v then d else g v
    | otherwise = error "This term is not a variable."

ass0 :: Assignment Entity
ass0 = \ v -> Alice

ass1 :: Assignment Entity
ass1 = change ass0 y Bob

eval :: Eq a =>
    [a]              ->
    Interp a         ->
    Assignment a     ->
    Formula          -> Bool

eval domain i = eval' where
  eval' g (Atom str vs) = i str (map g vs)
  eval' g (Eq   v1 v2)  = (g v1) == (g v2)
  eval' g (Neg  f)      = not (eval' g f)
  eval' g (Impl f1 f2)  = not ((eval' g f1) &&
                               not (eval' g f2))
  eval' g (Equi f1 f2)  = (eval' g f1) == (eval' g f2)
  eval' g (Conj fs)     = and (map (eval' g) fs)
  eval' g (Disj fs)     = or  (map (eval' g) fs)
  eval' g (Forall v f)
              | isVar v = and [eval' (change g v d) f | d <- domain]
              | otherwise = error "Cannot quantify over a term that is not a variable."
  eval' g (Exists v f)
              | isVar v = or [eval' (change g v d) f | d <- domain]
              | otherwise = error "Cannot quantify over a term that is not a variable."


-- we will now generalize the interpretation function to cover structured terms too

type FInterp a = Name -> [a] -> a

fint0 :: FInterp Entity
fint0 "Alice"  []       = Alice
fint0 "Dorothy"  []     = Dorothy
fint0 "Goldilocks"  []  = Goldilocks
fint0 "LittleMook"  []  = LittleMook
fint0 "SnowWhite"  []   = SnowWhite
fint0 "Atreyu"  []      = Atreyu
-- we can keep adding as many of these as we have entities in our domain (see the Model module)
fint0 "Bob"  []         = Bob
fint0 "Cyrus"  []       = Cyrus
fint0 "Ellie"  []       = Ellie
fint0 "Fred"  []        = Fred
-- etc.

type FVal a = Term -> a

liftAssignment :: FInterp a -> Assignment a -> FVal a
liftAssignment fint g v@(Var _ _)     = g v
liftAssignment fint g (Struct str ts) =
           fint str (map (liftAssignment fint g) ts)

evl :: Eq a =>
  [a]           ->
  Interp  a     ->
  FInterp a     ->
  Assignment  a ->
  Formula       -> Bool

evl domain i fint = evl' where
   lift = liftAssignment fint
   evl' g (Atom str ts) = i str (map (lift g) ts)
   evl' g (Eq   t1 t2)  = lift g t1 == lift g t2
   evl' g (Neg  f)      = not (evl' g f)
   evl' g (Impl f1 f2)  = not ((evl' g f1) &&
                               not (evl' g f2))
   evl' g (Equi f1 f2)  = evl' g f1 == evl' g f2
   evl' g (Conj fs)     = and (map (evl' g) fs)
   evl' g (Disj fs)     = or  (map (evl' g) fs)
   evl' g (Forall v f)
              | isVar v = and [ evl' (change g v d) f | d <- domain ]
              | otherwise = error "Cannot quantify over a term that is not a variable."
   evl' g (Exists v f)
              | isVar v = or [ evl' (change g v d) f | d <- domain ]
              | otherwise = error "Cannot quantify over a term that is not a variable."
