module Model where

import Data.List
import Semilattice

-- This constrains type variable a to be an instance of Entity
type OnePlacePred   = Entity -> Bool
type OnePlacePred'  = Atom -> Bool
type TwoPlacePred   = Entity -> Entity -> Bool
type ThreePlacePred = Entity -> Entity -> Entity -> Bool

-- Helper Functions
curry2 :: ((a,b) -> c) -> b -> a -> c
curry2 f y x = f (x,y)

curry3 :: ((a,b,c) -> d) -> c -> b -> a -> d
curry3 f z y x = f (x,y,z)

self ::  (a -> a -> b) -> a -> b
self p = \ x -> p x x

-- Entity Types
data Atom = Knife1      | Knife2   | Alice'   | Bob'    | Cyrus'    | Ellie'   | 
            Goldilocks' | Hillary' | Irene'   | Jim'    | Kim'      | Linda'   | 
            Noah'       | Ollie'   | Penny'   | Quine'  |
            SnowWhite'  | Tom'     | Uli'     | Victor' | Willie'   | Xena'    | Cup2    |
            Spoon1      | Spoon2   | Zorba'   | Atreyu' | Fork1     | Fork2    | Cup1    |
            Dress1      | Dress2   | Shoe1    | Shoe2   | Shoe3     | Shoe4    | Bottle2 |
            Dorothy'    | Fred'    | Glasses1 | Jeans1  | Whiskers' | Mittens' | Bottle1 | 
            Stuart'     | Gerald'  | Minnie'  | Mickey' | Sue'      | 
            Raft1 | Raft2 | Raft3   | Raft4   | The_Genesee' | Lake_Ontario' deriving (Eq, Show, Bounded, Enum)

data Plural = Pluralize [Atom] deriving Show

instance Eq Plural where
    (==) = equalP

data Mass =  Water | Blood    | Earth | Knowledge | 
             Cloth | Metal    | Air   | Cultery   | 
             MassOfS Atom | MassOfP Plural deriving (Eq, Show)

data Entity = At' Atom | Pl' Plural | Ms' Mass deriving (Eq, Show)

-- Helper
list2OnePlacePred' :: (Eq a) => [a] -> a -> Bool
list2OnePlacePred' xs = \ x -> elem x xs

-- Compose two one-place predicates for Atoms
compose' :: OnePlacePred' -> OnePlacePred' -> OnePlacePred'
compose' p q = \ x -> (p x) && (q x)

--helper :: OnePlacePred -> OnePlacePred -> Bool
--helper xs q = \p -> any (p `compose` q) xs

-- Predicates
false1 :: OnePlacePred 
false1 e = False

false2 :: TwoPlacePred 
false2 e1 e2 = False

false3 :: ThreePlacePred 
false3 e1 e2 e3 = False

equalP :: Plural -> Plural -> Bool
equalP (Pluralize xs) (Pluralize ys) = (lenXS == lenYS) && (lenXS == lenFiltered)
    where lenXS = length xs
          lenYS = length ys
          lenFiltered = length (filter (\x -> x `elem` ys) xs) 
  

instance Join Plural where
    (\/)  = \(Pluralize x) (Pluralize y) -> Pluralize (x ++ y)
{-
instance Join Mass where
    (\/) x y = mSum
    (=~=) x y = equalM
    (<=) x y = mPart
-}

--iPart :: Atom -> Plural -> Bool
--iPart (Pluralize x) (Pluralize y) = x `elem` y
  
--iSum :: Atom -> Atom -> Plural
--iSum x y = x ++ y


coven_ :: Plural
coven_ = Pluralize [Alice', Linda', Irene', Ellie']


-- All the royalty
court_ :: Plural
court_ = Pluralize [Xena', Atreyu', Victor', Linda']

coven' :: Entity
coven' = Pl' coven_

court' :: Entity
court' = Pl' court_

couple :: Atom -> Atom -> Plural
couple x y = Pluralize [x,y]
 
female' :: Atom -> Bool
female' = list2OnePlacePred' [Minnie', Dorothy', Hillary', Xena', Mittens', SnowWhite']

male' :: Atom -> Bool
male' = list2OnePlacePred' [Zorba', Whiskers', Gerald', Victor', Stuart', Ollie']

young' :: Atom -> Bool
young' = list2OnePlacePred' [Atreyu', Alice', Ollie']

boy' = young' `compose'` male'

female :: OnePlacePred
female (At' x) = female' x
female (Pl' (Pluralize xs)) = all female' xs
female _ = False

male :: OnePlacePred
male (At' x) = male' x
male (Pl' (Pluralize xs)) = all male' xs
male _ = False

young :: OnePlacePred
young (At' x) = young' x
young (Pl' (Pluralize xs)) = all young' xs
young _ = False

old :: OnePlacePred
old (At' x) = old' x
old (Pl' (Pluralize xs)) = all old' xs
old _ = False

boy :: OnePlacePred
boy (At' x) = boy' x
boy (Pl' (Pluralize xs)) = all boy' xs
boy _ = False

girl :: OnePlacePred
girl (At' x) = girl' xs
girl (Pl' (Pluralize xs)) = all girl' xs
girl _ = False

girl :: OnePlacePred
girl (At' x) = girl' xs
girl (Pl' (Pluralize xs)) = all girl' xs
girl _ = False

girl :: OnePlacePred
girl (At' x) = girl' xs
girl (Pl' (Pluralize xs)) = all girl' xs
girl _ = False




{-
girl, boy, princess, dwarf, giant, wizard, sword, dagger, rusty, child, person, man, woman, 
    male, female, thing, laugh, smile, wise, foolish, 
    young, old, heavy, light, clean, dirty, wet, dry, magical, tall, short, 
    long, sharp, dull :: OnePlacePred
love, admire, help, defeat ::  TwoPlacePred
give :: ThreePlacePred
-}

wise :: OnePlacePred
wise (At' x) = wise' xs
wise (Pl' (Pluralize xs)) = all wise' xs
wise _ = False

dry :: OnePlacePred
dry (At' x) = dry' xs
dry (Pl' (Pluralize xs)) = all dry' xs
dry _ = False

wet :: OnePlacePred
wet (At' x) = wet' xs
wet (Pl' (Pluralize xs)) = all wet' xs
wet _ = False

short :: OnePlacePred
short (At' x) = short' xs
short (Pl' (Pluralize xs)) = all short' xs
short _ = False

tall :: OnePlacePred
tall (At' x) = tall' xs
tall (Pl' (Pluralize xs)) = all tall' xs
tall _ = False

dull :: OnePlacePred
dull (At' x) = dull' xs
dull (Pl' (Pluralize xs)) = all dull' xs
dull _ = False

shiney :: OnePlacePred
shiney (At' x) = shiney' xs
shiney (Pl' (Pluralize xs)) = all shiney' xs
shiney _ = False

spy :: OnePlacePred
spy (At' x) = spy' xs
spy (Pl' (Pluralize xs)) = all spy' xs
spy _ = False

cat :: OnePlacePred
cat (At' x) = cat' xs
cat (Pl' (Pluralize xs)) = all cat' xs
cat _ = False

mouse :: OnePlacePred
mouse (At' x) = mouse' xs
mouse (Pl' (Pluralize xs)) = all mouse' xs
mouse _ = False

-- INF Verbs
scatter :: OnePlacePred
scatter (At' x) = False
scatter (Pl' (Pluralize xs)) = all being' xs
scatter _ = False

swim :: OnePlacePred
swim (At' x) = swim' xs
swim (Pl' (Pluralize xs)) = all swim' xs
swim _ = False

walk :: OnePlacePred
walk (At' x) = walk' xs
walk (Pl' (Pluralize xs)) = all walk' xs
walk _ = False

run :: OnePlacePred
run (At' x) = run' xs
run (Pl' (Pluralize xs)) = all run' xs
run _ = False

smile :: OnePlacePred
smile (At' x) = smile' xs
smile (Pl' (Pluralize xs)) = all smile' xs
smile _ = False

laugh :: OnePlacePred
laugh (At' x) = laugh' xs
laugh (Pl' (Pluralize xs)) = all laugh' xs
laugh _ = False

-- TV Verbs
surround :: TwoPlacePred
surround (At' x) = surround' xs
surround (Pl' (Pluralize xs)) = all surround' xs
surround _ = False

build :: TwoPlacePred
build (At' x) = build' xs
build (Pl' (Pluralize xs)) = all build' xs
build _ = False

love :: TwoPlacePred
love (At' x) = build' xs
love (Pl' (Pluralize xs)) = all love' xs
love _ = False

help :: TwoPlacePred
help (At' x) = help' xs
help (Pl' (Pluralize xs)) = all help' xs
help _ = False

defeat :: TwoPlacePred
defeat (At' x) = defeat' xs
defeat (Pl' (Pluralize xs)) = all defeat' xs
defeat _ = False

chase :: TwoPlacePred
chase (At' x) = chase' xs
chase (Pl' (Pluralize xs)) = all chase' xs
chase _ = False

drink :: TwoPlacePred
drink (At' x) = drink' xs
drink (Pl' (Pluralize xs)) = all drink' xs
drink _ = False

be :: TwoPlacePred
be (At' x) = be' xs
be (Pl' (Pluralize xs)) = all be' xs
be _ = False

have :: TwoPlacePred
have (At' x) = have' xs
have (Pl' (Pluralize xs)) = all have' xs
have _ = False

-- DV Verbs
give :: ThreePlacePred
give (At' x) = give' xs
give (Pl' (Pluralize xs)) = all give' xs
give _ = False

-- Collective Predicates
coven :: OnePlacePred
coven x = x == coven'

court :: OnePlacePred
court x = x == court'

-- Distributive Predicates


atoms :: [Atom]
atoms = [minBound..maxBound]

passivize :: TwoPlacePred -> OnePlacePred
passivize r = \ x -> any (r x) (map At' atoms)