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
data Atom = Sword1      | Sword2   | Alice'   | Bob'    | Cyrus'    | Ellie'   | 
            Goldilocks' | Hillary' | Irene'   | Jim'    | Kim'      | Linda'   | 
            Noah'       | Ollie'   | Penny'   | Quine'  | Dagger    | Stuart'  |
            SnowWhite'  | Tom'     | Uli'     | Victor' | Willie'   | Xena'    |
            Zorba'      | Atreyu'  | Cup1     | Cup2    | Bottle1   | Bottle2  | 
            Dress1      | Dress2   | Raft1    | Raft2   | Raft3     | Raft4    |
            Dorothy'    | Fred'    | Glasses1 | Jeans1  | Whiskers' | Mittens' | 
            Gerald'     | Minnie'  | Mickey'  | Sue' | The_Genesee' deriving (Eq, Show, Bounded, Enum)

data MassT = Water' | Wood' | Air'  | Wine'   | Fabric' |
             Metal' | Rust' | Gold' | Advice' | Ice'     | Everything deriving (Eq, Show)

data Entity = Pl' [[Atom]] | Ms' [[Atom]] deriving (Eq, Show)

flatten :: (Eq a) => [[a]] -> [a]
flatten []   = []
flatten [[]] = []
flatten (x:xs) = x ++ (flatten xs)

materializeM :: [MassT] -> Atom -> Mass 
materializeM ts x = M [ts] [[x]]

materializeP :: [MassT] -> Plural -> Mass
materializeP ts (P xs) = M [ts] [(flatten xs)]


fusion :: Mass -> Mass -> Mass
fusion (M ts1 xs) (M ts2 ys) = M (ts1 ++ ts2) (xs ++ ys)

-- Helper
list2OnePlacePred' :: (Eq a) => [a] -> a -> Bool
list2OnePlacePred' xs = \ x -> elem x xs

-- Compose two one-place predicates for Atoms
compose' :: OnePlacePred' -> OnePlacePred' -> OnePlacePred'
compose' p q = \ x -> (p x) && (q x)

-- Predicates
false1 :: OnePlacePred 
false1 e = False

false2 :: TwoPlacePred 
false2 e1 e2 = False

false3 :: ThreePlacePred 
false3 e1 e2 e3 = False

iSum :: Atom -> Atom -> Plural
iSum x y = P [[x,y]]

-- Basically check if lists contain identical elements, regardless of order
equalP :: Plural -> Plural -> Bool
equalP p1 p2 = (p1 `iPart` p2) && (p2 `iPart` p1) 

equalM :: Mass -> Mass -> Bool
equalM m1 m2 = (m1 `mPart` m2) && (m2 `mPart` m1)  

iPart :: Plural -> Plural -> Bool
iPart (P xs) (P ys) = (lenXS == lenFiltered)
    where lenFiltered = length (filter (\x -> x `elem` ys) xs)
          lenXS = length xs
          
mPart :: Mass -> Mass -> Bool
mPart m1 m2 = (m1 `fusion` m2) `equalM` m2 

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs) = x : (filter (\y -> y /= x) (unique xs))

concatP :: Plural -> Plural -> Plural
concatP (P x) (P y) = P (y ++ x)

instance Eq Plural where
    (==) = equalP

instance PartialOrd Plural where
    (<=*) = iPart

instance Join Plural where
    (\/)  = concatP

instance Eq Mass where
    (==) = equalM
        
instance PartialOrd Mass where
    (<=*) = mPart

instance Join Mass where
    (\/) = fusion

coven_ :: Plural
coven_ = P [[Alice', Linda', Irene', Ellie']]

-- All the royalty
court_ :: Plural
court_ = P [[Xena', Atreyu', Victor', Linda']]

coven' :: Entity
coven' = Pl' coven_

court' :: Entity
court' = Pl' court_

couple' :: Atom -> Atom -> Plural
couple' x y = P [[x,y]]

{-
couple :: OnePlacePred
couple (At' x) = False
couple (Pl' (P xs)) = 
couple _ = False

-- CN
raft :: OnePlacePred
raft (At' x) = raft' xs
raft (Pl' (P xs)) = all raft' xs
raft _ = False

person :: OnePlacePred
person (At' x) = person' xs
person (Pl' (P xs)) = all person' xs
person _ = False

-- Only 1 dagger and 2 swords
dagger :: OnePlacePred
dagger (At' x) = dagger' xs
dagger (Pl' (P xs)) = False
dagger _ = False

sword :: OnePlacePred
sword (At' x) = sword' xs
sword (Pl' (P xs)) = all sword' xs
sword _ = False

queen :: OnePlacePred
queen (At' x) = queen' xs
queen (Pl' (P xs)) = all queen' xs
queen _ = False

king :: OnePlacePred
king (At' x) = king' xs
king (Pl' (P xs)) = all king' xs
king _ = False

princess :: OnePlacePred
princess (At' x) = princess' xs
princess (Pl' (P xs)) = all princess' xs
princess _ = False

prince :: OnePlacePred
prince (At' x) = prince' xs
prince (Pl' (P xs)) = all prince' xs
prince _ = False

giant :: OnePlacePred
giant (At' x) = giant' xs
giant (Pl' (P xs)) = all giant' xs
giant _ = False

dwarf :: OnePlacePred
dwarf (At' x) = dwarf' xs
dwarf (Pl' (P xs)) = all dwarf' xs
dwarf _ = False

witch :: OnePlacePred
witch (At' x) = witch' xs
witch (Pl' (P xs)) = all witch' xs
witch _ = False

wizard :: OnePlacePred
wizard (At' x) = wizard' xs
wizard (Pl' (P xs)) = all wizard' xs
wizard _ = False

cup :: OnePlacePred
cup (At' x) = cup' xs
cup (Pl' (P xs)) = all cup' xs
cup _ = False

bottle :: OnePlacePred
bottle (At' x) = bottle' xs
bottle (Pl' (P xs)) = all bottle' xs
bottle _ = False

spy :: OnePlacePred
spy (At' x) = spy' xs
spy (Pl' (P xs)) = all spy' xs
spy _ = False

cat :: OnePlacePred
cat (At' x) = cat' xs
cat (Pl' (P xs)) = all cat' xs
cat _ = False

mouse :: OnePlacePred
mouse (At' x) = mouse' xs
mouse (Pl' (P xs)) = all mouse' xs
mouse _ = False

boy' = young' `compose'` male'

boy :: OnePlacePred
boy (At' x) = boy' x
boy (Pl' (P xs)) = all boy' xs
boy _ = False

girl :: OnePlacePred
girl (At' x) = girl' xs
girl (Pl' (P xs)) = all girl' xs
girl _ = False

man' = old' `compose'` male'

man :: OnePlacePred
man (At' x) = man' x
man (Pl' (P xs)) = all man' xs
man _ = False

woman :: OnePlacePred
woman (At' x) = woman' xs
woman (Pl' (P xs)) = all woman' xs
woman _ = False

-- ADJ
female :: OnePlacePred
female (At' x) = female' x
female (Pl' (P xs)) = all female' xs
female _ = False

male :: OnePlacePred
male (At' x) = male' x
male (Pl' (P xs)) = all male' xs
male _ = False

young :: OnePlacePred
young (At' x) = young' x
young (Pl' (P xs)) = all young' xs
young _ = False

old :: OnePlacePred
old (At' x) = old' x
old (Pl' (P xs)) = all old' xs
old _ = False

being :: OnePlacePred
being (At' x) = being' xs
being (Pl' (P xs)) = all being' xs
being _ = False

thing :: OnePlacePred
thing (At' x) = thing' xs
thing (Pl' (P xs)) = all thing' xs
thing _ = True

rusty :: OnePlacePred
rusty (At' x) = rusty' xs
rusty (Pl' (P xs)) = all rusty' xs
rusty _ = False

foolish :: OnePlacePred
foolish (At' x) = foolish' xs
foolish (Pl' (P xs)) = all foolish' xs
foolish _ = False

wise :: OnePlacePred
wise (At' x) = wise' xs
wise (Pl' (P xs)) = all wise' xs
wise _ = False

dry :: OnePlacePred
dry (At' x) = dry' xs
dry (Pl' (P xs)) = all dry' xs
dry _ = False

wet :: OnePlacePred
wet (At' x) = wet' xs
wet (Pl' (P xs)) = all wet' xs
wet _ = False

short :: OnePlacePred
short (At' x) = short' xs
short (Pl' (P xs)) = all short' xs
short _ = False

tall :: OnePlacePred
tall (At' x) = tall' xs
tall (Pl' (P xs)) = all tall' xs
tall _ = False

dull :: OnePlacePred
dull (At' x) = dull' xs
dull (Pl' (P xs)) = all dull' xs
dull _ = False

shiney :: OnePlacePred
shiney (At' x) = shiney' xs
shiney (Pl' (P xs)) = all shiney' xs
shiney _ = False

numerous :: OnePlacePred
numerous (At' x) = False
numerous (Pl' (P xs)) = 
numerous _ = False

-- INF Verbs
scatter :: OnePlacePred
scatter (At' x) = False
scatter (Pl' (P xs)) = all being' xs
scatter _ = False

swim :: OnePlacePred
swim (At' x) = swim' xs
swim (Pl' (P xs)) = all swim' xs
swim _ = False

walk :: OnePlacePred
walk (At' x) = walk' xs
walk (Pl' (P xs)) = all walk' xs
walk _ = False

run :: OnePlacePred
run (At' x) = run' xs
run (Pl' (P xs)) = all run' xs
run _ = False

smile :: OnePlacePred
smile (At' x) = smile' xs
smile (Pl' (P xs)) = all smile' xs
smile _ = False

laugh :: OnePlacePred
laugh (At' x) = laugh' xs
laugh (Pl' (P xs)) = all laugh' xs
laugh _ = False

-- TV Verbs
surround :: TwoPlacePred
surround (At' x) = surround' xs
surround (Pl' (P xs)) = all surround' xs
surround _ = False

build :: TwoPlacePred
build (At' x) = build' xs
build (Pl' (P xs)) = all build' xs
build _ = False

love :: TwoPlacePred
love (At' x) = build' xs
love (Pl' (P xs)) = all love' xs
love _ = False

help :: TwoPlacePred
help (At' x) = help' xs
help (Pl' (P xs)) = all help' xs
help _ = False

defeat :: TwoPlacePred
defeat (At' x) = defeat' xs
defeat (Pl' (P xs)) = all defeat' xs
defeat _ = False

chase :: TwoPlacePred
chase (At' x) = chase' xs
chase (Pl' (P xs)) = all chase' xs
chase _ = False

drink :: TwoPlacePred
drink (At' x) = drink' xs
drink (Pl' (P xs)) = all drink' xs
drink _ = False

be :: TwoPlacePred
be (At' x) = be' xs
be (Pl' (P xs)) = all be' xs
be _ = False

have :: TwoPlacePred
have (At' x) = have' xs
have (Pl' (P xs)) = all have' xs
have _ = False

-- DV Verbs
give :: ThreePlacePred
give (At' x) = give' xs
give (Pl' (P xs)) = all give' xs
give _ = False

-}

coven :: OnePlacePred
coven x = x == coven'

court :: OnePlacePred
court x = x == court'

atoms :: [Atom]
atoms = [minBound..maxBound]

passivize :: TwoPlacePred -> OnePlacePred
passivize r = \ x -> any (r x) (map At' atoms)