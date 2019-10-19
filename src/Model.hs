module Model where

--import Prelude hiding ((<=))
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
            Gerald'     | Minnie'  | Mickey'  | Sue' deriving (Eq, Show, Bounded, Enum)

data Plural = Pluralize [Atom] deriving Show

data Mass = Water' | Wood' | Air'  | Wine'   | Fabric' |
            Metal' | Rust' | Gold' | Advice' | Ice deriving (Eq, Show)

data Entity = At' Atom | Pl' Plural | Ms' Mass deriving (Eq, Show)

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

-- Basically check if lists contain identical elements, regardless of order
equalP :: Plural -> Plural -> Bool
equalP (Pluralize xs) (Pluralize ys) = (lenXS == lenYS) && (lenXS == lenFiltered)
    where lenXS = length xs
          lenYS = length ys
          lenFiltered = length (filter (\x -> x `elem` ys) xs) 

iPart :: Plural -> Plural -> Bool
iPart (Pluralize xs) (Pluralize ys) 
    | (lenXS <= lenYS) && (lenXS == lenFiltered) = True
    | otherwise = False
    where lenXS = length xs
          lenYS = length ys
          lenFiltered = length (filter (\x -> x `elem` ys) xs) 

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs) = x : (filter (\y -> y /= x) (unique xs))

concatP :: Plural -> Plural -> Plural
concatP (Pluralize x) (Pluralize y) = Pluralize (unique (x ++ y))

instance Eq Plural where
    (==) = equalP

instance PartialOrd Plural where
    (<=*) = iPart

instance Join Plural where
    (\/)  = concatP

coven_ :: Plural
coven_ = Pluralize [Alice', Linda', Irene', Ellie']


-- All the royalty
court_ :: Plural
court_ = Pluralize [Xena', Atreyu', Victor', Linda']

coven' :: Entity
coven' = Pl' coven_

court' :: Entity
court' = Pl' court_

couple' :: Atom -> Atom -> Plural
couple' x y = Pluralize [x,y]

{-}
couple :: OnePlacePred
couple (At' x) = False
couple (Pl' (Pluralize xs)) = 
couple _ = False

-- CN
raft :: OnePlacePred
raft (At' x) = raft' xs
raft (Pl' (Pluralize xs)) = all raft' xs
raft _ = False

person :: OnePlacePred
person (At' x) = person' xs
person (Pl' (Pluralize xs)) = all person' xs
person _ = False

-- Only 1 dagger and 2 swords
dagger :: OnePlacePred
dagger (At' x) = dagger' xs
dagger (Pl' (Pluralize xs)) = False
dagger _ = False

sword :: OnePlacePred
sword (At' x) = sword' xs
sword (Pl' (Pluralize xs)) = all sword' xs
sword _ = False

queen :: OnePlacePred
queen (At' x) = queen' xs
queen (Pl' (Pluralize xs)) = all queen' xs
queen _ = False

king :: OnePlacePred
king (At' x) = king' xs
king (Pl' (Pluralize xs)) = all king' xs
king _ = False

princess :: OnePlacePred
princess (At' x) = princess' xs
princess (Pl' (Pluralize xs)) = all princess' xs
princess _ = False

prince :: OnePlacePred
prince (At' x) = prince' xs
prince (Pl' (Pluralize xs)) = all prince' xs
prince _ = False

giant :: OnePlacePred
giant (At' x) = giant' xs
giant (Pl' (Pluralize xs)) = all giant' xs
giant _ = False

dwarf :: OnePlacePred
dwarf (At' x) = dwarf' xs
dwarf (Pl' (Pluralize xs)) = all dwarf' xs
dwarf _ = False

witch :: OnePlacePred
witch (At' x) = witch' xs
witch (Pl' (Pluralize xs)) = all witch' xs
witch _ = False

wizard :: OnePlacePred
wizard (At' x) = wizard' xs
wizard (Pl' (Pluralize xs)) = all wizard' xs
wizard _ = False

cup :: OnePlacePred
cup (At' x) = cup' xs
cup (Pl' (Pluralize xs)) = all cup' xs
cup _ = False

bottle :: OnePlacePred
bottle (At' x) = bottle' xs
bottle (Pl' (Pluralize xs)) = all bottle' xs
bottle _ = False

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

boy' = young' `compose'` male'

boy :: OnePlacePred
boy (At' x) = boy' x
boy (Pl' (Pluralize xs)) = all boy' xs
boy _ = False

girl :: OnePlacePred
girl (At' x) = girl' xs
girl (Pl' (Pluralize xs)) = all girl' xs
girl _ = False

man' = old' `compose'` male'

man :: OnePlacePred
man (At' x) = man' x
man (Pl' (Pluralize xs)) = all man' xs
man _ = False

woman :: OnePlacePred
woman (At' x) = woman' xs
woman (Pl' (Pluralize xs)) = all woman' xs
woman _ = False

-- ADJ
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

being :: OnePlacePred
being (At' x) = being' xs
being (Pl' (Pluralize xs)) = all being' xs
being _ = False

thing :: OnePlacePred
thing (At' x) = thing' xs
thing (Pl' (Pluralize xs)) = all thing' xs
thing _ = True

rusty :: OnePlacePred
rusty (At' x) = rusty' xs
rusty (Pl' (Pluralize xs)) = all rusty' xs
rusty _ = False

foolish :: OnePlacePred
foolish (At' x) = foolish' xs
foolish (Pl' (Pluralize xs)) = all foolish' xs
foolish _ = False

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

numerous :: OnePlacePred
numerous (At' x) = False
numerous (Pl' (Pluralize xs)) = 
numerous _ = False

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

-}

coven :: OnePlacePred
coven x = x == coven'

court :: OnePlacePred
court x = x == court'

atoms :: [Atom]
atoms = [minBound..maxBound]

passivize :: TwoPlacePred -> OnePlacePred
passivize r = \ x -> any (r x) (map At' atoms)