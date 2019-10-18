module Model where

import Data.List

-- This constrains type variable a to be an instance of Entity
type OnePlacePred   = Entity -> Bool
type OnePlacePred'   = Atom -> Bool
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
            Stuart'     | Gerald'  | Minnie'  | Mickey' | Sue'      | Lake_Huron' |
            Coven'      | The_Genesee' | Deck1 | Lake_Ontario' |
            Deck2 | Card_1_1 | Card_1_2 | Card_1_3 | Card_1_4 | Card_1_5 | Card_1_6 |
            Card_2_1 | Card_2_2 | Card_2_3 | Card_2_4 | Card_2_5 | Card_2_6 deriving (Eq, Show, Bounded, Enum)

type Plural = [Atom] 

data Mass =  Water | Blood    | Earth | Knowledge | 
             Cloth | Metal    | Air   | Cultery   | 
             MassOfS Atom | MassOfP Plural deriving (Eq, Show)

data Entity = At' Atom | Pl' Plural | Ms' Mass deriving (Eq, Show)

-- Helper
list2OnePlacePred' :: [Atom] -> OnePlacePred'
list2OnePlacePred' xs = \ x -> elem x xs 

-- Compose two one-place predicates
compose :: OnePlacePred -> OnePlacePred -> OnePlacePred
compose p q = \ x -> (p x) && (q x)

--helper :: OnePlacePred -> OnePlacePred -> Bool
--helper xs q = \p -> any (p `compose` q) xs

-- Predicates
false1 :: OnePlacePred 
false1 e = False

false2 :: TwoPlacePred 
false2 e1 e2 = False

false3 :: ThreePlacePred 
false3 e1 e2 e3 = False

{-
girl, boy, princess, dwarf, giant, wizard, sword, dagger, rusty, child, person, man, woman, 
    male, female, thing, laugh, cheer, shudder, smile, wise, foolish, bad, good, rich, poor,
    young, old, heavy, light, dark, clean, dirty, wet, dry, cold, warm, magical, tall, short, 
    long, sharp, dull, shiney, bird, cat, mouse, can_fly, duck, goose, spy :: OnePlacePred
love, admire, help, defeat ::  TwoPlacePred
give :: ThreePlacePred
-}

female' :: OnePlacePred'
female' = list2OnePlacePred' [Zorba', Minnie', Dorothy', Hillary', Xena', Mittens', SnowWhite']

male' :: OnePlacePred'
male' = list2OnePlacePred' [Zorba', Minnie', Dorothy', Hillary', Xena', Mittens', SnowWhite']

female :: OnePlacePred
female (At' x) = female' x
female (Pl' xs) = all female' xs
female _ = False

male :: OnePlacePred
male (At' x) = male' x
male (Pl' xs) = all male' xs
male _ = False

atoms :: [Atom]
atoms = [minBound..maxBound]

passivize :: TwoPlacePred -> OnePlacePred
passivize r = \ x -> any (r x) (map At' atoms)
