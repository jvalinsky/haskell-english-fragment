{-# LANGUAGE TypeFamilies, AllowAmbiguousTypes, TypeOperators, RankNTypes, ConstrainedClassMethods, FlexibleContexts #-}

module Model where

import Data.List

curry2 :: ((a,b) -> c) -> b -> a -> c
curry2 f y x = f (x,y)

curry3 :: ((a,b,c) -> d) -> c -> b -> a -> d
curry3 f z y x = f (x,y,z)

self ::  (a -> a -> b) -> a -> b
self p = \ x -> p x x

-- Entity Types
data Singular = Knife1     | Knife2  | Alice | Bob     | Cyrus  | Ellie | 
                Goldilocks | Hillary | Irene | Jim     | Kim    | Linda | 
                LittleMook | Noah    | Ollie | Penny   | Quine  | Remmy | 
                SnowWhite  | Tom     | Uli   | Victor  | Willie | Xena  | 
                Spoon1     | Spoon2  | Zorba | Atreyu  | Fork1  | Fork2 |
                Dress1     | Dress2  | Shoe1 | Shoe2   | Shoe3  | Shoe4 |
                Dorothy    | Fred deriving (Eq,Show, Read, Bounded,Enum)


data Plural = Plural [Singular] deriving (Eq,Show, Read)

data Mass =  Water | Blood | Earth | Knowledge | 
             Cloth | Metal | Air   | Cultery   | 
             MassOfS Singular | MassOfP Plural deriving (Eq,Show, Read)

-- This constrains type variable a to be an instance of Entity
type OnePlacePred   a = Entity a => a -> Bool
type TwoPlacePred   a = Entity a => a -> a -> Bool
type ThreePlacePred a = Entity a => a -> a -> a -> Bool

class (Show entity, Read entity, Eq entity) => Entity entity where
    -- Helper
    list2OnePlacePred :: [entity] -> OnePlacePred entity
    list2OnePlacePred xs = \ x -> elem x xs

    -- Predicates
    girl, boy, princess, dwarf, giant, wizard, sword, dagger, rusty, child, person, man, woman, 
        male, female, thing, neutral, laugh, cheer, shudder, smile :: OnePlacePred entity
    love, admire, help, defeat ::  TwoPlacePred entity
    give :: ThreePlacePred entity

    false' :: OnePlacePred entity -> Bool
    false' p = False    

class (Bounded entity, Enum entity, Entity entity) => AtomicEntity entity where
    -- Compose two one-place predicates
    compose :: OnePlacePred entity -> OnePlacePred entity -> Bool
    compose x y =  (filter x entities) `intersect` (filter y entities) /= []

    entities :: [entity]
    entities = [minBound..maxBound]

    passivize :: TwoPlacePred entity -> OnePlacePred entity
    passivize r = \ x -> any (r x) entities

instance Entity Singular where
    girl     = list2OnePlacePred [SnowWhite,Alice,Dorothy,Goldilocks]
    boy      = list2OnePlacePred [LittleMook,Atreyu]
    princess = list2OnePlacePred [Ellie, Xena]
    dwarf    = list2OnePlacePred [Bob,Remmy]
    giant    = list2OnePlacePred [Tom]
    wizard   = list2OnePlacePred [Willie,Victor]
    sword    = list2OnePlacePred [Fred]
    dagger   = list2OnePlacePred [Xena]
    thing    = list2OnePlacePred [Spoon1, Spoon2, Fork1, Fork2, Dress1, Dress2, Shoe1, Shoe2, Shoe3, Shoe4]
    rusty    = list2OnePlacePred [Fork1, Spoon1]

    child  = \ x -> (girl x || boy x)
    person = \ x -> (child x || princess x || dwarf x || giant x || wizard x)
    man    = \ x -> (dwarf x || giant x || wizard x)
    woman  = \ x -> princess x
    male   = \ x -> (man x || boy x)
    female = \ x -> (woman x || girl x)
    neutral = \ x -> True

    laugh   = list2OnePlacePred [Alice,Goldilocks,Ellie]
    cheer   = list2OnePlacePred [LittleMook,Dorothy]
    shudder = list2OnePlacePred [SnowWhite]
    smile   = list2OnePlacePred [Alice,Bob,Cyrus,Dorothy,Ellie,Fred,Goldilocks,LittleMook]

    love   = curry2 (`elem` [(Atreyu,Ellie),(Bob,SnowWhite),(Remmy,SnowWhite),(SnowWhite,LittleMook)])
    admire = curry2 (`elem` [(x,Goldilocks) | x <- entities, person x])
    help   = curry2 (`elem` [(Willie,Willie),(Victor,Victor),(SnowWhite,Bob),(Dorothy,LittleMook)])
    defeat = curry2 (`elem` [(x,y) | x <- entities, y <- entities, dwarf x && giant y] ++ [(Alice,Willie),(Alice,Victor)])

    give = curry3 (`elem` [(Tom,SnowWhite,Xena),(Alice,Ellie,SnowWhite)])


instance AtomicEntity Singular

{-
instance Entity Plural where



instance Entity Mass where
    
-}
