{-# LANGUAGE TypeFamilies, AllowAmbiguousTypes, TypeOperators, RankNTypes, ConstrainedClassMethods, FlexibleContexts #-}

module Model where

import Data.List

-- Helper Functions
curry2 :: ((a,b) -> c) -> b -> a -> c
curry2 f y x = f (x,y)

curry3 :: ((a,b,c) -> d) -> c -> b -> a -> d
curry3 f z y x = f (x,y,z)

self ::  (a -> a -> b) -> a -> b
self p = \ x -> p x x

pluralize1 :: (Singular -> Bool) -> (Plural -> Bool)
pluralize1 p = helper p
    where helper :: (Singular -> Bool) -> Plural -> Bool
          helper p (Collective xs)   = foldr (&&) True (map p xs)
          helper p (Distributive ys) = foldr (&&) True (map p ys)

{- 
pluralize2 :: (Singular -> Singular -> Bool) -> (Plural -> Plural -> Bool)
pluralize2 p = helper p
              where helper :: (Singular -> Bool) -> Plural -> Bool
                    helper p (Collective xs)   = foldr (&&) True (map p xs)
                    helper p (Distributive ys) = foldr (&&) True (map p ys)
-}

-- Entity Types
data Singular = Knife1     | Knife2  | Alice    | Bob     | Cyrus  | Ellie | 
                Goldilocks | Hillary | Irene    | Jim     | Kim    | Linda | 
                LittleMook | Noah    | Ollie    | Penny   | Quine  | Remmy | 
                SnowWhite  | Tom     | Uli      | Victor  | Willie | Xena  | 
                Spoon1     | Spoon2  | Zorba    | Atreyu  | Fork1  | Fork2 |
                Dress1     | Dress2  | Shoe1    | Shoe2   | Shoe3  | Shoe4 |
                Dorothy    | Fred    | Glasses1 | Jeans1 deriving (Eq,Show, Read, Bounded, Enum)

data Plural = Collective [Singular] | Distributive [Singular] deriving (Eq,Show, Read)

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

    false' :: OnePlacePred entity -> Bool
    false' p = False 

    -- Predicates
    false1 :: OnePlacePred entity
    false1 e = False

    false2 :: TwoPlacePred entity
    false2 e1 e2 = False

    false3 :: ThreePlacePred entity
    false3 e1 e2 e3 = False

    girl, boy, princess, dwarf, giant, wizard, sword, dagger, rusty, child, person, man, woman, 
        male, female, thing, neutral, laugh, cheer, shudder, smile, wise, foolish, bad, good, rich, poor,
        mellow, discordant, young, old, heavy, light, dark, clean, dirty, wet, dry, cold, hot,
        magical, tall, short, long, sharp, dull, shiney :: OnePlacePred entity
    love, admire, help, defeat ::  TwoPlacePred entity
    give :: ThreePlacePred entity

    -- default to returning false 
    -- (assuming predicate is nonsensical for arbitrary Entity unless otherwise specified)
    girl = false1
    boy  = false1
    princess = false1
    dwarf = false1
    giant = false1
    wizard = false1
    sword = false1
    dagger = false1
    rusty = false1
    child = false1
    person = false1
    man = false1
    woman = false1
    male = false1
    female = false1
    thing = false1
    neutral = false1
    laugh = false1
    cheer = false1
    shudder = false1
    smile = false1
    wise = false1
    foolish = false1
    bad = false1
    good = false1
    rich = false1
    poor = false1
    heavy = false1
    mellow = false1
    discordant = false1
    young = false1
    old = false1
    light = false1
    dark = false1
    clean = false1
    dirty = false1
    wet = false1
    dry = false1
    hot = false1
    cold = false1
    shiney = false1
    magical = false1
    tall = false1
    short = false1
    long = false1
    sharp = false1
    dull = false1

    love = false2
    admire = false2
    help = false2 
    defeat = false2

    give = false3

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

instance Entity Plural where
    girl = pluralize1 girl
    boy  = pluralize1 boy
    princess = pluralize1 princess
    dwarf = pluralize1 dwarf
    giant = pluralize1 giant
    wizard = pluralize1 wizard
    sword = pluralize1 sword
    dagger = pluralize1 dagger
    rusty = pluralize1 rusty
    child = pluralize1 child
    person = pluralize1 person
    man = pluralize1 man 
    woman = pluralize1 woman
    male = pluralize1 male
    female = pluralize1 female
    thing = pluralize1 thing
    neutral = pluralize1 neutral
    laugh = pluralize1 laugh
    cheer = pluralize1 cheer
    shudder = pluralize1 shudder
    smile = pluralize1 smile
    wise = pluralize1 wise
    foolish = pluralize1 foolish
    bad = pluralize1 bad
    good = pluralize1 good
    rich = pluralize1 rich
    poor = pluralize1 poor
    heavy = pluralize1 heavy
    mellow = pluralize1 mellow
    discordant = pluralize1 discordant
    young = pluralize1 young
    old = pluralize1 old
    light = pluralize1 light
    dark = pluralize1 dark
    clean = pluralize1 clean
    dirty = pluralize1 dirty
    wet = pluralize1 wet
    dry = pluralize1 dry
    hot = pluralize1 hot 
    cold = pluralize1 cold
    shiney = pluralize1 shiney
    magical = pluralize1 magical
    tall = pluralize1 tall 
    short = pluralize1 short 
    long = pluralize1 long 
    sharp = pluralize1 sharp
    dull = pluralize1 dull
    

instance Entity Mass

{-
class (Entity entity) => DistributiveEntity entity where
    ??


instance Entity Plural where
    ??


instance Entity Mass where
    ??
-}
