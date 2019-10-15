{-# LANGUAGE TypeFamilies, AllowAmbiguousTypes, TypeOperators, RankNTypes, ConstrainedClassMethods, FlexibleContexts #-}

module Model where

import Data.List
import Entity

-- Helper Functions
curry2 :: ((a,b) -> c) -> b -> a -> c
curry2 f y x = f (x,y)

curry3 :: ((a,b,c) -> d) -> c -> b -> a -> d
curry3 f z y x = f (x,y,z)

self ::  (a -> a -> b) -> a -> b
self p = \ x -> p x x

pluralize1 :: (Singular -> Bool) -> (Plural -> Bool)
pluralize1 p = helper p
    where helper :: OnePlacePred Singular -> OnePlacePred Plural
          helper p (Collective xs)   = foldr (&&) True (map p xs)
          helper p (Distributive ys) =  foldr (&&) True (map p ys)
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
                Dorothy    | Fred    | Glasses1 | Jeans1  | Whiskers | Mittens |
                Stuart | Gerald | Minnie | Mickey | Sue | Donald | Oscar | 
                Ryan | Daffy deriving (Eq, Show, Read, Bounded, Enum)

data Plural = Collective [Singular] | Distributive [Singular] deriving (Eq, Show, Read)

data Mass =  Water | Blood | Earth | Knowledge | 
             Cloth | Metal | Air   | Cultery   | 
             MassOfS Singular | MassOfP Plural deriving (Eq,Show, Read)


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
    cat      = list2OnePlacePred [Whiskers, Mittens]
    mouse    = list2OnePlacePred [Mickey, Minnie, Stuart, Gerald, Sue]
    bird     = list2OnePlacePred[Donald, Daffy, Oscar]
    duck     = list2OnePlacePred[Donald, Daffy]
    goose    = list2OnePlacePred[Oscar, Ryan]
    can_fly  = list2OnePlacePred[Donald, Daffy, Oscar, Oscar]

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
