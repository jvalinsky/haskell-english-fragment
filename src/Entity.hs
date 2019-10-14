{-# LANGUAGE TypeFamilies, AllowAmbiguousTypes, TypeOperators, RankNTypes, ConstrainedClassMethods, FlexibleContexts #-}

module Entity where

import Data.List (intersect)

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
