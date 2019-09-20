module Model where

import Data.List

data Entity = Alice | Bob | Cyrus | Dorothy | Ellie | Fred | Goldilocks | Hillary | Irene | Jim | Kim | Linda | LittleMook | Noah | Ollie | Penny | Quine | Remmy | SnowWhite | Tom | Uli | Victor | Willie | Xena | Atreyu | Zorba
     deriving (Eq,Show,Bounded,Enum)
entities :: [Entity]
entities = [minBound..maxBound]

type OnePlacePred = Entity -> Bool
list2OnePlacePred :: [Entity] -> OnePlacePred
list2OnePlacePred xs = \ x -> elem x xs

girl, boy, princess, dwarf, giant, wizard, sword, dagger :: OnePlacePred
girl     = list2OnePlacePred [SnowWhite,Alice,Dorothy,Goldilocks]
boy      = list2OnePlacePred [LittleMook,Atreyu]
princess = list2OnePlacePred [Ellie]
dwarf    = list2OnePlacePred [Bob,Remmy]
giant    = list2OnePlacePred [Tom]
wizard   = list2OnePlacePred [Willie,Victor]
sword    = list2OnePlacePred [Fred]
dagger   = list2OnePlacePred [Xena]

child, person, man, woman, male, female, thing :: OnePlacePred
child  = \ x -> (girl x || boy x)
person = \ x -> (child x || princess x || dwarf x || giant x || wizard x)
man    = \ x -> (dwarf x || giant x || wizard x)
woman  = \ x -> princess x
male   = \ x -> (man x || boy x)
female = \ x -> (woman x || girl x)
thing  = \ x -> not (person x)

laugh, cheer, shudder, smile :: OnePlacePred
laugh   = list2OnePlacePred [Alice,Goldilocks,Ellie]
cheer   = list2OnePlacePred [LittleMook,Dorothy]
shudder = list2OnePlacePred [SnowWhite]
smile   = list2OnePlacePred [Alice,Bob,Cyrus,Dorothy,Ellie,Fred,Goldilocks,LittleMook]

curry2 :: ((a,b) -> c) -> b -> a -> c
curry2 f y x = f (x,y)

type TwoPlacePred = Entity -> Entity -> Bool
love, admire, help, defeat :: TwoPlacePred
love   = curry2 (`elem` [(Atreyu,Ellie),(Bob,SnowWhite),(Remmy,SnowWhite),(SnowWhite,LittleMook)])
admire = curry2 (`elem` [(x,Goldilocks) | x <- entities, person x])
help   = curry2 (`elem` [(Willie,Willie),(Victor,Victor),(SnowWhite,Bob),(Dorothy,LittleMook)])
defeat = curry2 (`elem` [(x,y) | x <- entities, y <- entities, dwarf x && giant y] ++ [(Alice,Willie),(Alice,Victor)])

curry3 :: ((a,b,c) -> d) -> c -> b -> a -> d
curry3 f z y x = f (x,y,z)

type ThreePlacePred = Entity -> Entity -> Entity -> Bool
give :: ThreePlacePred
give = curry3 (`elem` [(Tom,SnowWhite,Xena),(Alice,Ellie,SnowWhite)])

passivize :: TwoPlacePred -> OnePlacePred
passivize r = \ x -> any (r x) entities

self ::  (a -> a -> b) -> a -> b
self p = \ x -> p x x
