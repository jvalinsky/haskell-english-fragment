module Mass (MassT, MassEntity, makeMass, materalize) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Tuple (swap)
import Control.Monad (liftM, liftM2, liftM3, mapM)
import Model
import Semilattice
import Plural (PluralEntity(..), PluralJoin(..), isum, list2PEnt)
import Control.Monad

type Location = (Int, Int, Int)
-- MassEntity can be the mass corresponding to an individual or 
-- a lump of matter with an index and an amount in kilograms
data MassT = Water | Metal | Gold | Dirt | Carbon deriving (Eq, Ord, Enum, Show)
data MassEntity = Mass [MassT] [Location] [Float] | Everything deriving Show


-- Basically checks if lists are same length so value is sane.
-- If we used dependent typing here, there would be a way of
-- ensuring the lengths of all the arbitrarily long Lists were the same
makeMass :: ([MassT], [Location], [Float]) -> Maybe MassEntity
makeMass (x, y, z) = Just (Mass x y z)

-- helper functions
seqtup3 :: (Maybe a, Maybe b, Maybe c) -> Maybe (a, b, c)
seqtup3 (Nothing , Nothing , Nothing) = Nothing
seqtup3 (Nothing, y, z) = Nothing
seqtup3 (x, Nothing, z) = Nothing
seqtup3 (x, y, Nothing) = Nothing
seqtup3 (Just x, Just y, Just z) = Just (x, y, z)

-- dealing with Maybes
tup2Mass :: ([Maybe MassT], [Maybe Location], Maybe Float) -> Maybe MassEntity
tup2Mass (mTypes, locs, mass) = liftM (\(x,y,z) -> (Mass x y z)) (seqtup3 (sequence mTypes, sequence locs, sequence mass))

simplify :: MassEntity -> MassEntity
simplify (Mass t l m) = foldr combine [] 
    where props = zip3 t l m
          combine (x1, y1, z1) (x1, y1, z2) = [(x1, y1, z1 + z2)]
          combine (x1, y1, z1) (x1, y2, z2) = [(x1, y1, z1 + z2)]
          combine x y = [x, y]

-- Homomorphism between individuals and mass terms
-- i.e. turn an individual into the mass associated with it
materalize :: PluralEntity -> Maybe MassEntity 
materalize x = tup2Mass ((massT x), (massLoc x), (massOf x))

-- turn mass entity into individual (PluralEntity)
individualize :: MassEntity -> Maybe PluralEntity
individualize (Mass t l m) =  
    where types = sequence (map entsWithMassType t) 
          locs  = sequence (map entsWithExactLoc l)

entMassList :: [(Entity, Float)]
entMassList = [(Alice,73.5),(Bob,92.6),(Cyrus,81.3),(Dorothy,65.7),(Ellie,68.2),(Fred,76.5),
    (Goldilocks,43.2),(Hillary,67.1),(Irene,73.5),(Jim,77.78),(Kim,68.34),(Linda,64.78),
    (LittleMook,40.12),(Noah,73.5),(Ollie,83.7),(Penny,45.4),(Quine,72.45),(Remmy,54.67),
    (SnowWhite,62.8),(Tom,74.0),(Uli,71.4),(Victor,75.22),(Willie,76.51),(Xena,66.43),(Atreyu,80.13),(Zorba,65.41)]

entMTList :: [(Entity, MassT)]
entMTList = [(Alice,Carbon),(Bob,Carbon),(Cyrus,Carbon),(Dorothy,Carbon),(Ellie,Carbon),(Fred,Carbon),
    (Goldilocks,Carbon),(Hillary,Carbon),(Irene,Carbon),(Jim,Carbon),(Kim,Carbon),(Linda,Carbon),
    (LittleMook,Carbon),(Noah,Carbon),(Ollie,Carbon),(Penny,Carbon),(Quine,Carbon),(Remmy,Carbon),
    (SnowWhite,Carbon),(Tom,Carbon),(Uli,Carbon),(Victor,Carbon),(Willie,Carbon),(Xena,Carbon),
    (Atreyu,Carbon),(Zorba,Carbon)]

-- Cube of 100x100x100 with bottom front left corner as (0,0,0)
entLocList :: [(Entity, (Int, Int, Int))]
entLocList = [(Alice,(25,25,0)),(Bob,(35,45,0)),(Cyrus,(75,55,0)),(Dorothy,(100,100,0)),
    (Ellie,(80,85,0)),(Fred,(20,75,0)),(Goldilocks,(60,75,0)),(Hillary,(46,50,0)),(Irene,(30,65,0)),
    (Jim,(60,75,0)),(Kim,(10,15,0)),(Linda,(0,100,0)),(LittleMook,(60,55,50)),(Noah,(60,35,30)),
    (Ollie,(30,25,81)),(Penny,(40,85,80)),(Quine,(10,45,90)),(Remmy,(57,17,0)),(SnowWhite,(33,55,20)),
    (Tom,(23,45,31)),(Uli,(80,35,0)),(Victor,(44,75,8)),(Willie,(63,45,0)),(Xena,(40,85,80)),
    (Atreyu,(72,35,39)),(Zorba,(32,65,30))]

massEntsList = map ((\(x,y) -> (x, [y])) . swap)  entMassList
massTEntsList = map ((\(x,y) -> (x, [y])) . swap) entMTList
locEntsList =  map ((\(x,y) -> (x, [y])) . swap)  entLocList

-- Hashtables to look up properties of an Entity
entityMasses :: Map Entity Float
entityMasses = Map.fromList entMassList

entityMT :: Map Entity MassT
entityMT = Map.fromList entMTList

entityLoc :: Map Entity Location
entityLoc = Map.fromList entLocList

-- Hashtables to look up Entities with a specific property.
-- Values are a List of Entities.

massesOfEnts :: Map Float [Entity]
massesOfEnts = Map.fromListWith (++) massEntsList

massTypesOfEnts :: Map MassT [Entity]
massTypesOfEnts = Map.fromListWith (++) massTEntsList

locsOfEnts :: Map Location [Entity]
locsOfEnts = Map.fromListWith (++) locEntsList

-- Functions to look up properties for Entities and vice versa
massOf' :: Entity -> Maybe Float
massOf' x = Map.lookup x entityMasses 

massOf'' :: PluralEntity -> Maybe Float
massOf'' (Atom x) = (massOf' x)
massOf'' (Plural x) = foldr (liftM2 (+)) (Just 0) (map massOf' x)

massOf :: PluralEntity -> Maybe [Float]
massOf (Atom x) = sequence [massOf' x]
massOf (Plural x) = sequence (liftM (map massOf') x)


massT' :: Entity -> Maybe MassT
massT' x = Map.lookup x entityMT

massT :: PluralEntity -> [Maybe MassT]
massT (Atom x) = [massT' x]
massT (Plural x) = map massT' x 

massLoc' :: Entity -> Maybe Location
massLoc' x = Map.lookup x entityLoc

massLoc :: PluralEntity -> [Maybe Location]
massLoc (Atom x)   =  [massLoc' x]
massLoc (Plural x) = map massLoc' x 

entsWithExactMass :: Float -> Maybe [Entity]
entsWithExactMass mass = Map.lookup mass massesOfEnts 

entsWithMassType :: MassT -> Maybe [Entity]
entsWithMassType massType = Map.lookup massType massTypesOfEnts

entsWithExactLoc :: Location -> Maybe [Entity]
entsWithExactLoc loc = Map.lookup loc locsOfEnts

-- TODO:
-- Now the fun part, lookup entities within a range of masses 
-- or within a radius of a Location

-- Only thing to watch out for is not fusing the same entity to itself
-- (i.e. check Location)
fusion :: MassEntity -> MassEntity -> MassEntity
fusion (Mass t1 l1 m1) (Mass t2 l2 m2) = Mass (t1 ++ t2) (l1 ++ l2) (m1 + m2)

-- If all the fields are equal then the mass entities are equal,
-- the only thing to be careful of is that List is an ordered list
-- even though Set or some kind of unordered list may have rendered
-- defining mequal unnecessary
mequal = undefined

{-- 
instance Join MassEntity where
    (\/) = fusion
    (==-) = mequal
    (<=-) x y = if (x \/ y) ==- y then True else False

-- If one individal is i-part of another, then that individal is also m-part 
-- of the other individal.
-- If two MassEntities have the same location and type but different masses
-- that means one is m-part of the other 
-- (ex. 1/2 of the gold matter making up the gold ring)
class (Join s) => MassJoin s where
    mpart :: MassEntity -> MassEntity -> Bool

instance MassJoin MassEntity where
    -- if they don't share the same types then False.
    -- if they share the same types, locations, and the first
    -- has less mass than the second then True.
    -- Check if the corresponding individual i-parts the other,
    -- then True.
    -- Otherwise False.
    mpart (Mass t1 l1 m1) (Mass t2 l2 m2) =
    mpart x y = ipart . individualize $ x y
    mpart _ _ = False
--}
