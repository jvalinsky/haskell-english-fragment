module Mass (MassT, MassEntity, materalize) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Monad (liftM, liftM2, liftM3, mapM)
import Model
import Semilattice
import Plural (PluralEntity(..), PluralJoin(..), isum, list2PEnt)
import Control.Monad

tup4 :: [s] -> (s, s, s, s)
tup4 [i, x, y, z] = (i, x, y, z)

type Index = Int
type Location = (Index, Int, Int, Int)
-- MassEntity can be the mass corresponding to an individual or 
-- a lump of matter with an index and an amount in kilograms
data MassT = Water | Metal | Gold | Dirt | Carbon deriving (Eq, Enum, Show)
data MassEntity = Mass [MassT] [Location] Float | Everything deriving Show

seqtup3 :: (Maybe a, Maybe b, Maybe c) -> Maybe (a, b, c)
seqtup3 (Nothing , Nothing , Nothing) = Nothing
seqtup3 (Nothing, y, z) = Nothing
seqtup3 (x, Nothing, z) = Nothing
seqtup3 (x, y, Nothing) = Nothing
seqtup3 (Just x, Just y, Just z) = Just (x, y, z)

list2Mass :: ([Maybe MassT], [Maybe Location], Maybe Float) -> Maybe MassEntity
list2Mass (mTypes, locs, mass) = liftM (\(x,y,z) -> (Mass x y z)) (seqtup3 (sequence mTypes, sequence locs, mass))

-- Homomorphism between individuals and mass terms
-- i.e. turn an individual into the mass associated with it
materalize :: PluralEntity -> Maybe MassEntity 
materalize x = list2Mass ((massT x), (massLoc x), (massOf x))
--materalize (Atom x) = list2Mass ((massT' x), (massLoc' x), (massOf' x))
--materalize (Plural x) = list2Mass (map massT x) (map tup4 (map massLoc x)) (foldr (+) 0 $ map $ massOf x)

massList :: [(Entity, Float)]
massList = [(Alice,73.5),(Bob,92.6),(Cyrus,81.3),(Dorothy,65.7),(Ellie,68.2),(Fred,76.5),
    (Goldilocks,43.2),(Hillary,67.1),(Irene,73.5),(Jim,77.78),(Kim,68.34),(Linda,64.78),
    (LittleMook,40.12),(Noah,73.4),(Ollie,83.7),(Penny,45.4),(Quine,72.45),(Remmy,54.67),
    (SnowWhite,62.8),(Tom,74.0),(Uli,71.4),(Victor,75.22),(Willie,76.51),(Xena,66.43),(Atreyu,80.13),(Zorba,65.41)]

massTList :: [(Entity, MassT)]
massTList = [(Alice,Carbon),(Bob,Carbon),(Cyrus,Carbon),(Dorothy,Carbon),(Ellie,Carbon),(Fred,Carbon),
    (Goldilocks,Carbon),(Hillary,Carbon),(Irene,Carbon),(Jim,Carbon),(Kim,Carbon),(Linda,Carbon),
    (LittleMook,Carbon),(Noah,Carbon),(Ollie,Carbon),(Penny,Carbon),(Quine,Carbon),(Remmy,Carbon),
    (SnowWhite,Carbon),(Tom,Carbon),(Uli,Carbon),(Victor,Carbon),(Willie,Carbon),(Xena,Carbon),
    (Atreyu,Carbon),(Zorba,Carbon)]

-- Grid of 100x100 with bottom front left corner as (0,0,0)
massLocList :: [(Entity, (Index, Int, Int, Int))]
massLocList = [(Alice,(0,25,25,0)),(Bob,(0,35,45,0)),(Cyrus,(0,75,55,0)),(Dorothy,(0,100,100,0)),
    (Ellie,(0,80,85,0)),(Fred,(0,20,75,0)),(Goldilocks,(0,60,75,0)),(Hillary,(0,46,50,0)),(Irene,(0,30,65,0)),
    (Jim,(0,60,75,0)),(Kim,(0,10,15,0)),(Linda,(0,0,100,0)),(LittleMook,(0,60,55,50)),(Noah,(0,60,35,30)),
    (Ollie,(0,30,25,81)),(Penny,(0,40,85,80)),(Quine,(0,10,45,90)),(Remmy,(0,57,17,0)),(SnowWhite,(0,33,55,20)),
    (Tom,(0,23,45,31)),(Uli,(0,80,35,0)),(Victor,(0,44,75,8)),(Willie,(0,63,45,0)),(Xena,(0,40,85,80)),
    (Atreyu,(0,72,35,39)),(Zorba,(0,32,65,30))]

entityMasses :: Map Entity Float
entityMasses = Map.fromList massList

entityMT :: Map Entity MassT
entityMT = Map.fromList massTList

entityLoc :: Map Entity Location
entityLoc = Map.fromList massLocList

massOf' :: Entity -> Maybe Float
massOf' x = Map.lookup x entityMasses 

massOf :: PluralEntity -> Maybe Float
massOf (Atom x) = (massOf' x)
massOf (Plural x) = foldr (liftM2 (+)) (Just 0) (map massOf' x)

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


{--

fusion :: MassEntity -> MassEntity -> MassEntity
fusion (MatterOf x) (MatterOf y) = MatterOf (x \/ y)
fusion (Mass t1 r1 x) (Mass t2 r2 y) | (t1 == t2) && (r1 /= r2) = 

instance Join MassEntity where
    (\/) = fusion
    (<=-) = undefined
    (==-) = undefined


--}
