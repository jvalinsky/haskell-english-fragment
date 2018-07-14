module Mass (MassEntity(..), materalize) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Model
import Semilattice
import Plural (PluralEntity(..), PluralJoin(..), isum, list2PEnt)
import Control.Monad

tup3 :: [s] -> (s, s, s)
tup3 [x, y, z] = (x, y, z)

type Index = Int
type Location = (Index, Int, Int, Int)
-- MassEntity can be the mass corresponding to an individual or 
-- a lump of matter with an index and an amount in kilograms
data MassT = Water | Metal | Gold | Dirt | Carbon deriving (Eq, Enum, Show)
data MassEntity = Mass [MassT] [Location] Float | Nothing | Everything

-- Homomorphism between individuals and mass terms
-- i.e. turn an individual into the mass associated with it
materalize :: PluralEntity -> MassEntity 
materalize (Atom x) = Mass (massT x) [ (tup3 0:(pos x))] (massOf x)
materalize (Plural x) = Mass (map massT x) (map (tup3) (zip [0..] x)) (foldr (+) 0 $ map $ massOf x)


massList =  [ ( 73.5 ,Alice ), ( 92.6, Bob ), ( 81.3, Cyrus ), (65.7, Dorothy ), ( 68.2, Ellie ), ( 76.5, Fred ), 
              (43.2, Goldilocks ), (67.1, Hillary ), (73.5,Irene ), (77.78,Jim ), (68.34, Kim ), (64.78, Linda ), 
              (40.12, LittleMook ), (73.4,Noah ), (83.7,Ollie ), (,Penny ), (72.45,Quine ), (54.67,Remmy ), 
              (62.8,SnowWhite ), (74.0,Tom ), (71.4,Uli ), (75.22,Victor ), (76.51,Willie ), (66.43,Xena ), (80.13,Atreyu ), (65.41,Zorba)]

massTList =  [ ( Carbon,Alice ), (Carbon, Bob ), (Carbon, Cyrus ), (Carbon, Dorothy ), (Carbon, Ellie ), (Carbon, Fred ), 
              (Carbon, Goldilocks ), (Carbon, Hillary ), (Carbon,Irene ), (Carbon,Jim ), (Carbon, Kim ), (Carbon, Linda ), 
              (Carbon, LittleMook ), (Carbon,Noah ), (Carbon,Ollie ), (Carbon,Penny ), (Carbon,Quine ), (Carbon,Remmy ), 
              (Carbon,SnowWhite ), (Carbon,Tom ), (Carbon,Uli ), (Carbon,Victor ), (Carbon,Willie ), (Carbon,Xena ), (Carbon,Atreyu ), (Carbon,Zorba)]


-- Grid of 100x100 with bottom front left corner as (0,0,0)
massLocList = [ ((0,25,25,0) ,Alice ), ((0,35,45,0), Bob ), ((0,75,55,0), Cyrus ), ((0,100,100,0), Dorothy ), ((0,80,85,0), Ellie ), ((0,20,75,0), Fred ), 
              ((0,60,75,0), Goldilocks ), ((0,46,50,0), Hillary ), ((0,30,65,0),Irene ), ((0,60,75,0), Jim ), ((0,10,15,0), Kim ), ((0,0,100,0), Linda ), 
              ((0,60,55,50), LittleMook ), ((0,60,35,30),Noah ), ((0,30,25,81),Ollie ), ((0,40,85,80),Penny ), ((0,10,45,90),Quine ), ((0,57,17,0),Remmy ), 
              ((0,33,55,20),SnowWhite ), ((0,23,45,31),Tom ), ((0,80,35,0),Uli ), ((0,44,75,8),Victor ), 
              ((0,63,45,0),Willie ), ((0,40,85,80),Xena ), ((0,72,35,39),Atreyu ), ((0,32,65,30),Zorba)]

entityMasses :: Map Entity Float
entityMasses = Map.fromList massList

massOf' :: Entity -> Float
masOff' x = Map.lookup x entityMasses 

massOf :: PluralEntity -> Float
massOf (Atom x) = massOf' x
massOf (Plural x) = foldr (+) 0 (map massOf' x)

{--
fusion :: MassEntity -> MassEntity -> MassEntity
fusion (MatterOf x) (MatterOf y) = MatterOf (x \/ y)
fusion (Mass t1 r1 x) (Mass t2 r2 y) | (t1 == t2) && (r1 /= r2) = 

instance Join MassEntity where
    (\/) = fusion
    (<=-) = undefined
    (==-) = undefined


--}
