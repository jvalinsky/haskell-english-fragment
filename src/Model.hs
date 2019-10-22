module Model where

import Data.List
import Semilattice

-- This constrains type variable a to be an instance of Entity
type OnePlacePred   = Entity -> Bool
type TwoPlacePred   = Entity -> Entity -> Bool
type ThreePlacePred = Entity -> Entity -> Entity -> Bool

-- Entity Types
data Atom = Sword1      | Sword2   | Alice'   | Bob'    | Cyrus'     | Ellie'       | 
            Goldilocks' | Hillary' | Irene'   | Jim'    | Kim'       | Linda'       | 
            Noah'       | Ollie'   | Penny'   | Quine'  | Dagger1    | Stuart'      |
            SnowWhite'  | Tom'     | Uli'     | Victor' | Willie'    | Xena'        |
            Zorba'      | Cup1     | Cup2     | Bottle1 | Bottle2    | The_Genesee' |
            Dress1      | Dress2   | Raft1    | Raft2   | Raft3      | Raft4        |
            Dorothy'    | Fred'    | Glasses1 | Ring1   | Whiskers'  | Mittens'     | 
            Gerald'     | Minnie'  | Mickey'  | Sue'    | The_Rhine' | Dis'         | 
            Thorin'     | Ring2    | Ring3 deriving (Eq, Show, Bounded, Enum)

atoms :: [Atom]
atoms = [minBound..maxBound]

data MType = Water' | Wood' | Air'  | Wine'   | Fabric' |
             Metal' | Rust' | Gold' | Advice' | Ice'    | Everything deriving (Eq, Show)

data Entity = Pl' [[Atom]] | Ms' [[Atom]] deriving (Eq, Show)

-- Lists for OnePlacePreds
thingList :: [Atom]
thingList = [Sword1, Sword2, Dagger1, Cup1, Cup2, Bottle1, Bottle2, The_Genesee', 
              Dress1, Dress2, Raft1, Raft2, Raft3, Raft4, Glasses1, Ring1, Ring2, Ring3]

glassesList :: [Atom]
glassesList = [Glasses1]

animalList :: [Atom]
animalList = [Stuart', Minnie', Mickey', Whiskers', Mittens', Gerald']

giantList :: [Atom]
giantList = [Tom', Bob']

dwarfList :: [Atom]
dwarfList = [Dis', Thorin']

beingList :: [Atom]
beingList = animalList ++ personList

catList :: [Atom]
catList = [Whiskers', Mittens']

mouseList :: [Atom]
mouseList = [Mickey', Minnie']

birdList :: [Atom]
birdList = [Gerald']

personList :: [Atom]
personList = atoms \\ (thingList ++ animalList)

weaponList :: [Atom]
weaponList = [Sword1, Sword2, Dagger1]

cupList :: [Atom]
cupList = [Cup1, Cup2]

bottleList :: [Atom]
bottleList = [Bottle1, Bottle2]

containerList :: [Atom]
containerList = bottleList ++ cupList

raftList :: [Atom]
raftList = [Raft1, Raft2, Raft3, Raft4]

dressList :: [Atom]
dressList = [Dress1, Dress2]

riverList :: [Atom]
riverList = [The_Genesee', The_Rhine']

magicalList :: [Atom]
magicalList = [Penny', Alice', Jim', Linda', Ellie', Victor', Kim', Dis', 
               Thorin', Tom', Cup1, Sword1, Ring1]

wizardList :: [Atom]
wizardList = personList `intersect` magicalList

witchList :: [Atom]
witchList = personList `intersect` magicalList

enchantedList :: [Atom]
enchantedList = magicalList `intersect` thingList

maleList :: [Atom]
maleList = [Bob', Cyrus', Jim', Noah', Ollie', Penny', Quine', Stuart', Tom', Uli', Victor',
            Willie', Zorba', Fred', Whiskers', Gerald', Mickey', Thorin']

femaleList :: [Atom]
femaleList = [Alice', Ellie', Goldilocks', Hillary', Irene', Kim', Linda', SnowWhite', Xena',
              Dorothy', Mittens', Minnie', Sue', Dis']

youngList :: [Atom]
youngList = [Ollie', Penny', Stuart', Uli', Willie', Noah', Tom',
             Alice', Ellie', Goldilocks', SnowWhite', Dorothy', Mittens']

oldList :: [Atom]
oldList = [Sword1, Bottle1, Ring3, Dress2] ++ (personList \\ youngList) ++ birdList

newList :: [Atom]
newList = thingList \\ oldList

boyList :: [Atom]
boyList = youngList `intersect` maleList

girlList :: [Atom]
girlList = youngList `intersect` femaleList

manList :: [Atom]
manList = oldList `intersect` maleList

womanList :: [Atom]
womanList = oldList `intersect` femaleList

metalList :: [Atom]
metalList = [Sword1, Sword2, Dagger1, Ring1, Ring2, Ring3]

steelList :: [Atom]
steelList = [Sword1, Sword2, Dagger1]

goldList :: [Atom]
goldList = [Ring1, Ring2]

silverList :: [Atom]
silverList = [Ring3]

ringList :: [Atom]
ringList = [Ring1, Ring2, Ring3]

dressList :: [Atom]
dressList = [Dress1, Dress2]

shiny :: [Atom]
shiny = metalList `intersect` newList

rustyList :: [Atom]
rustyList = metalList `intersect` oldList

glassList :: [Atom]
glassList = bottleList ++ glassesList

ceramicList :: [Atom]
ceramicList = cupList

wiseList :: [Atom]
wiseList = [Linda', Irene', Minnie', Xena', Jim', Victor', Fred']

foolishList :: [Atom]
foolishList = [Alice', Goldilocks', Ollie', Tom', Zorba', Mickey', SnowWhite']

laughList :: [Atom]
laughList = [Zorba', Alice', Dorothy', Gerald', Tom', Fred', Thorin']

runList :: [Atom]
runList = [Xena', Thorin', Jim', Alice'] ++ catList ++ mouseList

walkList :: [Atom]
walkList = [Linda', Fred', Jim', Penny', Ollie', Tom', Bob', Kim', SnowWhite']

smileList :: [Atom]
smileList = [Tom', Ollie', Alice', Kim', Uli']

swimList :: [Atom]
swimList = [Gerald', Whiskers', Mickey', Minnie', Thorin', Ollie', Jim']


-- Lists for TwoPlacePreds




{-
data ADJ = Wise  | Foolish | Bad      | Good       | Young | Old   | Present |
           Heavy | Rusty   | Clean    | Dirty      | Wet   | Dry   | Dull    |
           Warm  | Cold    | Magical  | Tall       | Short | Long  | Sharp   | 
           Sweet | Shiney  | Numerous | Widespread  deriving (Show, Eq)

data VP = VP0 INF | VP1 TV DetP | VP3 DV DetP DetP deriving Show

data INF = Laugh    | Scatter  | Smile | Run  | Walk   | Swim deriving (Show, Eq)
data TV  = Surround | Build    | Love  | Help | Defeat | Chase | Drink | Be | Have deriving (Show, Eq)
data DV  = Give deriving (Show, Eq)
-}


{-
extension :: OnePlacePred -> [Entity]

star :: OnePlacePred -> [Entity]

mu :: [OnePlacePred] -> Entity

fusion :: Entity -> Entity -> Entity

isum :: Entity -> Entity -> Entity 

distributive :: OnePlacePred -> OnePlacePred

sigma :: [OnePlacePred] -> OnePlacePred

sigmap :: [OnePlacePred] -> OnePlacePred

ipart :: Entity -> Entity -> Bool

mpart :: Entity -> Entity -> Bool

-- Materially Equivalent
(~) :: Entity -> Entity -> Bool
x ~ y = (x `mpart` y) && (y `mpart` x)


atom :: Entity -> Bool
atom (Ms' x) = True
atom (Pl' (xs:xss)) = xss == [] && length xs == 1

* individual sums of members of extensions of P
m mass term correspondent for P
c proper plural predicate of P
sigma sum of predicates
sigma* proper sum of predicates

Pi individual part (i-part) relation
m  material   part (m-part) relation

mu material fusion of P's
At x := x is an atom
Distr(P) := P is distributive

||P|| := extension of P, generated by join-semilattice

i-sum 
fusion 
-}