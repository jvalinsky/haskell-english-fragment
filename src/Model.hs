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
            Thorin'     | Ring2    | Ring3    deriving (Eq, Show, Bounded, Enum)

atoms :: [Atom]
atoms = [minBound..maxBound]

data MType = Water' | Wine' | Wood' | Advice' | Glass' | Metal' | Everything' | Nothing' deriving (Eq, Show)

data Mass = MassOf [MType] [Atom] deriving (Eq, Show)

data Plural = Pl [Atom] deriving (Eq, Show)

data Entity = Pl' Plural | Ms' Mass deriving (Eq, Show)

-- Lists for OnePlacePreds
thingList :: [Atom]
thingList = [Sword1, Sword2, Dagger1, Cup1, Cup2, Bottle1, Bottle2, The_Genesee', The_Rhine',
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
               Thorin', Tom', Cup1, Sword1, Ring1, Bottle1]

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
oldList = [Sword1, Raft1, Raft4, Bottle1, Ring3, Dress2, Glasses1] ++ (personList \\ youngList) ++ birdList ++ metalList 

oldMList :: [Mass]
oldMList = [(MassOf [Wood'] [Raft3]), (MassOf [Wood'] [Raft4]), (MassOf [Wood'] [Sword1])]

newList :: [Atom]
newList = (thingList \\ oldList) 

newMList :: [Mass]
newMList = [(MassOf [Wood'] [Raft1]), (MassOf [Wood'] [Raft2]), (MassOf [Wood'] [Sword2])]

boyList :: [Atom]
boyList = youngList `intersect` maleList

girlList :: [Atom]
girlList = youngList `intersect` femaleList

manList :: [Atom]
manList = oldList `intersect` maleList

womanList :: [Atom]
womanList = oldList `intersect` femaleList

childrenList :: [Atom]
childrenList = boyList `union` girlList

ringList :: [Atom]
ringList = [Ring1, Ring2, Ring3]

shiny :: [Atom]
shiny = metalList `intersect` newList

rustyList :: [Atom]
rustyList = [Sword1, Ring3]

wiseList :: [Atom]
wiseList = [Linda', Irene', Xena', Jim', Victor', Fred']

foolishList :: [Atom]
foolishList = [Alice', Goldilocks', Ollie', Tom', Zorba', SnowWhite']

laughList :: [Atom]
laughList = [Zorba', Alice', Dorothy', Gerald', Tom', Fred', Thorin']

runList :: [Atom]
runList = [Xena', Thorin', Jim', Alice', Dorothy', Ellie'] `union` 
           catList `union` 
           mouseList `union` 
           giantList `union` 
           birdList

walkList :: [Atom]
walkList = [Linda', Fred', Jim', Penny', Ollie', Tom', Bob', Kim', SnowWhite']

smileList :: [Atom]
smileList = [Tom', Ollie', Alice', Kim', Uli']

swimList :: [Atom]
swimList = [Gerald', Whiskers', Mickey', Minnie', Thorin', Ollie', Jim']

coldList :: [Atom]
coldList = [The_Genesee', The_Rhine', Bottle2, Ollie', Alice']

coldMList :: [Mass]
coldMList = [(MassOf [Water'] [Cup1]), (MassOf [Water'] [The_Genesee']), (MassOf [Water'] [The_Rhine'])]

-- Lists for TwoPlacePreds
chaseList :: [[Atom]]
chaseList = [ [Whiskers' ,  Mickey' ],
              [Whiskers' ,  Minnie' ],
              [Whiskers' ,  Sue'    ],
              [Mittens'  ,  Mickey' ],
              [Mittens'  ,  Minnie' ],
              [Mittens'  , Sue'     ],
              [Mittens'  , Gerald'  ],
              [Tom'      , Ollie'   ],
              [Bob'      , Ollie'   ], 
              [Tom'      , Penny'   ],
              [Bob'      , Penny'   ],
              [Tom'      , Uli'     ],
              [Bob'      , Uli'     ],
              [Ollie'    , Alice'   ], 
              [Ollie'    , Ellie'   ],
              [Ollie'    , Dorothy' ],
              [Dorothy'  , Ellie'   ],
              [SnowWhite', Dorothy' ] ]

drinkList :: [[Atom]]
drinkList = [ [Uli'     , Cup1         ],
              [Ollie'   , Bottle1      ],
              [Penny'   , Bottle1      ],
              [Dorothy' , Bottle1      ],
              [Alice'   , Bottle2      ],
              [Linda'   , Bottle2      ],
              [Ellie'   , Bottle2      ],
              [Victor'  , Bottle2      ],
              [Kim'     , Bottle2      ],
              [Jim'     , Bottle2      ],
              [Quine'   , Cup2         ],
              [Tom'     , The_Rhine'   ],
              [Bob'     , The_Rhine'   ],
              [Whiskers', The_Genesee' ], 
              [Mittens' , The_Genesee' ] ]

buildList :: [[Atom]]
buildList = [ [Ollie'  , Raft1],
              [Penny'  , Raft1],
              [Uli'    , Raft1],
              [Alice'  , Raft2],
              [Dorothy', Raft2],
              [Ellie'  , Raft2],
              [Linda'  , Raft3],
              [Hillary', Raft3],
              [Zorba'  , Raft3],
              [Xena'   , Raft4],
              [Thorin' , Raft4] ]

warriorList :: [Atom]
warriorList = [Xena', Thorin', Irene', Victor', Fred', Cyrus', Quine'] 

hasSwordList :: [[Atom]]
hasSwordList = [ [Thorin', Sword1], 
                 [Fred'  , Sword2] ]

hasDaggerList :: [[Atom]]
hasDaggerList = [ [Xena', Dagger1] ]

hasBottle :: [[Atom]]
hasBottle = [ [Ollie', Bottle2],
              [Linda', Bottle1] ]

hasCup :: [[Atom]]
hasCup = [ [Quine', Cup2],
              [Uli'  , Cup1] ]

hasRing :: [[Atom]]
hasRing = [ [Kim'   , Ring1],
            [Dis'   , Ring2],
            [Zorba' , Ring3] ]

hasGlasses :: [[Atom]]
hasGlasses = [ [Fred', Glasses1] ]


fightList :: [[Atom]]
fightList = [ [Xena'  , Irene'],
              [Thorin', Fred' ],
              [Victor', Tom'  ],
              [Victor', Bob'  ],
              [Fred'  , Tom'  ],
              [Fred'  , Bob'  ] ]

defeatList :: [[Atom]]
defeatList = [ [Xena'  , Irene' ],
               [Thorin', Fred'  ],
               [Tom'   , Fred'  ],
               [Bob'   , Victor'] ]

helpList :: [[Atom]]
helpList = [ [Victor', Fred'   ],
             [Ollie' , Penny'  ],
             [Ollie' , Uli'    ],
             [Alice' , Dorothy'],
             [Alice' , Ellie'  ],
             [Linda' , Hillary'],
             [Linda' , Zorba'  ],
             [Xena'  , Thorin' ] ]

scatterList :: [[Atom]]
scatterList = [mouseList, warriorList]

surroundList :: [[[Atom]]]
surroundList = [ [warriorList, giantList] ]

badList :: [Atom]
badList =  [Noah', Kim']

badMList :: [Mass]
badMList = [(MassOf [Advice'] [Ollie']), (MassOf [Advice'] [Zorba']), 
            (MassOf [Advice'] [SnowWhite']), (MassOf [Advice'] [Alice']), 
            (MassOf [Advice'] [Goldilocks']), (MassOf [Advice'] [Tom']),
            (MassOf [Water'] [The_Genesee']), (MassOf [Wine'] [Bottle1])]

goodList :: [Atom]
goodList = [Fred', Xena', Thorin']

goodMList :: [Mass]
goodMList = [ (MassOf [Advice'] [Linda']), (MassOf [Advice'] [Irene']), 
              (MassOf [Advice'] [Xena']), (MassOf [Advice'] [Jim']),
              (MassOf [Advice'] [Victor']), (MassOf [Advice'] [Fred']), 
              (MassOf [Wine'] [Bottle2]), (MassOf [Water'] [The_Rhine'])]

tornList :: [Atom]
tornList = [Dress2]

adviceList :: [Mass]
adviceList = [ (MassOf [Advice'] [Linda']), (MassOf [Advice'] [Irene']), 
               (MassOf [Advice'] [Xena']), (MassOf [Advice'] [Jim']),
               (MassOf [Advice'] [Victor']), (MassOf [Advice'] [Fred']),
               (MassOf [Advice'] [Ollie']), (MassOf [Advice'] [Zorba']), 
               (MassOf [Advice'] [SnowWhite']), (MassOf [Advice'] [Alice']), 
               (MassOf [Advice'] [Goldilocks']), (MassOf [Advice'] [Tom']) ]
    
waterMList :: [Mass]
waterMList = [ (MassOf [Water'] [The_Rhine']), (MassOf [Water'] [The_Genesee']),
               (MassOf [Water'] [Cup1]), (MassOf [Water'] [Cup2]) ]

widespreadMList :: [[Mass]]
widespreadMList = [adviceList, waterMList]

dirtyList :: [Atom]
dirtyList = [Ollie', Willie', Dress2, Ring3]

dirtyMList :: [Mass]
dirtyMList = [(MassOf [Water'] [The_Genesee'])]

cleanList :: [Atom]
cleanList = atoms \\ dirtyList

cleanMList :: [Mass]
cleanMList =  [(MassOf [Water'] [The_Rhine'])]

giveList :: [[Atom]]
giveList = [ [Irene' , Alice' , Dress1  ],
             [Kim'   , Ellie' , Dress2  ],
             [Fred'  , Xena'  , Dagger1 ],
             [Dis'   , Thorin', Sword1  ],
             [Victor', Fred'  , Sword2  ] ]

tallList :: [Atom]
tallList = giantList ++ [Xena', Fred', Victor', Ollie', Irene', Bottle2]

shortList :: [Atom]
shortList = dwarfList ++ [Uli', Alice', Goldilocks', Quine', Willie', Bottle1]

metalList :: [Atom]
metalList = [Dagger1, Sword1, Sword2, Glasses1] ++ ringList

metalMList :: [Mass]
metalMList = [ (MassOf [Metal'] [Dagger1]), (MassOf [Metal'] [Sword1]), 
              (MassOf [Metal'] [Sword2]), (MassOf [Metal'] [Glasses1]), 
              (MassOf [Metal'] [Ring1]), (MassOf [Metal'] [Ring2]), 
              (MassOf [Metal'] [Ring3]) ]

steelList :: [Atom]
steelList = [Sword1, Sword2, Dagger1]

goldList :: [Atom]
goldList = [Ring1, Ring2]

silverList :: [Atom]
silverList = [Ring3]

steelMList :: [Mass]
steelMList = [(MassOf [Metal'] [Dagger1]), (MassOf [Metal'] [Sword1]), 
              (MassOf [Metal'] [Sword2]), (MassOf [Metal'] [Glasses1]) ]

goldMList :: [Mass]
goldMList = [(MassOf [Metal'] [Ring1]), (MassOf [Metal'] [Ring2])]

silverMList :: [Mass]
silverMList = [(MassOf [Metal'] [Ring3])]

wineMList :: [Mass]
wineMList = [ (MassOf [Wine'] [Bottle1]),  (MassOf [Wine'] [Bottle2])]

-- Helper functions

list2OnePlacePred :: [Entity] -> OnePlacePred
list2OnePlacePred xs = \ x -> elem x xs 

compose :: OnePlacePred -> OnePlacePred -> OnePlacePred
compose p q = \ x -> (p x) && (q x)


composedOf :: Atom -> [MType]
composedOf x = case x of
    Ring1        -> [Metal']
    Ring2        -> [Metal']
    Ring3        -> [Metal']
    Sword1       -> [Wood', Metal']
    Sword2       -> [Wood', Metal']
    The_Rhine'   -> [Water']
    The_Genesee' -> [Water']
    Bottle1      -> [Wine', Glass']
    Bottle2      -> [Wine', Glass']
    Cup1         -> [Water', Glass']
    Cup2         -> [Water', Glass']
    Raft1        -> [Wood', Metal']
    Raft2        -> [Wood', Metal']
    Raft3        -> [Wood', Metal']
    Raft4        -> [Wood', Metal']
    Linda'       -> [Advice']
    Irene'       -> [Advice']
    Xena'        -> [Advice']
    Jim'         -> [Advice']
    Victor'      -> [Advice']
    Fred'        -> [Advice']
    Ollie'       -> [Advice']
    Zorba'       -> [Advice']
    SnowWhite'   -> [Advice']
    Alice'       -> [Advice']
    Goldilocks'  -> [Advice']
    Tom'         -> [Advice']
    _            -> [Everything']


--extension :: OnePlacePred -> [Entity]
--extension

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs) = x : (filter (\y -> y /= x) (unique xs))

fusion' :: Mass -> Mass -> Mass
fusion' (MassOf ts1 es1) (MassOf ts2 es2) = MassOf (ts1 ++ ts2) (es1 ++ es2)

fusion :: Entity -> Entity -> Entity
fusion (Ms' x) (Ms' y) = Ms' (x `fusion'` y)
fusion (Pl' x) (Pl' y) = Ms' (helper x y)
    where helper (Pl xs) (Pl ys) = MassOf [Everything'] (unique (xs ++ ys))

--constitutes' :: Plural -> Mass -> Bool

--constitutes :: Entity -> Entity -> Bool

materialize' :: Plural -> Mass
materialize' (Pl xs) = MassOf [Everything'] xs

materializeWith' :: [MType] -> Plural -> Mass
materializeWith' ts (Pl xs) = MassOf ts xs

materialize :: Entity -> Entity
materialize  m@(Ms' x) = m
materialize (Pl' (Pl ys)) = Ms' (MassOf [Everything'] ys)

materializeWith :: [MType] -> Entity -> Entity
materializeWith ts m@(Ms' x) = m
materializeWith ts (Pl' (Pl ys)) = Ms' (MassOf ts ys)

{-
passivize :: TwoPlacePred -> OnePlacePred
passivize r = \ x -> any (r x) atoms

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