module Model where

import Data.List

-- This constrains type variable a to be an instance of Entity
type OnePlacePred   = Entity -> Bool
type TwoPlacePred   = Entity -> Entity -> Bool
type ThreePlacePred = Entity -> Entity -> Entity -> Bool

-- i operator means there exists a unique
-- lambda is from lambda calculus
-- Entity Types
data Atom = Sword1      | Sword2   | Alice'   | Bob'    | Cyrus'     | Ellie'       | 
            Goldilocks' | Hillary' | Irene'   | Jim'    | Kim'       | Linda'       | 
            Noah'       | Ollie'   | Penny'   | Quine'  | Dagger1    | Stuart'      |
            SnowWhite'  | Tom'     | Uli'     | Victor' | Harry'    | Xena'        |
            Zorba'      | Cup1     | Cup2     | Bottle1 | Bottle2    | The_Genesee' |
            Dress1      | Dress2   | Raft1    | Raft2   | Raft3      | Raft4        |
            Dorothy'    | Fred'    | Glasses1 | Ring1   | Whiskers'  | Mittens'     | 
            Gerald'     | Minnie'  | Mickey'  | Sue'    | The_Rhine' | Dis'         | 
            Thorin'     | Ring2    | Ring3    deriving (Eq, Show, Bounded, Enum)


atoms :: [Atom]
atoms = [minBound..maxBound]

type Entity = [Atom]

----------------------
-- Helper Functions --
----------------------

equal' :: Entity -> Entity -> Bool
equal' xs ys = (lenXS == lenYS) && (lenXS == lenFiltered)
    where lenXS = length xs
          lenYS = length ys
          lenFiltered = length (filter (\x -> x `elem` ys) xs) 

x /=* y = not (equal' x y)
x ==* y = equal' x y

elem' :: [Atom] -> [[Atom]] -> Bool
elem' xs xss = any (equal' xs) xss

list2OnePlacePred :: [Entity] -> OnePlacePred
list2OnePlacePred xs = \ x -> elem' x xs

list2OnePlacePred' :: [Atom] -> OnePlacePred
list2OnePlacePred' xs = \ x -> elem' x (rmEmptySet $ powerset xs) 
    where rmEmptySet xs = filter (\x -> x /= []) xs

list2OnePlacePredProp :: [Atom] -> OnePlacePred
list2OnePlacePredProp xs = \ x -> elem' x (propPlurals $ powerset xs) 

compose :: OnePlacePred -> OnePlacePred -> OnePlacePred
compose p q = \ x -> (p x) && (q x)

unique' :: [[Atom]] -> [[Atom]]
unique' [[]] = [[]]
unique' (x:xs) = x : (filter (\y -> y /=* x) (unique' xs))

subList :: (Eq a) => [a] -> [a] -> Bool
subList xs ys = length (filter (\x -> x `elem` ys) xs) == length xs


-- From my L0HW.hs 
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = (map (x:) (powerset xs)) ++ (powerset xs)

propPlurals :: [Entity] -> [Entity]
propPlurals xss = filter (\x -> (length) x > 1) xss

-- Lists for OnePlacePreds
thingList :: [Atom]
thingList = [Sword1, Sword2, Dagger1, Cup1, Cup2, Bottle1, Bottle2, The_Genesee', The_Rhine',
              Dress1, Dress2, Raft1, Raft2, Raft3, Raft4, Glasses1, Ring1, Ring2, Ring3]

glassesList :: [Atom]
glassesList = [Glasses1]

swordList :: [Atom]
swordList = [Sword1, Sword2]

animalList :: [Atom]
animalList = [Stuart', Minnie', Mickey', Whiskers', Mittens', Gerald', Sue']

giantList :: [Atom]
giantList = [Tom', Bob']

dwarfList :: [Atom]
dwarfList = [Dis', Thorin']

beingList :: [Atom]
beingList = animalList ++ personList

catList :: [Atom]
catList = [Whiskers', Mittens']

mouseList :: [Atom]
mouseList = [Mickey', Minnie', Sue']

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

raftList :: [Atom]
raftList = [Raft1, Raft2, Raft3, Raft4]

dressList :: [Atom]
dressList = [Dress1, Dress2]

riverList :: [Atom]
riverList = [The_Genesee', The_Rhine']

magicalList :: [Atom]
magicalList = [Penny', Alice', Jim', Linda', Ellie', Victor', Kim', Dis', 
               Thorin', Tom', Cup1, Sword1, Ring1, Bottle1, Harry']

wizardList :: [Atom]
wizardList = personList `intersect` magicalList

witchList :: [Atom]
witchList = personList `intersect` magicalList

maleList :: [Atom]
maleList = [Bob', Cyrus', Jim', Noah', Ollie', Penny', Quine', Stuart', Tom', Uli', Victor',
            Harry', Zorba', Fred', Whiskers', Gerald', Mickey', Thorin']

femaleList :: [Atom]
femaleList = [Alice', Ellie', Goldilocks', Hillary', Irene', Kim', Linda', SnowWhite', Xena',
              Dorothy', Mittens', Minnie', Sue', Dis']

youngList :: [Atom]
youngList = [Ollie', Penny', Stuart', Uli', Harry', Noah', Tom',
             Alice', Ellie', Goldilocks', SnowWhite', Dorothy', Mittens']

oldList :: [Atom]
oldList = [Sword1, Bottle1, Ring3, Dress2, Glasses1] ++ (personList \\ youngList) ++ birdList ++ metalList 

newList :: [Atom]
newList = (thingList \\ oldList) 

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

shinyList :: [Atom]
shinyList = metalList `intersect` newList

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

warriorList :: [Atom]
warriorList = [Xena', Thorin', Irene', Victor', Fred', Cyrus', Quine'] 

badList :: [Atom]
badList =  [Noah', Kim']

goodList :: [Atom]
goodList = [Fred', Xena', Thorin']

tornList :: [Atom]
tornList = [Dress2]
   
dirtyList :: [Atom]
dirtyList = [Ollie', Harry', Dress2, Ring3]

cleanList :: [Atom]
cleanList = atoms \\ dirtyList

tallList :: [Atom]
tallList = giantList ++ [Xena', Fred', Victor', Ollie', Irene', Bottle2]

shortList :: [Atom]
shortList = dwarfList ++ [Uli', Alice', Goldilocks', Quine', Harry', Bottle1]

metalList :: [Atom]
metalList = [Dagger1, Sword1, Sword2, Glasses1] ++ ringList

steelList :: [Atom]
steelList = [Sword1, Sword2, Dagger1]

goldList :: [Atom]
goldList = [Ring1, Ring2]

silverList :: [Atom]
silverList = [Ring3]

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

-- Three-Place Predicate Lists
giveList :: [[Atom]]
giveList = [ [Irene' , Alice' , Dress1  ],
             [Kim'   , Ellie' , Dress2  ],
             [Fred'  , Xena'  , Dagger1 ],
             [Dis'   , Thorin', Sword1  ],
             [Victor', Fred'  , Sword2  ] ]

-- One-Place Predicates
giant :: OnePlacePred
giant = list2OnePlacePred' giantList

dwarf :: OnePlacePred
dwarf = list2OnePlacePred' dwarfList

magical :: OnePlacePred
magical = list2OnePlacePred' magicalList

metal :: OnePlacePred
metal = list2OnePlacePred' metalList

steel :: OnePlacePred
steel = list2OnePlacePred' steelList

gold :: OnePlacePred
gold = list2OnePlacePred' goldList

silver :: OnePlacePred
silver = list2OnePlacePred' silverList

short :: OnePlacePred
short = list2OnePlacePred' shortList

tall :: OnePlacePred
tall = list2OnePlacePred' tallList

clean :: OnePlacePred
clean = list2OnePlacePred' cleanList

dirty :: OnePlacePred
dirty = list2OnePlacePred' dirtyList

bad :: OnePlacePred
bad = list2OnePlacePred' badList

good :: OnePlacePred
good = list2OnePlacePred' goodList

torn :: OnePlacePred
torn = list2OnePlacePred' tornList

warrior :: OnePlacePred
warrior = list2OnePlacePred' warriorList

cold :: OnePlacePred
cold = list2OnePlacePred' coldList

smile :: OnePlacePred
smile = list2OnePlacePred' smileList

run :: OnePlacePred
run = list2OnePlacePred' runList

walk :: OnePlacePred
walk = list2OnePlacePred' walkList

laugh :: OnePlacePred
laugh = list2OnePlacePred' laughList

swim :: OnePlacePred
swim = list2OnePlacePred' swimList

foolish :: OnePlacePred
foolish = list2OnePlacePred' foolishList

wise :: OnePlacePred
wise = list2OnePlacePred' wiseList

rusty :: OnePlacePred
rusty = list2OnePlacePred' rustyList

shiny :: OnePlacePred
shiny = list2OnePlacePred' shinyList

ring :: OnePlacePred
ring = list2OnePlacePred' ringList

sword :: OnePlacePred
sword = list2OnePlacePred' swordList

raft :: OnePlacePred
raft = list2OnePlacePred' raftList

cup :: OnePlacePred
cup = list2OnePlacePred' cupList

bottle :: OnePlacePred
bottle = list2OnePlacePred' bottleList

glasses :: OnePlacePred
glasses = list2OnePlacePred' glassesList

river :: OnePlacePred
river = list2OnePlacePred' riverList

female :: OnePlacePred
female = list2OnePlacePred' femaleList

male :: OnePlacePred
male = list2OnePlacePred' maleList

thing :: OnePlacePred
thing = list2OnePlacePred' thingList

boy :: OnePlacePred
boy = list2OnePlacePred' boyList

girl :: OnePlacePred
girl = list2OnePlacePred' girlList

man :: OnePlacePred
man = list2OnePlacePred' manList

woman :: OnePlacePred
woman = list2OnePlacePred' womanList

wizard :: OnePlacePred
wizard = list2OnePlacePred' wizardList

witch :: OnePlacePred
witch = list2OnePlacePred' witchList

mouse :: OnePlacePred
mouse = list2OnePlacePred' mouseList

cat :: OnePlacePred
cat = list2OnePlacePred' catList

bird :: OnePlacePred
bird = list2OnePlacePred' birdList

animal :: OnePlacePred
animal = list2OnePlacePred' animalList

people :: OnePlacePred
people = plural `compose` ((negate' animal) `and'` (negate' thing))

person :: OnePlacePred
person = list2OnePlacePred (map (:[]) personList)

being :: OnePlacePred
being = list2OnePlacePred' beingList

old :: OnePlacePred
old = list2OnePlacePred' oldList

young :: OnePlacePred
young = list2OnePlacePred' youngList

new :: OnePlacePred
new = list2OnePlacePred' newList

dress :: OnePlacePred
dress = list2OnePlacePred' dressList

groupPred :: Ordering -> Int -> OnePlacePred
groupPred ord n = \x -> (length x) `compare` n == ord

-- Collective Predicate
numerous :: OnePlacePred
numerous = groupPred GT 3

couple :: OnePlacePred
couple = groupPred EQ 2

group' :: OnePlacePred
group' = groupPred GT 2

crowd :: OnePlacePred
crowd = groupPred GT 4

atom :: OnePlacePred
atom = groupPred EQ 1

plural :: OnePlacePred
plural = groupPred GT 1

negate' :: OnePlacePred -> OnePlacePred
negate' p = \x -> not (p x)

or' :: OnePlacePred -> OnePlacePred -> OnePlacePred
or' p q = \x -> (p x) || (q x)

and' :: OnePlacePred -> OnePlacePred -> OnePlacePred
and' p q = \x -> (p x) && (q x)

scatter :: OnePlacePred
scatter = plural `compose` (negate' thing)

gather :: OnePlacePred
gather = group' `compose` (negate' thing)

-- Two-Place Predicates

-- Communitative (order doesn't matter)
list2TwoPlacePred' :: [[Atom]] -> TwoPlacePred
list2TwoPlacePred' xs = \x -> (\y -> findPair x y xs)
    where findPair :: [Atom] -> [Atom] -> [[Atom]] -> Bool
          findPair [x] [y] pairs = elem' [x, y] pairs
          findPair _ _ _ = False

-- Non-communitive (order does matter)
list2TwoPlacePred :: [[Atom]] -> TwoPlacePred
list2TwoPlacePred xs = \x -> (\y -> findPair x y xs)
    where findPair :: [Atom] -> [Atom] -> [[Atom]] -> Bool
          findPair [x] [y] pairs = elem [x, y] pairs
          findPair _ _ _ = False

fight :: TwoPlacePred
fight = list2TwoPlacePred' fightList

defeat :: TwoPlacePred
defeat = list2TwoPlacePred defeatList

help :: TwoPlacePred
help = list2TwoPlacePred helpList

drink :: TwoPlacePred
drink = list2TwoPlacePred drinkList

chase :: TwoPlacePred
chase = list2TwoPlacePred chaseList

-- Three-Place Predicates
list2ThreePlacePred :: [[Atom]] -> ThreePlacePred
list2ThreePlacePred xs = \x -> (\y -> (\z -> findThree x y z xs))
    where findThree :: [Atom] -> [Atom] -> [Atom] -> [[Atom]] -> Bool
          findThree [x] [y] [z] threes = elem [x, y, z] threes
          findThree _ _ _ _ = False

give :: ThreePlacePred
give = list2ThreePlacePred giveList