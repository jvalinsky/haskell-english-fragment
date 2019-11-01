module Model where

import Data.List (intersect, union, (\\))

-- This constrains type variable a to be an instance of Entity
type OnePlacePred   = Entity -> Bool
type TwoPlacePred   = Entity -> Entity -> Bool
type ThreePlacePred = Entity -> Entity -> Entity -> Bool

-- i operator means there exists a unique
-- lambda is from lambda calculus
-- Entity Types
data Atom = Alice'   | Bob'    | Cyrus' | Ellie' | Irene' | SnowWhite'  | Sword' | Bottle' |
             Ollie'  | Quine'  | Ring'  | Harry' | Xena'   deriving (Eq, Show, Bounded, Enum)

  
atoms' :: [Atom]
atoms' = [minBound..maxBound]

atoms :: [Entity]
atoms = map (:[]) atoms'

domain :: [Entity]
domain = powerset atoms'

type Entity = [Atom]

type Tag = Int
data Mass = Mass Tag

--materialize :: Entity -> Mass
--materialize x = 

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


ipart :: [Atom] -> [Atom] -> Bool
ipart xs ys = xs `subList` ys

-- From my L0HW.hs 
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = (map (x:) (powerset xs)) ++ (powerset xs)

propPlurals :: [Entity] -> [Entity]
propPlurals xss = filter (\x -> (length) x > 1) xss

-- Lists for OnePlacePreds

giantList :: [Atom]
giantList = [Cyrus', Bob']

dwarfList :: [Atom]
dwarfList = [Irene']

thingList :: [Atom]
thingList = [Ring', Bottle', Sword']

ringList :: [Atom]
ringList = [Ring']

bottleList :: [Atom]
bottleList = [Bottle']

swordList :: [Atom]
swordList = [Sword']

metalList :: [Atom]
metalList = [Ring', Sword']

rustyList :: [Atom]
rustyList = [Sword']

personList :: [Atom]
personList = atoms' \\ thingList

magicalList :: [Atom]
magicalList = [Alice', Ellie',  Irene',Harry', Quine']

wizardList :: [Atom]
wizardList = maleList `intersect` magicalList

witchList :: [Atom]
witchList = femaleList `intersect` magicalList

maleList :: [Atom]
maleList = [Bob', Cyrus', Ollie',  Harry', Quine']

femaleList :: [Atom]
femaleList = [Alice', Ellie', Irene', SnowWhite', Xena']

youngList :: [Atom]
youngList = [Ollie', Harry', Alice', Ellie', SnowWhite']

oldList :: [Atom]
oldList = (personList \\ youngList) ++ [Bottle', Sword']

newList :: [Atom]
newList = [Ring']

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

wiseList :: [Atom]
wiseList = [Irene', Xena', Quine']

foolishList :: [Atom]
foolishList = [Alice', Ollie', SnowWhite']

laughList :: [Atom]
laughList = [Alice', Ollie']

runList :: [Atom]
runList = warriorList ++  [ Alice',  Ellie']

walkList :: [Atom]
walkList = [Ollie', Bob', SnowWhite']

smileList :: [Atom]
smileList = [Irene', Ollie', Alice']

swimList :: [Atom]
swimList = [Ollie', Irene', Alice']

coldList :: [Atom]
coldList = [Irene', Alice']

warriorList :: [Atom]
warriorList = [Xena', Irene', Bob', Cyrus', Quine'] 

badList :: [Atom]
badList =  [Ellie', Ollie']

goodList :: [Atom]
goodList = [Xena', Quine']

dirtyList :: [Atom]
dirtyList = [Ollie', Harry', Ring']

cleanList :: [Atom]
cleanList = atoms' \\ dirtyList

tallList :: [Atom]
tallList = giantList ++ [Xena', Ollie', Irene']

shortList :: [Atom]
shortList = (atoms' \\ thingList) \\ tallList

-- Lists for TwoPlacePreds
chaseList :: [[Atom]]
chaseList = [ [SnowWhite'      , Ollie'     ],
              [Bob'      , Ollie'     ], 
              [SnowWhite'      , Harry'     ],
              [Bob'      , Harry'     ],
              [Bob'      , Xena'     ],
              [Bob'      , Cyrus'     ],
              [Ollie'    , Alice'     ], 
              [Ollie'    , Ellie'     ],
              [Ollie'    , SnowWhite' ],
              [Alice'  , Ellie'     ],
              [SnowWhite', Harry'   ] ]

fightList :: [[Atom]]
fightList = [ [Xena'  , Irene'],
              [Ellie', Cyrus'  ],
              [Ollie'  , SnowWhite'  ],
              [Harry', Alice'],
              [Ollie'  , Cyrus'  ] ]

defeatList :: [[Atom]]
defeatList = [ [Xena'  , Irene' ],
               [Cyrus', Ellie'],
               [Ollie'   , Cyrus'],
               [Ollie'   , SnowWhite'],
               [Harry', Alice'] ]

helpList :: [[Atom]]
helpList = [ [Quine', Harry'   ],
             [Ollie' , Alice'  ],
             [Ollie' , Bob'    ],
             [Alice' , Irene'],
             [Alice' , Ellie'  ],
             [Alice' , SnowWhite'],
             [Irene' , Xena'  ],
             [Xena'  , Bob' ] ]

-- Three-Place Predicate Lists
giveList :: [[Atom]]
giveList = [ [Irene' , Alice' , Bottle'  ],
             [Irene'   , Ellie' , Bottle'  ],
             [Quine'  , Xena'  , Sword' ],
             [Ellie', Alice', Ring'  ] ]

-- One-Place Predicates
giant :: OnePlacePred
giant = list2OnePlacePred' giantList

thing :: OnePlacePred
thing = list2OnePlacePred' thingList

dwarf :: OnePlacePred
dwarf = list2OnePlacePred' dwarfList

magical :: OnePlacePred
magical = list2OnePlacePred' magicalList

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

female :: OnePlacePred
female = list2OnePlacePred' femaleList

male :: OnePlacePred
male = list2OnePlacePred' maleList

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

ring :: OnePlacePred
ring = list2OnePlacePred' ringList

bottle :: OnePlacePred
bottle = list2OnePlacePred' bottleList

sword :: OnePlacePred
sword = list2OnePlacePred' swordList

metal :: OnePlacePred
metal = list2OnePlacePred' metalList

people :: OnePlacePred
people = plural `compose` person

person :: OnePlacePred
person = list2OnePlacePred' personList

old :: OnePlacePred
old = list2OnePlacePred' oldList

new :: OnePlacePred
new = list2OnePlacePred' newList

young :: OnePlacePred
young = list2OnePlacePred' youngList

groupPred :: Ordering -> Int -> OnePlacePred
groupPred ord n = \x -> (length x) `compare` n == ord

-- Collective Predicate
numerous :: OnePlacePred
numerous = groupPred GT 2

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
scatter = plural

gather :: OnePlacePred
gather = group'

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