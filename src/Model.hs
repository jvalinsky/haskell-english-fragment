module Model where

import Data.List (intersect, union, (\\), sort)

type OnePlacePred   = Entity -> Bool
type TwoPlacePred   = Entity -> Entity -> Bool
type ThreePlacePred = Entity -> Entity -> Entity -> Bool

-- i operator means there exists a unique
-- lambda is from lambda calculus
-- Plural Types
data Atom = Alice'   | Bob'    | Cyrus' | Ellie' | Irene' | SnowWhite'  | Sword' | Bottle' |
             Ollie'  | Quine'  | Ring'  | Harry' | Xena'   deriving (Eq, Show, Bounded, Enum, Ord)

type Tag = Int
data Mass = Mass' [Tag] deriving Show
data Plural = Plural' [Atom] deriving Show

data Entity = Pl' Plural | Ms' Mass deriving Show
  
atoms :: [Atom]
atoms = [minBound..maxBound]

domain :: [Entity]
domain = (map Pl' (map Plural' (powerset atoms))) ++ masses

masses = [materialize_ [Ring'], materialize_ [Sword']]

-- The Portion of Matter have unique Tag numbers, 
-- a material fusion of portions of matter has a list
-- of the individual Tag numbers for each portion of matter

-- The Tag number for the portion of matter that corresponds to
-- the portion of matter of an individual (i.e. materialize (a :: Atom) ) 
-- is the result of fromEnum a
materialize'' :: [Atom] -> Mass
materialize'' xs =  Mass' (map fromEnum xs)

materialize_ :: [Atom] -> Entity
materialize_ xs =  Ms' (materialize'' xs)

materialize' :: Plural -> Mass
materialize' (Plural' xs) =  materialize'' xs

materialize :: Entity -> Entity
materialize (Ms' m)  = (Ms' m)
materialize (Pl' xs) = (Ms' (materialize' xs)) 

mPart_ :: Mass -> Mass -> Bool
mPart_ (Mass' x) (Mass' y) = x `subList` y

mPart' :: Plural -> Plural -> Bool
mPart' x y = (materialize' x) `mPart_` (materialize' y)

mPart :: Entity -> Entity -> Bool
mPart x y = (materialize x) `mPart` (materialize y)

mEqual_ :: Mass -> Mass -> Bool
mEqual_ (Mass' x) (Mass' y) = equal' x y

instance Eq Mass where
    (==) = mEqual_

----------------------
-- Helper Functions --
----------------------

instance Eq Plural where
    (==) = pEqual

pEqual :: Plural -> Plural -> Bool
pEqual (Plural' xs) (Plural' ys) = xs `equal'` ys

equal' :: (Eq a, Ord a) => [a] -> [a] -> Bool
equal' xs ys =  (sort xs) == (sort ys)

x /=* y = not (equal' x y)
x ==* y = equal' x y

entEqual :: Entity -> Entity -> Bool
entEqual (Pl' x) (Ms' y) = False
entEqual (Ms' x) (Pl' y) = False

entEqual (Pl' (Plural' x)) (Pl' (Plural' y)) = x `equal'` y
entEqual (Ms' x) (Ms' y) = x `mEqual_` y

instance Eq Entity where
    (==) = entEqual


list2OnePlacePredM :: [Mass] -> OnePlacePred
list2OnePlacePredM xs = \m -> elem m (map Ms' xs)

list2OnePlacePred' :: [Atom] -> OnePlacePred
list2OnePlacePred' xs = \ x -> elem x ents
    where rmEmptySet xs = filter (\x -> x /= []) xs
          lst = rmEmptySet $ powerset xs
          ents = map Pl' (map Plural' lst)

compose :: OnePlacePred -> OnePlacePred -> OnePlacePred
compose p q = \ x -> (p x) && (q x)

subList :: (Eq a) => [a] -> [a] -> Bool
subList xs ys = length (filter (\x -> x `elem` ys) xs) == length xs

ipart'' :: [Atom] -> [Atom] -> Bool
ipart'' xs ys = xs `subList` ys

ipart' :: Plural -> Plural -> Bool
ipart' (Plural' xs) (Plural' ys) = xs `ipart''` ys

ipart :: Entity -> Entity -> Bool
ipart (Pl' x) (Pl' y) = x `ipart'` y
ipart _ _ = False 

-- From my L0HW.hs 
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = (map (x:) (powerset xs)) ++ (powerset xs)

propPlurals :: [[Atom]] -> [[Atom]]
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
personList = atoms \\ thingList

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
cleanList = atoms \\ dirtyList

tallList :: [Atom]
tallList = giantList ++ [Xena', Ollie', Irene']

shortList :: [Atom]
shortList = (atoms \\ thingList) \\ tallList

goldList :: [Atom]
goldList = [Ring']

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


-- Three-Place Predicate Lists
giveListM :: [[Entity]]
giveListM = [ [Pl' (Plural' [Ellie']), Pl' (Plural' [Alice']), materialize_ [Ring']  ] ]

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
metal = (list2OnePlacePred' metalList) `or'` (list2OnePlacePredM goldMList)

gold :: OnePlacePred
gold = (list2OnePlacePred' goldList) `or'` (list2OnePlacePredM goldMList)

goldMList :: [Mass]
goldMList = [materialize'' [Ring']]

people :: OnePlacePred
people = plural `compose` person

person :: OnePlacePred
person = list2OnePlacePred' personList

old :: OnePlacePred
old = (list2OnePlacePred' oldList) `or'` (list2OnePlacePredM oldMList)

oldMList :: [Mass]
oldMList = [materialize'' [Ring'], materialize'' [Sword']]

new :: OnePlacePred
new = list2OnePlacePred' newList

young :: OnePlacePred
young = list2OnePlacePred' youngList

groupPred :: Ordering -> Int -> Entity -> Bool
groupPred ord n = f
    where f (Pl' (Plural' xs)) = groupPred' xs  
          f (Ms' y) = False
          groupPred' = \x -> (length x) `compare` n == ord

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

coven :: OnePlacePred
coven (Pl' x) = x == (Plural' witchList)
coven _ = False

-- Two-Place Predicates

elem' :: (Eq a, Ord a) => [a] -> [[a]] -> Bool
elem' xs xss = any (equal' xs) xss

-- Communitative (order doesn't matter)
list2TwoPlacePred' :: [[Atom]] -> [Atom] -> [Atom] -> Bool
list2TwoPlacePred' xs = \x -> (\y -> findPair x y xs)
    where findPair :: [Atom] -> [Atom] -> [[Atom]] -> Bool
          findPair [x] [y] pairs = elem' [x, y] pairs
          findPair _ _ _ = False

-- Non-communitive (order does matter)
list2TwoPlacePred :: [[Atom]] -> [Atom] -> [Atom] -> Bool
list2TwoPlacePred xs = \x -> (\y -> findPair x y xs)
    where findPair :: [Atom] -> [Atom] -> [[Atom]] -> Bool
          findPair [x] [y] pairs = elem [x, y] pairs
          findPair _ _ _ = False

fight :: TwoPlacePred
fight (Pl' (Plural' x)) (Pl' (Plural' y)) = fight' x y
    where fight' = list2TwoPlacePred' fightList

fight  _ (Ms' y) = False
fight (Ms' x) _ = False

defeat :: TwoPlacePred
defeat (Pl' (Plural' x)) (Pl' (Plural' y)) = defeat' x y
    where defeat' = list2TwoPlacePred defeatList

defeat  _ (Ms' y) = False
defeat (Ms' x) _ = False

help :: TwoPlacePred
help (Pl' (Plural' x)) (Pl' (Plural' y)) = help' x y
    where help' = list2TwoPlacePred helpList

help  _ (Ms' y) = False
help (Ms' x) _  = False

chase :: TwoPlacePred
chase (Pl' (Plural' x)) (Pl' (Plural' y)) =  chase' x y
    where chase' = list2TwoPlacePred chaseList

chase  _ (Ms' y) = False
chase (Ms' x) _  = False

-- Three-Place Predicates
list2ThreePlacePred :: [[Atom]] -> [Atom] -> [Atom] -> [Atom] -> Bool
list2ThreePlacePred xs = \x -> (\y -> (\z -> findThree x y z xs))
    where findThree :: [Atom] -> [Atom] -> [Atom] -> [[Atom]] -> Bool
          findThree [x] [y] [z] threes = elem [x, y, z] threes
          findThree _ _ _ _ = False

elem3m :: [Entity] -> [[Entity]] -> Bool
elem3m [(Pl' x), (Pl' y), (Ms' z)] xss = any (\[(Pl' x'), (Pl' y'), (Ms' z')] -> (x == x' && y == y' && z == z')) xss
elem3m _ xss = False 

list2ThreePlacePredM :: [[Entity]] -> ThreePlacePred
list2ThreePlacePredM xs = \x y z -> findThree x y z xs
    where findThree :: Entity -> Entity -> Entity -> [[Entity]] -> Bool
          findThree x y z threes = elem3m [x, y, z] threes

give :: ThreePlacePred
give (Pl' (Plural' x)) (Pl' (Plural' y)) (Pl' (Plural' z)) =  give' x y z
    where give' = list2ThreePlacePred giveList

give x@(Pl' x') y@(Pl' y') z@(Ms' z') = giveM x y z
    where giveM = list2ThreePlacePredM giveListM

give _ _ _ = False

gold' :: OnePlacePred
gold' = list2OnePlacePredM goldMList