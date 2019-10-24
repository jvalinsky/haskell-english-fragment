
module L0HW

where


--Below I have given a clean version of our solution derived in class to represent the syntax of L0 logic, deriving show manually.



import Data.List

data Name = Dick | Noam | John | Muhammad deriving (Eq,Show)
data Pred1 = HasMustache | IsBald deriving (Eq,Show)
data Pred2 = Knows | Loves deriving (Eq,Show)
data Form = P1 Pred1 Name | P2 Pred2 Name Name | Ng Form | Cnj [Form] | Dsj [Form] deriving Eq

instance Show Form where
    show (P1 pred1 name) = show pred1 ++ "(" ++ show name ++ ")"
    show (P2 pred2 name1 name2) = show pred2 ++ "(" ++ show name2 ++ "," ++ show name1 ++ ")"
    show (Ng  f)  = "~ " ++ show f
    show (Cnj fs) = "(" ++ intercalate " /\\ " (map show fs) ++ ")"
    show (Dsj fs) = "(" ++ intercalate " \\/ " (map show fs) ++ ")"





{-- Semantics of L0

Let's implement a more general version of the semantics of L0 as follows:

-assume that all models have 4 entities Nixon, Chomsky, Mitchell and Ali, i.e., the domain of entities is [ Nixon, Chomsky, Mitchell, Ali ]

-assume that the semantic values of the four names Dick, Noam, John, Muhammad are fixed in the obvious way: Dick denotes (Richard) Nixon, Noam denotes Chomsky, John denotes Mitchell and Muhammad denotes Ali

-generate all possible models that satisfy the above two constraints, i.e., generate all possible (combinations of) appropriate denotations for the one-place and two-place predicates listed above;

-in particular, one-place predicates should denote subsets of the domain of entities  and two-place predicates should denote sets of pairs of entities


--}


{-- Question 1:

Define the domain of entities underlying all our models (this is type e (for entity) in Montagovian semantics.)
what should this look like? 


--} 


--Insert data type Entity here
data Entity = Nixon | Chomsky | Mitchell | Ali deriving (Eq, Enum, Show)

--And further:  

--define "Entities", which gives an ordered list of the entities in the domain 
type Entities = [Entity]

entities :: Entities
entities = [Nixon .. Ali]


--For transitive verbs and the like, we will need to reason about pairs of entities, so define "entityPairs".
type EntityPair  = (Entity, Entity)
type EntityPairs = [EntityPair]

entityPairs :: EntityPairs
entityPairs = [ (e1, e2) | e1 <- [Nixon .. Ali], e2 <- [Nixon .. Ali] ]

{-- Question 2:


Now let’s start working towards the eval function.  First, let's consider the type of the interpretation function eval for L0.

How should this work?

The eval function should take a basic interpretation function as its first argument (i.e., the model), a formula as its second argument, and return a truth value, namely the semantic value of the formula under consideration relative to the model under consideration.


A basic interpretation function is just a function from basic expressions (names, one- place predicates and two-place predicates) to corresponding semantic values – let’s call them associated basic interpretations. We will need to define two data types, BasicExp and BasicInt, and then the the type of the interpretation function eval is:
eval ::(BasicExp → BasicInt) → Form → Bool
--}


--Insert data types for BasicExp and BasicInt:  use record syntax for both of them so that we can easily extract the individual expressions and the individual denotations wrapped together in these sum types. Note that record syntax automatically makes available functions that extract particular types of basic expressions or interpretations for basic expressions.


{--Hint: Both these types are sum types:
for the BasicExp type, we need to sum together names, one-place pred.s and two-place pred.s
for the BasicInt type, we need to sum together Entity values (semantic values for names, i.e., type e), Entity → Bool functions (semantic values for one-place pred.s, i.e., type ⟨e, t⟩) and Entity → Entity → Bool functions (semantic values for two-place pred.s, i.e., type ⟨e, ⟨e, t⟩⟩)--}

data BasicExp = NameExp { name :: Name } | Pred1Exp { pred1 :: Pred1 } | Pred2Exp { pred2 :: Pred2 } deriving (Eq, Show)

data BasicInt = EntityInt { entity :: Entity } | Pred1Int { pred1Fun :: (Entity -> Bool) } | Pred2Int { pred2Fun :: (Entity -> Entity -> Bool) } 


{-- Question 3:

Now we need to define models / valuations for L0, which are functions from basic expressions to their corresponding interpretations, i.e., functions of type BasicExp → BasicInt.

Need three helper functions:

The first two helper functions take lists of entities and lists of entity pairs and convert them to appropriate semantic values for one-place pred.s and two- place pred.s, respectively.


The third helper function, named basicIntFun, takes two lists of entities and two lists of pairs of entities as arguments – these are the denotations for our one-place pred.s and our two-place pred.s, respectively – and returns a model, i.e., a function of type BasicExp → BasicInt, constructed based on those 4 lists.
We always assign the same entities to the four names.


--}

--Insert helper function list2Pred1Value, list2Pred2Value, and basicIntFun here:
list2Pred1Value :: Entities -> (Entity -> Bool) 
list2Pred1Value es = \ e -> if e `elem` es then True else False


list2Pred2Value :: EntityPairs -> (Entity -> Entity -> Bool)
list2Pred2Value eps = \ e1 e2 -> if (e1, e2) `elem` eps then True else False 

basicIntFun :: Entities -> Entities -> EntityPairs -> EntityPairs -> (BasicExp -> BasicInt)
basicIntFun esMustache esBald epKnows epLoves = \ exp -> interp exp
    where interp NameExp { name = Dick }     = EntityInt { entity = Nixon }
          interp NameExp { name = Noam }     = EntityInt { entity = Chomsky }
          interp NameExp { name = John }     = EntityInt { entity = Mitchell }
          interp NameExp { name = Muhammad } = EntityInt { entity = Ali }
          interp Pred1Exp { pred1 = HasMustache } = Pred1Int { pred1Fun = list2Pred1Value esMustache }  
          interp Pred1Exp { pred1 = IsBald }      = Pred1Int { pred1Fun = list2Pred1Value esBald }  
          interp Pred2Exp { pred2 = Knows }       = Pred2Int { pred2Fun = list2Pred2Value epKnows }  
          interp Pred2Exp { pred2 = Loves }       = Pred2Int { pred2Fun = list2Pred2Value epLoves }  
                                                            


{-- Question 4:

Now we would like to generate all the models for L0.


We  need a final helper function:  define a powerset function that will generate all possible lists of entities and all possible lists of pairs of entities that we will assign as denotations for our one-place pred.s and two-place pred.s, respectively.

Now it is possible to define the allVals function for L0

--}


-- data Entity = Nixon | Chomsky | Mitchell | Ali deriving (Eq, Enum, Show)
--Insert helper function powerset here: 
-- If given the empty list then return [[]] else
-- concatenate the powerset of the tail of the list with the list consisting of
-- the head appended to each sublist of the powerset of the tail
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = (map (x:) (powerset xs)) ++ (powerset xs)

--Insert allVals function here:
allVals :: [(BasicExp -> BasicInt)]
allVals = [ basicIntFun esM esB epK epL | esM <- (powerset entities )
                                        , esB <- (powerset entities)
                                        , epK <- (powerset entityPairs) 
                                        , epL <- (powerset entityPairs) ] 

{-- Question 5:


You are finally ready to provide the recursive definition of the eval function, which is actually the definition of the semantics of L0. The eval function is of type (BasicExp → BasicInt) → Form → Bool:

-it takes a model / valuation as its first argument, i.e., an assignment of semantic values to basic expressions, which is just a function of type BasicExp → BasicInt;

-it takes a formula of type Form as its second argument;

-it returns a truth value of type Bool as its value; 

-this truth value is the semantic value of the formula under consideration relative to the model under consideration.
--}



--Insert the eval function here (and convince yourself that it works!)
eval :: (BasicExp -> BasicInt) -> Form -> Bool
eval model (P1 prd1 n) = (pred1Fun (model (Pred1Exp { pred1 = prd1 }))) (entity (model NameExp { name = n}))
eval model (P2 prd2 n1 n2) = (pred2Fun (model (Pred2Exp { pred2 = prd2 }))) (entity (model NameExp { name = n1})) (entity (model NameExp { name = n2}))
eval model (Ng f) = not (eval model f)
eval model (Cnj fs) = and (map (eval model) fs)
eval model (Dsj fs) = or  (map (eval model) fs)

