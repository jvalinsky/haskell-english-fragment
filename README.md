# Collectivity, Distributivity, Mass Terms, and Parser Combinators
## by Jack Valinsky


### Files:
- src/Model          &rarr; defines individual and mass types and predicates
- src/EF2synShort.hs &rarr; syntax definition for English fragment
- src/EF3semShort.hs &rarr; how to evaluate truth value of a synactic type given a model
- src/Parser/hs      &rarr; how to parse strings to syntactic types and evaluate them
- app/Main.hs        &rarr; demo showing evaluation of different sentences


### How to build demo:
- <code> stack build && stack exec plurals-lin268-exe </code>
- Alternatively one can just load all the files in src/ in the repl using <code> stack ghci </code>

### Modeling Individual Entities and Mass Terms and their Predicates

In my collective-distributive homework I represented plural entities as lists of atoms. To allow predicates
to distinguish between mass terms and plural entities I modified my definition of <code>Entity</code>.

```haskell
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

-- ...

materialize'' :: [Atom] -> Mass
materialize'' xs =  Mass' (map fromEnum xs)

materialize_ :: [Atom] -> Entity
materialize_ xs =  Ms' (materialize'' xs)
```

I use pattern matching in my predicates to check if a value of type <code>Entity</code> is a mass term or a plural entity.
My definition of the <code>Mass</code> type allows for a clear connection between individuals and portions of matter (I just use
the number returned by fromEnum for the <code>Tag</code>) but while allowing for portions of matter that do not correspond to 
any individual (just use a <code>Tag</code> greater than <code>fromEnum maxBound</code>). I do not do anything clever regarding defining
the mass terms in my domain, I just add a few chosen mass terms to my domain list. This was in interest of time because I had tried
many possible ideas that did not result in working code. The <code>domain</code> variable is used inside my implementation of the <code>intDET</code>
for interpretating determiners as shown below in the semantics section. It is used as a starting domain of entities to filter through using
predicates that the determiners modify (i.e. the nouns).


As an illustration of how I handle predicates which differentiate between individuals and masses, here is my code for implementing predicate
for the verb "give":
```haskell
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
```

Here I am not handling plural entities because of the complexity involved (i.e. combinatoric explosion of possible interpretations), 
only my 1-place predicates handle plurals. I allow for a mass to be given to an individual extensive use of pattern matching.
My plural and mass types implement the <code>Eq</code> typeclass to be able to handle different notions of equality for masses and plurals.


"Scatter" is an example of collective predicate which only works for the domain of plural objects and not atoms (ex: one person cannot scatter),
I utilize a <code>groupPred</code> helper function to count the numer of atoms a plural entity is composed of. In this case I define "scatter" to be true
for any group of atoms greater than two.

### Syntax

```haskell
data CN = Sng SCN | Pl PCN | Ms MCN deriving Show 

data MCN = Gold_ deriving (Bounded, Enum, Show, Eq)

data PCN = Plur SCN deriving Show
```

I added syntax types for singular (<code>SCN</code>), plural (<code>PCN</code>), and mass terms (<code>MCN</code>) as shown above. 
Plurals are constructed from singular nouns and <code>CN</code>'s or "common nouns" are constructed from either of the three subtypes of nouns.
My type constructor for a gold mass <code>Gold_</code> has an underscore because I also have a gold adjective type and haskell does not
allow type constructors with the same name for different types in the same module. 

```haskell

data DET' = Each' | Every' | Some' | Most' | The' | A' | All' | No' deriving Show

data DP =   Some1  PCN  | Some2  ADJ PCN | Some3  RPCN | Each1 SCN  | Each2 ADJ SCN | Each3 RSCN |
            Every1 SCN  | Every2 ADJ SCN | Every3 RSCN | Most1 PCN  | Most2 ADJ PCN | Most3 RPCN |
            The1   CN   | The2   ADJ CN  | The3   RCN  | A1    SCN  | A2    ADJ SCN | A3    RSCN |
            All1   PCN  | All2   ADJ PCN | All3   RPCN | No1   CN   | No2   ADJ CN  | No3   RCN  | 
            Empty Name  | Some4 MCN | Most4 MCN | The4 MCN | All4 MCN | No4 MCN deriving Show
```

I used type constructors to restrict the type of noun a determiner can receive to make a determiner phrase as shown above.
The <code>DET'</code> type is not used in constructing any syntax types but rather is used as a hack for defining my interpretation
function for determiner phrases as will be discussed in the Semantics section.

<code>RCN</code>, <code>RSCN</code>, and <code>RPCN</code> correspond to phrases constructed using "That". 

```haskell

data RCN = RCN1 CN That VP | RCN2 CN That DP TV | RCN3 ADJ CN That VP | RCN4 ADJ CN That DP TV deriving Show

data RSCN = RSCN1 SCN That VP | RSCN2 SCN That DP TV | RSCN3 ADJ SCN That VP | RSCN4 ADJ SCN That DP TV deriving Show

data RPCN = RPCN1 PCN That VP | RPCN2 PCN That DP TV | RPCN3 ADJ PCN That VP | RPCN4 ADJ PCN That DP TV deriving Show
```

Since my determiners are
restricted to what type of noun they can take, that is why I have three different <code>RCN</code> types instead of one. This could
possibly been rewritten to use less types but it works. Notice I also have type constructors to handle the case of an optional adjective.
This could have been implemented using a <code>Maybe ADJ</code> type but I did not do this for simplicity, once you start using <code>Maybe</code>
your code tends to have to handle it everywhere.

### Semantics
Here are snippets of my interpretation function for determiners and determiner phrases:

```haskell
-- Based on starter code by Professor Grimm
intDET :: DET' -> (Entity -> Bool) -> (Entity -> Bool) -> Bool
intDET All' p q = all q (filter p domain) 
intDET Each' p q = all q (filter p domain) 
intDET Some' p q = any q (filter p domain)
intDET A' p q = any q (filter p domain)
intDET Every' p q = all q (filter p domain)
intDET The' p q = (not $ null plist) && singleton plist && q (maxElement plist)
    where plist = filter p domain
          singleton [x] = True
          singleton xs  = all (\x -> x `ipart` (maxElement xs)) xs

intDP :: DP -> OnePlacePred -> Bool
intDP (The1 cn) = (intDET The') (intCN cn)
intDP (The2 adj cn) = (intDET The') ((intADJ adj) (intCN cn))
intDP (The3 rcn)    = (intDET The') (intRCN rcn)
intDP (The4 mcn)    = (intDET The') (intMCN mcn)
```
I pattern match on the type constructor for my <code>DP</code> type and give the corresponding <code>DET'</code> type to my intDET function,
this was the so-called "hack" I mentioned earlier. Implementing my syntax and semantics this way allows for me to syntactically restrict the types
of nouns a determiner can take yet still have a single intDET function to handle all the different determiners. There are probably cleaner ways to do this.

Mass terms and plural object affect my implementation of determiners, which is most apparent in how I handle the "The" determiner:

```haskell
intDET The' p q = (not $ null plist) && singleton plist && q (maxElement plist)
    where plist = filter p domain
          singleton [x] = True
          singleton xs  = all (\x -> x `ipart` (maxElement xs)) xs
```

For singular entities, which are represented by a plural type with a list containing one entity, the "The" determiner is only true if there is only
one singular entity in the current domain. For plural entities and mass entities, I modify this by checking whether every smaller entity is an i-part or 
m-part of largest entity (as defined by which entity is the result of joining the most singular entities/portions of matter together). I have to check if plist
is not null so that I don't run into an empty list exception if there is no entity in the domain that satisfies the predicate p.

```haskell
size' :: Entity -> Int
size' (Pl' (Plural' xs)) = length xs
size' (Ms' (Mass' xs))   = length xs

maxElement xss = maximumBy (compare `on` size') xss
```

For the interpretation of determiners, finding this max element by comparing the length of the list of atoms or portions of matter a 
<code>Plural</code> or <code>Mass</code> type has. This implementation choice could have issues with representing collective predicates like "coven", but
I try to handle that in how I defined my predicates in <code>src/Model.hs</code> to handle collective or distributive behavior.

```haskell
intMCN :: MCN -> OnePlacePred
intMCN mcn = case mcn of 
    Gold_ -> gold'
        
intCN :: CN -> OnePlacePred
intCN (Sng scn) = (intSCN scn)
intCN (Pl pcn)  = intPCN pcn
intCN (Ms mcn)  = intMCN mcn
```
I modified my interpretation of nouns to handle the three different types of nouns and added a new interpretation function for mass terms.
<code>gold'</code> is different from the <code>gold</code> predicate in that it is only true for mass entities. These two predicats denote
the difference between something being a portion of gold and an individual entity having the quality of being gold, i.e. "the gold" vs.
"the gold ring". 

```haskell
intRCN :: RCN -> OnePlacePred
intRCN (RCN1 cn That vp)        = \ e -> ((intCN cn e) && (intVP vp e))
intRCN (RCN2 cn That dp tv)     = \ e -> ((intCN cn e) && (intDP dp (\ subj -> (intTV tv subj e))))
intRCN (RCN3 adj cn That vp)    = \ e -> ( ((intADJ adj) (intCN cn) e) && (intVP vp e))
intRCN (RCN4 adj cn That dp tv) = \ e -> ( ((intADJ adj) (intCN cn) e) && (intDP dp (\ subj -> (intTV tv subj e))))
```
I utilize partial function application and lambda functions to implement the interpretation of verb phrases in an <code>RCN</code>.
I connect the interpretation of the noun with the verb phrase with a logical and. <code>intADJ</code> has type <code>ADJ -> OnePlacePred -> OnePlacePred</code>
because an adjective modifies the interpretation of a noun.

```haskell
intSCN :: SCN -> OnePlacePred
intSCN scn = case scn of
    Bottle   -> f bottle
    Man      -> f man 
    Woman    -> f woman
    Boy      -> f boy
    Girl     -> f girl
    Witch    -> f witch
    Wizard   -> f wizard
    Giant    -> f giant
    Dwarf    -> f dwarf
    Warrior  -> f warrior
    Sword    -> f sword
    Ring     -> f ring
    Person   -> f person
    Thing    -> f thing
    Group    -> intCCN Group
    Crowd    -> intCCN Crowd
    Couple   -> intCCN Couple
    Coven    -> intCCN Coven
    where f p = atom `compose` p

intCCN :: SCN -> OnePlacePred
intCCN ccn = case ccn of
    Group    -> group'
    Crowd    -> crowd
    Couple   -> couple
    Coven    -> coven
```
Some nouns which are syntactically singular can have a collective interpretation for their semantics. I handle this with a case for such collective nouns
to be interpretated by my <code>intCCN</code> function. Collective predicates are defined using a <code>groupPred :: Ordering -> Int -> Entity -> Bool</code>
function in <code>src/Model.hs</code> which checks how many atoms a plural object has. If a mass entity is given, the result is always <code>False</code>.

```haskell
groupPred :: Ordering -> Int -> Entity -> Bool
groupPred ord n = f
    where f (Pl' (Plural' xs)) = groupPred' xs  
          f (Ms' y) = False
          groupPred' = \x -> (length x) `compare` n == ord
```

### Parsing
I implemented parsing of a fragment of English using parser combinators with the help of Haskell's 
Text.ParserCombinators.ReadP library. Parsers are of <code>ReadP a</code> type and I use the <code>readP_to_S</code>
function to output a list of tuples of a parse and an unparsed leftover string given a parser and a string. ReadP is a monad so I make
heavy use of do notation to write cleaner code for my parsers. 

```haskell
sent :: ReadP Sent
sent = do
    dp1 <- dp
    skipSpaces
    vp1 <- vp
    return (Sent dp1 vp1)
```
Parsers can be combined to form more complex parsers and I follow the definitions of my syntax types in <code>src/EF2synShort.hs</code> to guide
how write my parsers. Note that in the code above, the sentence parser is defined using the determiner phrase parser <code>dp</code> and the verb phrase
parser <code>vp</code>. I build the final parsed <code>Sent</code> type using the type constructor and <code>return</code>. <code>skipSpaces</code> just
accepts an arbitrary number of whitespace characters. 

Here is an example of how I defined the parsing of a determiner phrase that has "The":

```haskell
the' :: ReadP DP
the' = do
    string "the"
    skipSpaces
    dp <- (fmap The1 cn) <|> (fmap The4 mcn) <|> (fmap The3 rcn)
    return dp

the_adj' :: ReadP DP
the_adj' = do
    string "the"
    skipSpaces
    ad <- adj
    skipSpaces
    dp <- (fmap (The2 ad) cn)
    return dp

detThe :: ReadP DP
detThe = the' <|> the_adj'
```

I build up the final parser using smaller parsers that handle the different cases of how a determiner phrase using "The" can be constructed.
The <code><|></code> operator allows for defining alternative parses, both the left and right side of the operator are tried. <code>fmap</code> is
another handy operator that I use to essentially compose the result of parse with a type constructor. The <code>fmap</code> function lifts the type
constructor into the <code>ReadP</code> monad context so that it can be composed with the result of a parser function. I use these two operators heavily
as can be seen by my definition of <code>the'</code> to handle the different cases of constructing a determiner phrase using "The".

```haskell
helperParsers :: (a -> String) -> [a] -> [ReadP a]
helperParsers f xs = map (\x -> (string $ f x) >> return x) xs

-- takes advantage of [minBound..maxBound] to enumerate all
-- values of a Bounded, Enum type
enumParsers :: (Bounded a, Enum a, Show a) => [ReadP a]
enumParsers =  helperParsers lowerShow [minBound..maxBound]
    where lowerShow = (\x -> filter (not . (`elem` "_")) (map toLower (show x)))

-- this is for types with represented as capitalized strings, i.e. Names
enumParsers' :: (Bounded a, Enum a, Show a) => [ReadP a]
enumParsers' = helperParsers show [minBound..maxBound]

name :: ReadP Name
name = choice enumParsers'
```

I take advantage of many of my syntax types deriving <code>Bounded</code> and <code>Enum</code> to construct helper functions that will enumerate
all the possible parsers for a given type. <code>choice</code> is a function that works similar to <code><|></code>, it tries to use every parser in the
list given to it to parse a string. I take advantage of my syntax types deriving <code>Show</code> to help create a string representation of that type.
For example, I construct a function <code>pluralize</code> to output the string representation of a <code>PCN</code> type:

```haskell
plurals :: [PCN]
plurals = (map Plur singulars)

plualize :: PCN -> String
plualize (Plur x) = case x of
    Man   -> "Men"
    Woman -> "Women"
    Witch -> "Witches"
    Dwarf -> "Dwarves"
    _     -> (show x) ++ "s"

pluralParsers :: [ReadP PCN]
pluralParsers = helperParsers (\x -> map toLower (plualize x)) plurals

pcn :: ReadP PCN
pcn = choice pluralParsers
```

Just like with my parser for names, I use my <code>helperParsers</code> function to construct a list of parsers to parse all the values for <code>PCN</code>.

Finally I have an eval function as defined below to take a string containing an English sentence, parse it into a <code>Sent</code>, and interpret it.

```haskell
-- Find full parses and evaluate them using intSent
-- Empty list means no full parse (ill-formed sentence)
eval :: String -> [Bool]
eval str = map (\x -> intSent $ fst x) parses'
    where parses = readP_to_S sent str
          parses' = filter fullyParsed parses
          fullyParsed pair = length (snd pair) == 0
```

One note is that I filter out any parses with leftover characters because they are mostly likely incorrect parses of the sentence string.


### Possible Future Work
There is most likely a lot of refactoring I can do to make shorter, cleaner code. In some places I tried to balance terseness of code with readability, like
with how I defined my parsers for my determiners. Much of the verbose code was due to the constraint of wanting working code in a (somewhat) 
reasonable timeframe. My implementation of the domain of individuals as a powerset (i.e. a list of lists) is probably not the most efficient choice and
for more than about 13 atoms my interpretation functions take an unreasonable amount of time to run. There are also probably more clever ways of adding
mass entities to my domain. The tricky issue with mass entities is that they form a possibly non-atomic semillatice so they can not be enumerated like
my plural entities can be. Perphaps there is a way to utilize the lazy evaluation of Haskell to get around this issue.


#### References
- [ReadP on Hackage](https://hackage.haskell.org/package/base-4.12.0.0/docs/Text-ParserCombinators-ReadP.html)
- [Two Wrongs' blog post](https://two-wrongs.com/parser-combinators-parsing-for-haskell-beginners.html)
- Starter code provided by Professor Grimm
- Link paper


