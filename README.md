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



### Parsing
I implemented parsing of a fragment of English using parser combinators with the help of Haskell's 
Text.ParserCombinators.ReadP library.


