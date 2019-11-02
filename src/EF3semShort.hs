module EF3semShort where
    
import Data.List 
import Data.Function
import EF2synShort
import Model

size' :: Entity -> Int
size' (Pl' (Plural' xs)) = length xs
size' (Ms' (Mass' xs))   = length xs

maxElement xss = maximumBy (compare `on` size') xss
noniParts xss = filter (\x -> not (x `ipart` (maxElement xss) )) xss
    
intName :: Name -> OnePlacePred -> Bool
intName x = case x of
    Alice      -> f Alice'
    Bob        -> f Bob'
    Cyrus      -> f Cyrus'
    Ellie      -> f Ellie'
    Irene      -> f Irene'   
    Ollie      -> f Ollie'
    Quine      -> f Quine'
    SnowWhite  -> f SnowWhite'
    Harry      -> f Harry'
    Xena       -> f Xena'
    where f x = \p -> p (Pl' (Plural' [x]))
    
intADJ :: ADJ -> OnePlacePred -> OnePlacePred
intADJ adj = case adj of
    Wise        -> f wise
    Foolish     -> f foolish
    Bad         -> f bad
    Good        -> f good
    New         -> f new
    Young       -> f young
    Old         -> f old
    Rusty       -> f rusty
    Clean       -> f clean
    Dirty       -> f dirty
    Cold        -> f cold
    Magical     -> f magical
    Short       -> f short
    Tall        -> f tall
    Metal       -> f metal
    Male        -> f male
    Female      -> f female
    Numerous    -> f numerous
    Gold        -> f gold
    where f p = compose p

intADJ' :: ADJ -> OnePlacePred
intADJ' adj = case adj of
    Wise        -> f wise
    Foolish     -> f foolish
    Bad         -> f bad
    Good        -> f good
    New         -> f new
    Young       -> f young
    Old         -> f old
    Rusty       -> f rusty
    Clean       -> f clean
    Dirty       -> f dirty
    Cold        -> f cold
    Magical     -> f magical
    Short       -> f short
    Tall        -> f tall
    Metal       -> f metal
    Male        -> f male
    Female      -> f female
    Numerous    -> f numerous
    Gold        -> gold
    where f p = \x -> p x
    
intSCN :: SCN -> OnePlacePred
intSCN scn = case scn of
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

intPCN :: PCN -> OnePlacePred
intPCN (Plur scn) = case scn of
    Man      -> man 
    Woman    -> woman
    Boy      -> boy
    Girl     -> girl
    Witch    -> witch
    Wizard   -> wizard
    Giant    -> giant
    Dwarf    -> dwarf
    Warrior  -> warrior
    Sword    -> sword
    Ring     -> ring
    Person   -> person
    Thing    -> thing
    Group    -> group'
    Crowd    -> crowd
    Couple   -> couple
    Coven    -> coven


intMCN :: MCN -> OnePlacePred
intMCN mcn = case mcn of 
    Gold_ -> gold'
        
intCN :: CN -> OnePlacePred
intCN (Sng scn) = (intSCN scn)
intCN (Pl pcn)  = intPCN pcn
intCN (Ms mcn)  = intMCN mcn

-- Based on starter code by Professor Grimm
intDET :: DET' -> (Entity -> Bool) -> (Entity -> Bool) -> Bool
intDET All' p q = all q (filter p domain) 
intDET Each' p q = all q (filter p domain) 
intDET Some' p q = any q (filter p domain)
intDET A' p q = any q (filter p domain)
intDET Every' p q = all q (filter p domain)
intDET The' p q = singleton plist && q (maxElement plist)
    where plist = filter p domain
          singleton [x] = True
          singleton xs  = all (\x -> x `ipart` (maxElement xs)) xs

intDET No' p q = not (intDET Some' p q)
intDET Most' p q = length pqlist > length (plist \\ qlist)
    where plist  = filter p domain
          qlist  = filter q domain
          pqlist = filter q plist

intDP :: DP -> OnePlacePred -> Bool
intDP (Empty name) = intName name
intDP (Some1 pcn) = (intDET Some') (intPCN pcn)
intDP (Some2 adj pcn) = (intDET Some') ((intADJ adj) (intPCN pcn))
intDP (Some3 rpcn)    = (intDET Some') (intRPCN rpcn)
intDP (Some4 mcn)    = (intDET Some') (intMCN mcn)

intDP (Each1 scn) = (intDET Each') (intSCN scn)
intDP (Each2 adj scn) = (intDET Each') ((intADJ adj) (intSCN scn))
intDP (Each3 rscn)    = (intDET Each') (intRSCN rscn)

intDP (Every1 scn) = (intDET Every') (intSCN scn)
intDP (Every2 adj scn) = (intDET Every') ((intADJ adj) (intSCN scn))
intDP (Every3 rscn)    = (intDET Every') (intRSCN rscn)

intDP (Most1 pcn) = (intDET Most') (intPCN pcn)
intDP (Most2 adj pcn) = (intDET Most') ((intADJ adj) (intPCN pcn))
intDP (Most3 rpcn)    = (intDET Most') (intRPCN rpcn)
intDP (Most4 mcn)    = (intDET Most') (intMCN mcn)

intDP (The1 cn) = (intDET The') (intCN cn)
intDP (The2 adj cn) = (intDET The') ((intADJ adj) (intCN cn))
intDP (The3 rcn)    = (intDET The') (intRCN rcn)
intDP (The4 mcn)    = (intDET The') (intMCN mcn)

intDP (A1 scn) = (intDET A') (intSCN scn)
intDP (A2 adj scn) = (intDET A') ((intADJ adj) (intSCN scn))
intDP (A3 rscn)    = (intDET A') (intRSCN rscn)

intDP (All1 pcn) = (intDET All') (intPCN pcn)
intDP (All2 adj pcn) = (intDET All') ((intADJ adj) (intPCN pcn))
intDP (All3 rpcn)    = (intDET All') (intRPCN rpcn)
intDP (All4 mcn)     = (intDET All') (intMCN mcn)

intDP (No1 cn) = (intDET No') (intCN cn)
intDP (No2 adj cn) = (intDET No') ((intADJ adj) (intCN cn))
intDP (No3 rcn)    = (intDET No') (intRCN rcn)
intDP (No4 mcn)    = (intDET No') (intMCN mcn)

intINF :: INF -> OnePlacePred
intINF x = case x of
    Laugh   -> laugh
    Smile   -> smile
    Swim    -> swim
    Run     -> run
    Walk    -> walk
    Scatter -> scatter
    Gather  -> gather

intTV :: TV -> TwoPlacePred
intTV Help   = \x y -> help x y
intTV Defeat = \x y -> defeat x y
intTV Chase  = \x y -> chase x y
intTV Fight  = \x y -> fight x y

intDV :: DV -> ThreePlacePred
intDV Give = give

intVP :: VP -> OnePlacePred
intVP (VP0 inf)        = (intINF inf)
intVP (VP1 tv dp)      = \ subj -> intDP dp (\ obj -> intTV tv obj subj)
intVP (VP2 dv dp1 dp2) = \ subj -> intDP dp1 (\ dobj -> intDP dp2 (\ iobj -> intDV dv iobj dobj subj))
intVP (VP3 Be adj)     = intADJ' adj

intSent :: Sent -> Bool
intSent (Sent dp vp) = (intDP dp) (intVP vp)

intRCN :: RCN -> OnePlacePred
intRCN (RCN1 cn That vp)        = \ e -> ((intCN cn e) && (intVP vp e))
intRCN (RCN2 cn That dp tv)     = \ e -> ((intCN cn e) && (intDP dp (\ subj -> (intTV tv e subj))))
intRCN (RCN3 adj cn That vp)    = \ e -> ( ((intADJ adj) (intCN cn) e) && (intVP vp e))
intRCN (RCN4 adj cn That dp tv) = \ e -> ( ((intADJ adj) (intCN cn) e) && (intDP dp (\ subj -> (intTV tv e subj))))

intRSCN :: RSCN -> OnePlacePred
intRSCN (RSCN1 scn That vp)        = \ e -> ((intSCN scn e) && (intVP vp e))
intRSCN (RSCN2 scn That dp tv)     = \ e -> ((intSCN scn e) && (intDP dp (\ subj -> (intTV tv e subj))))
intRSCN (RSCN3 adj scn That vp)    = \ e -> ( ((intADJ adj) (intSCN scn) e) && (intVP vp e))
intRSCN (RSCN4 adj scn That dp tv) = \ e -> ( ((intADJ adj) (intSCN scn) e) && (intDP dp (\ subj -> (intTV tv e subj))))

intRPCN :: RPCN -> OnePlacePred
intRPCN (RPCN1 pcn That vp)        = \ e -> ((intPCN pcn e) && (intVP vp e))
intRPCN (RPCN2 pcn That dp tv)     = \ e -> ((intPCN pcn e) && (intDP dp (\ subj -> (intTV tv e subj))))
intRPCN (RPCN3 adj pcn That vp)    = \ e -> ( ((intADJ adj) (intPCN pcn) e) && (intVP vp e))
intRPCN (RPCN4 adj pcn That dp tv) = \ e -> ( ((intADJ adj) (intPCN pcn) e) && (intDP dp (\ subj -> (intTV tv e subj))))