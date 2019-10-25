module EF3semShort where
    
import Data.List 
import EF2synShort
import Model
    
allNum, noNum :: Int -> Int -> Bool
allNum = \ m n -> m == 0
noNum  = \ m n -> n == 0
    
atleastNum, atmostNum :: Int -> Int -> Int -> Bool
atleastNum k = \ m n -> n >= k
atmostNum  k = \ m n -> n <= k
    
atleast2butnotall :: Int -> Int -> Bool
atleast2butnotall = \ m n -> m > 0 && n >= 2
    
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
    where f x = \p -> p [x]
    
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
    where f p = compose p
    
intSCN :: SCN -> OnePlacePred
intSCN scn = case scn of
    Man      -> man 
    Woman    -> woman
    Boy      -> boy
    Girl     -> girl
    Witch    -> witch
    Wizard   -> wizard
    Giant    -> giant
    Dwarf    -> dwarf
    Someone  -> person
    Sword    -> sword
    Ring     -> ring
    Person   -> person

intPCN :: PCN -> OnePlacePred -> Bool
intPCN (Plur scn) = plural `compose` (intSCN scn)
        
intCN :: CN -> OnePlacePred -> Bool
intCN (Sng scn) = intSCN scn
intCN (Pl pcn)   = intPCN pcn

intDET :: DET' -> (Entity -> Bool) -> (Entity -> Bool) -> Bool
intDET Some' p q = any q (filter p domain)
intDET A' p q = any q (filter p domain)
intDET Every' p q = all q (filter p domain)
intDET The' p q = singleton plist && q (head plist)
    where plist = filter p domain
          singleton [x] = True
          singleton  _  = False
intDET No' p q = not (intDET Some' p q)
intDET Most' p q = length pqlist > length (plist \\ qlist)
    where plist  = filter p domain
          qlist  = filter q domain
          pqlist = filter q plist

intDP :: DP -> OnePlacePred -> Bool
intDP (Empty name) = intName name
intDP (Some1 pcn) = (intDET Some') (intPCN pcn)
intDP (Some2 adj pcn) = (intDET Some') ((intADJ adj) (intPCN pcn))
intDP (Many1 pcn) = (intDET Many') (intPCN pcn)
intDP (Many2 adj pcn) = (intDET Many') ((intADJ adj) (intPCN pcn))
intDP (Each1 scn) = (intDET Each') (intSCN scn)
intDP (Each2 adj scn) = (intDET Each') ((intADJ adj) (intSCN scn))
intDP (Every1 scn) = (intDET Every') (intSCN scn)
intDP (Every2 adj scn) = (intDET Every') ((intADJ adj) (intSCN scn))
intDP (Most1 pcn) = (intDET Most') (intPCN pcn)
intDP (Most2 adj pcn) = (intDET Most') ((intADJ adj) (intPCN pcn))
intDP (The1 cn) = (intDET The') (intCN cn)
intDP (The2 adj cn) = (intDET The') ((intADJ adj) (intCN cn))
intDP (A1 scn) = (intDET A') (intSCN scn)
intDP (A2 adj scn) = (intDET A') ((intADJ adj) (intSCN scn))
intDP (All1 pcn) = (intDET All') (intPCN pcn)
intDP (All2 adj pcn) = (intDET All') ((intADJ adj) (intPCN pcn))
intDP (No1 scn) = (intDET No') (intsCN scn)
intDP (No2 adj scn) = (intDET No') ((intADJ adj) (intSCN scn))

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
intTV Help   = help
intTV Defeat = defeat
intTV Chase  = chase
intTV Fight  = fight

intDV :: DV -> ThreePlacePred
intDV Give = give

-- VP0 INF | VP1 TV DP | VP2 DV DP DP | VP3 Be' ADJ 
intVP :: OnePlacePred
intVP (VP0 inf)        = (intINF inf)
intVP (VP1 tv dp)      = \ subj -> intDP dp (\ obj -> intDV tv obj subj)
intVP (VP2 dv dp1 dp2) = \ subj -> intDP dp1 (\ dobj -> intDP dp2 (\ iobj -> intDV dv iobj dobj subj))
--intVP (VP3 Be adj)     = intADJ adj

intSent :: Sent -> Bool
intSent (Sent dp vp) = (intDP dp) (intVP vp)

{-
intRCN :: RCN -> OnePlacePred
intRCN (RCN1 cn _ vp) = \ e -> ((intCN cn e) && (intVP vp e))
intRCN (RCN2 cn _ dp tv) = \ e -> ((intCN cn e) && (intDP dp (\ subj -> (intTV tv e subj))))
-}