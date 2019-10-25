module EF3semShort where
    
import Data.List (intersect)
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
intPCN (Plur scn) = 
        
intCN :: CN -> OnePlacePred -> Bool
intCN (Sing scn) = intSCN scn
intCN (Pl pcn)   = intPCN pcn

intDET :: DET -> (Entity -> Bool) -> (Entity -> Bool) -> Bool
intDET Some p q = any q (filter p atoms)
intDET A p q = any q (filter p atoms)
intDET Every p q = all q (filter p atoms)
intDET The p q = singleton plist && q (head plist)
    where plist = filter p atoms
          singleton [x] = True
          singleton  _  = False
intDET No p q = not (intDET Some p q)
intDET Most p q = length pqlist > length (plist \\ qlist)
    where plist  = filter p atoms
          qlist  = filter q atoms
          pqlist = filter q plist

intDP :: DP -> OnePlacePred -> Bool
intDP (Empty name) = intName name
intDP (Some pcn) = (intDET Some) (intPCN pcn)
intDP (Some' adj pcn) = (intDET Some) ((intADJ adj) (intPCN pcn))
intDP (Many pcn) = (intDET Many) (intPCN pcn)
intDP (Many' adj pcn) = (intDET Many) ((intADJ adj) (intPCN pcn))
intDP (Each scn) = (intDET Each) (intSCN scn)
intDP (Each' adj scn) = (intDET Each) ((intADJ adj) (intSCN scn))
intDP (Every scn) = (intDET Every) (intSCN scn)
intDP (Every' adj scn) = (intDET Every) ((intADJ adj) (intSCN scn))
intDP (Most pcn) = (intDET Most) (intPCN pcn)
intDP (Most' adj pcn) = (intDET Most) ((intADJ adj) (intPCN pcn))
intDP (The cn) = (intDET The) (intCN cn)
intDP (The' adj cn) = (intDET The) ((intADJ adj) (intCN cn))
intDP (A scn) = (intDET A) (intSCN scn)
intDP (A' adj scn) = (intDET A) ((intADJ adj) (intSCN scn))
intDP (All pcn) = (intDET All) (intPCN pcn)
intDP (All' adj pcn) = (intDET All) ((intADJ adj) (intPCN pcn))

intSent :: Sent -> Bool
intSent (Sent dp vp) = (intDP dp) (intVP vp)