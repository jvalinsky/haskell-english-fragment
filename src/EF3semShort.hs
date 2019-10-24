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
    Goldilocks -> f Goldilocks'
    Hillary    -> f Hillary'
    Irene      -> f Irene'   
    Jim        -> f Jim'
    Kim        -> f Kim'
    Linda      -> f Linda'
    Noah       -> f Noah'
    Ollie      -> f Ollie'
    Penny      -> f Penny'
    Quine      -> f Quine'
    SnowWhite  -> f SnowWhite'
    Tom        -> f Tom'
    Uli        -> f Uli'
    Victor     -> f Victor'
    Harry      -> f Harry'
    Xena       -> f Xena'
    Zorba      -> f Zorba'
    Dorothy    -> f Dorothy'
    Fred       -> f Fred'
    Whiskers   -> f Whiskers'
    Mittens    -> f Mittens'
    Stuart     -> f Stuart'
    Gerald     -> f Gerald'
    Minnie     -> f Minnie'
    Mickey     -> Mickey'
    Sue        -> Sue'
    where f x = \p -> p x
    
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
    Shiny       -> f shiny
    Metal       -> f metal
    Gold        -> f gold
    Silver      -> f silver
    Steel       -> f steel
    Male        -> f male
    Female      -> f female
    Torn        -> f torn
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
    Mouse    -> mouse
    Cat      -> cat
    Bird     -> bird
    Dress    -> dress
    Someone  -> person
    Cup      -> cup
    Sword    -> sword
    Ring     -> ring
    Person   -> person

intPCN :: PCN -> OnePlacePred -> Bool
intPCN Glasses  = f glasses
    where f p = compose p

intRCN :: CN -> OnePlacePred -> Bool
intRCN (RCN1 That vp) =
intRCN (RCN2 That detp tv) = 
        
intCN :: CN -> OnePlacePred -> Bool
intCN (Sing scn) = intSCN scn
intCN (Pl pcn)   = intPCN pcn

intDET :: DET -> OnePlacePred -> OnePlacePred -> Bool
intDET Some p q = 
intDET Many p q = 
intDET Each p q = 
intDET Every p q = 
intDET Most p q = 
intDET The p q = 
intDET A p q = 
intDET All p q = 


intDetP :: DetP -> OnePlacePred -> Bool
intDetP (Empty name) = intName name
intDetP (Some pcn) = (intDET Some) (intPCN pcn)
intDetP (Some' adj pcn) = (intDET Some) ((intADJ adj) (intPCN pcn))
intDetP (Many pcn) = (intDET Many) (intPCN pcn)
intDetP (Many' adj pcn) = (intDET Many) ((intADJ adj) (intPCN pcn))
intDetP (Each scn) = (intDET Each) (intSCN scn)
intDetP (Each' adj scn) = (intDET Each) ((intADJ adj) (intSCN scn))
intDetP (Every scn) = (intDET Every) (intSCN scn)
intDetP (Every' adj scn) = (intDET Every) ((intADJ adj) (intSCN scn))
intDetP (Most pcn) = (intDET Most) (intPCN pcn)
intDetP (Most' adj pcn) = (intDET Most) ((intADJ adj) (intPCN pcn))
intDetP (The cn) = (intDET The) (intCN cn)
intDetP (The' adj cn) = (intDET The) ((intADJ adj) (intCN cn))
intDetP (A scn) = (intDET A) (intSCN scn)
intDetP (A' adj scn) = (intDET A) ((intADJ adj) (intSCN scn))
intDetP (All pcn) = (intDET All) (intPCN pcn)
intDetP (All' adj pcn) = (intDET All) ((intADJ adj) (intPCN pcn))

intDP :: DP -> OnePlacePred -> Bool
intDP (DP  dp) = (intDetP dp)
intDP (DP' dp rcn) = (intDetP dp) (intRCN rcn)

intSent :: Sent -> Bool
intSent (Sent dp vp) = (intDP dp) (intVP vp)