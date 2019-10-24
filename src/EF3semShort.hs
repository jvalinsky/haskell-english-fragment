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

        
intCCN :: CCN -> OnePlacePred -> Bool
intCCN ccn = 
        
intRCN :: CN -> OnePlacePred -> Bool
intRCN rcn =
        
intCN :: CN -> OnePlacePred -> Bool
intCN (Sing scn) = intSCN scn
intCN (Pl pcn)   = intPCN pcn
intCN (Col ccn)  = intCCN ccn

intDET :: DET -> OnePlacePred -> OnePlacePred -> Bool
intDET Some p q = 
intDET A p q = 
intDET Every p q = 
intDET The p q = 


intDetP :: DetP -> OnePlacePred -> Bool
intDetP (DP1 name)        = intName name
intDetP (DP2 det cn)      = (intDET det) (intCN cn)
intDetP (DP3 adj det cn)  = (intDET det) (intADJ adj) (intCN cn)

{-  
    intINF :: INF -> OnePlacePred 
    
    intTV :: TV -> TwoPlacePred -> Bool
    
    intAV :: AV -> 
    
    intDV :: DV -> ThreePlacePred -> Bool
    
    
    intVP :: VP -> OnePlacePred -> Bool
    intVP (VP0 inf)       =
    intVP (VP1 tv dp)     =
    intVP (VP3 av To inf) = 
    intVP (VP4 dv dp dp)  =
    
    intSent :: Sent -> Bool
    intSent (Sent dp vp) = (intDetP dp) (intVP vp)
-}