module EF3semShort where

import Data.Either
import Data.List (intersect)
import Control.Monad
import Text.Read
import EF2synShort
import Model
import Entity

allNum, noNum :: Int -> Int -> Bool
allNum = \ m n -> m == 0
noNum  = \ m n -> n == 0

atleastNum, atmostNum :: Int -> Int -> Int -> Bool
atleastNum k = \ m n -> n >= k
atmostNum  k = \ m n -> n <= k

atleast2butnotall :: Int -> Int -> Bool
atleast2butnotall = \ m n -> m > 0 && n >= 2

uncurry2 :: (b -> a -> c) -> (a, b) -> c
uncurry2 f (x,y) =  f y x

{-
IV :: Entity -> Bool
Term :: IV -> Bool
TV :: Term -> IV
IAV :: IV -> IV
CN :: Entity -> Bool
-}

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
    Willie     -> f Willie'
    Xena       -> f Xena'
    Zorba      -> f Zorba'
    Atreyu     -> f Atreyu'
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

intADJ :: ADJ -> OnePlacePred -> Bool
intADJ adj = case adj of
    Wise        -> 
    Foolish     ->
    Bad         ->
    Good        ->
    Rich        ->
    Mellow      ->
    Discordant  ->
    Poor        ->
    Young       ->
    Old         ->
    Heavy       ->
    Light       ->
    Dark        ->
    Rusty       ->
    Clean       ->
    Dirty       ->
    Wet         ->
    Dry         ->
    Cold        ->
    Hot         ->
    Magical     ->
    Short       ->
    Long        ->
    Sharp       ->
    Dull        ->
    Shiney      ->

intSCN :: SCN -> OnePlacePred -> Bool
intSCN scn = case scn of
    Man      -> helper man
    Woman    -> helper woman
    Boy      -> helper boy
    Girl     -> helper girl
    Witch    -> helper witch
    Wizard   -> helper wizard
    Prince   -> helper prince
    Princess -> helper princess
    Giant    -> helper giant
    Dwarf    -> helper dwarf
    Mouse    -> helper mouse
    Cat      -> helper cat
    Bird     -> helper bird
    Hero     -> helper hero
    Heroine  -> helper heroine
    Fork     -> helper fork
    Spoon    -> helper spoon
    Shoe     -> helper shoe
    Dress    -> helper dress
    Spy      -> helper spy
    Belief   -> helper belief
    Someone  -> helper person

intPCN :: PCN -> OnePlacePred -> Bool
intPCN Glasses  = helper glasses
intPCN Jeans    = helper jeans
intPCN (PS scn) =
intPCN (PC ccn) =

intMCN :: MCN -> OnePlacePred -> Bool
intMCN mcn = 

intCCN :: CCN -> OnePlacePred -> Bool
intCCN ccn = 

intRCN :: CN -> OnePlacePred -> Bool
intRCN rcn =

intCN :: CN -> OnePlacePred -> Bool
intCN (Sing scn) = intSCN scn
intCN (Pl pcn)   = intPCN pcn
intCN (Mass mcn) = intMCN mcn
intCN (Col ccn)  = intCCN ccn

intDET :: DET -> OnePlacePred -> OnePlacePred -> Bool
intDET Some p q = any q (filter p entities)
intDET A p q = any q (filter p entities)
intDET Every p q = all q (filter p entities)
intDET The p q = singleton plist && q (head plist)
    where plist = filter p entities
            singleton [x] = True
            singleton  _  = False
intDET No p q = not (intDET Some p q)
intDET Most p q = length pqlist > length (plist \\ qlist)
    where plist  = filter p entities
        qlist  = filter q entities
        pqlist = filter q plist

intDetP :: DetP -> OnePlacePred -> Bool
intDetP (DP1 name)        = intName name
intDetP (DP2 det cn)      = (intDET det) (intCN cn)
intDetP (DP3 adj det cn)  = (intDET det) (intADJ adj) (intCN cn)

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