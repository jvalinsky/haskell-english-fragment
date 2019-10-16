{-# LANGUAGE TypeFamilies, AllowAmbiguousTypes, TypeOperators, RankNTypes, ConstrainedClassMethods, FlexibleContexts #-}

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

resolveName :: (Entity entity) => Name -> Either String entity
resolveName x = readEither x

evalEnt :: (Entity entity) => entity -> OnePlacePred entity -> Bool
evalEnt x = \ p -> p x

intName :: (Entity entity) => Name -> OnePlacePred entity -> Bool
intName x = case e of
                Left  s -> \x -> False
                Right f -> f
    where e = liftM evalEnt $ resolveName x


intPronoun :: (Entity entity) => Pronoun -> OnePlacePred entity -> Bool
intPronoun y = case y of
                He   -> \p -> any (p `compose` male) entities
                She  -> \p -> any (p `compose` female) entities
                They -> \p -> True
                It   -> \p -> any (p `compose` thing) entities

intADJ :: ADJ -> OnePlacePred entity -> Bool
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

intSCN :: SCN -> OnePlacePred entity -> Bool
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

intPCN :: PCN -> OnePlacePred entity -> Bool
intPCN Glasses  = helper glasses
intPCN Jeans    = helper jeans
intPCN (PS scn) =
intPCN (PC ccn) =

intMCN :: MCN -> OnePlacePred entity -> Bool
intMCN mcn = 

intCCN :: CCN -> OnePlacePred entity -> Bool
intCCN ccn = 

intRCN :: CN -> OnePlacePred entity -> Bool
intRCN rcn =

intCN :: CN -> OnePlacePred entity -> Bool
intCN (Sing scn) = intSCN scn
intCN (Pl pcn)   = intPCN pcn
intCN (Mass mcn) = intMCN mcn
intCN (Col ccn)  = intCCN ccn

intDET :: DET -> OnePlacePred entity -> OnePlacePred entity -> Bool
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

intDetP :: (Entity entity) => DetP -> OnePlacePred entity -> Bool
--intDetP (DP0 pronoun) = intPronoun pronoun
intDetP (DP1 name)    = intName name
intDetP (DP2 det cn)  = (intDET det) (intCN cn)
intDetP (DP3 det rcn) = (intDET det) (intRCN rcn)

intINF :: INF -> OnePlacePred entity

intTV :: TV -> TwoPlacePred entity -> Bool

intAV :: AV -> 

intDV :: DV -> ThreePlacePred entity -> Bool

intAux :: Aux -> 

intVP :: VP -> OnePlacePred entity -> Bool
intVP (VP0 inf)       =
intVP (VP1 tv dp)     =
intVP (VP3 av To inf) = 
intVP (VP4 auxV inf)  =
intVP (VP5 AuxV TV)   = 
intVP (VP6 auxV dv)   =
intVP (VP7 auxV av)   = 
intVP (VP8 auxV)      =

intSent :: Sent -> Bool
intSent (Sent dp vp) = (intDetP dp) (intVP vp)