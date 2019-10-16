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

{-
intPronoun :: (AtomicEntity entity) => Pronoun -> OnePlacePred entity -> Bool
intPronoun y = case y of
                He   -> \p -> any (p `compose` male) entities
                She  -> \p -> any (p `compose` female) entities
                They -> \p -> True
                It   -> \p -> any (p `compose` thing) entities

intDET :: DET -> (Entity -> Bool) -> Bool
intDET Each = 
intDET Every = 
intDET The = 
intDET A = 
intDET No = 
intDET Neither = 
intDET Either = 
intDET Some = 
intDET Few = 
intDET Most =

    intDET :: DET -> (Entity -> Bool) -> (Entity -> Bool) -> Bool
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

intCN :: CN -> (Entity -> Bool) -> Bool

intMCN :: MCN -> (Entity -> Bool) -> Bool

intSCN :: SCN -> (Entity -> Bool) -> Bool

intPCN :: CN -> (Entity -> Bool) -> Bool

intRCN :: CN -> (Entity -> Bool) -> Bool

intADJ :: ADJ -> (Entity -> Bool) -> Bool
-}

{-
data SCN = Someone  | Belief | Man    | Woman | Hero  | Heroine  | Sword | Drop |
           Fork     | Spoon  | Knife  | Witch | Boy   | Girl     | Dwarf | Prince |
           Princess | Giant  | Wizard | Mouse | Cat   | Dress    | Shoe  | Bird |
           Spy deriving (Show, Bounded, Enum)
-}

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
    where helper q = \p -> p any (p `compose` q)


intPCN :: PCN -> OnePlacePred entity -> Bool
intPCN pcn = 

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
intDET det =

intDetP :: (Entity entity) => DetP -> OnePlacePred entity -> Bool
--intDetP (DP0 pronoun) = intPronoun pronoun
intDetP (DP1 name)   = intName name
intDetP (DP2 det cn) = (intDET det) (intCN cn)
--intNP (NP3 DET rcn) = (intDET det) (intRCN rcn)


intVP :: VP -> OnePlacePred entity -> Bool
intVP vp =


intSent :: Sent -> Bool
intSent (Sent dp vp) = (intDetP dp) (intVP vp)

{-
intVP :: VP -> (Entity -> Bool) -> Bool
intVP (VP0 INF)   =
intVP (VP1 TV NP) =   | VP3 AV To INF | VP4 AuxV INF |
VP5 AuxV TV | VP6 AuxV DV | VP7 AuxV AV   | VP8 AuxV
-}
