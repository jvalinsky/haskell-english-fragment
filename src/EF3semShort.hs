module EF3semShort where

import Data.Either
import Data.List (intersect)
import Control.Monad
import Text.Read
import EF2synShort
import Model

compose' x y =  (filter x entities) `intersect` (filter y entities) /= []

resolveName :: Name -> Either String Entity
resolveName x = readEither x

evalEnt :: Entity -> (Entity -> Bool) -> Bool
evalEnt x = \ p -> p x

false' :: (Entity -> Bool) -> Bool
false' p = False

intName :: Name -> (Entity -> Bool) -> Bool
intName x = case e of
                Left s -> false'
                Right f -> f
    where e = liftM evalEnt $ resolveName x


intPronoun :: Pronoun -> (Entity -> Bool) -> Bool
intPronoun y = case y of
                He   -> compose' male
                She  -> compose' female
                It   -> compose' thing
                They -> compose' neutral

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


{-
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
-}

intCN :: CN -> (Entity -> Bool) -> Bool

intMCN :: MCN -> (Entity -> Bool) -> Bool

intSCN :: SCN -> (Entity -> Bool) -> Bool

intPCN :: CN -> (Entity -> Bool) -> Bool

intRCN :: CN -> (Entity -> Bool) -> Bool

intADJ :: ADJ -> (Entity -> Bool) -> Bool

intNP :: NP -> (Entity -> Bool) -> Bool
intNP (NP1 name) = intName name
intNP (NP0 pronoun) = intPronoun pronoun
intNP (NP2 det cn) = (intDET det) (intCN cn)
intNP (NP3 DET rcn) = (intDET det) (intRCN rcn)

{-
intVP :: VP -> (Entity -> Bool) -> Bool
intVP (VP0 INF)   =
intVP (VP1 TV NP) =   | VP3 AV To INF | VP4 AuxV INF |
VP5 AuxV TV | VP6 AuxV DV | VP7 AuxV AV   | VP8 AuxV
-}

intSent :: Sent -> Bool
intSent (Sent np vp) = (intNP np) (intVP vp)