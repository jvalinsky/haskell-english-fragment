module EF2semShort where

import Data.List
import EF1synShort
import Model

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

intSent :: Sent -> Bool
intSent (Sent np vp) = (intNP np) (intVP vp)

intNP :: NP -> (Entity -> Bool) -> Bool
intNP SNOWWHITE     = \ p -> p SnowWhite
intNP ALICE         = \ p -> p Alice
intNP DOROTHY       = \ p -> p Dorothy
intNP GOLDILOCKS    = \ p -> p Goldilocks
intNP LITTLEMOOK    = \ p -> p LittleMook
intNP ATREYU        = \ p -> p Atreyu
intNP (NP1 det cn)  = (intDET det) (intCN cn)
intNP (NP2 det rcn) = (intDET det) (intRCN rcn)

intCN :: CN -> Entity -> Bool
intCN Girl     = \ x -> girl x
intCN Boy      = \ x -> boy x
intCN Princess = \ x -> princess x
intCN Dwarf    = \ x -> dwarf x
intCN Giant    = \ x -> giant x
intCN Wizard   = \ x -> wizard x
intCN Sword    = \ x -> sword x
intCN Dagger   = \ x -> dagger x

intTV :: TV -> Entity -> Entity -> Bool
intTV Loved    = \ y x -> love y x
intTV Admired  = \ y x -> admire y x
intTV Helped   = \ y x -> help y x
intTV Defeated = \ y x -> defeat y x


intVP :: VP -> Entity -> Bool
intVP Laughed   = \ x -> laugh x
intVP Cheered   = \ x -> cheer x
intVP Shuddered = \ x -> shudder x
intVP Smiled    = \ x -> smile x
intVP (VP1 tv np) = \ subj -> intNP np (\ obj -> intTV tv obj subj)


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

intRCN :: RCN -> Entity -> Bool
intRCN (RCN1 cn _ vp) = \ e -> ((intCN cn e) && (intVP vp e))
intRCN (RCN2 cn _ np tv) = \ e -> ((intCN cn e) && (intNP np (\ subj -> (intTV tv e subj))))
