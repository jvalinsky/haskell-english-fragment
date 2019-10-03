module EF3semShort where

import EF2synShort

intNP :: NP -> (Entity -> Bool) -> Bool
intNP NP1 x         = \ p -> p 
intNP (NP2 det cn)  = (intDET det) (intCN cn)
intNP (NP3 det rcn) = (intDET det) (intRCN rcn)

intCN :: CN -> Entity -> Bool
intCN Girl     = \ x -> girl x
intCN Boy      = \ x -> boy x
intCN Princess = \ x -> princess x
intCN Dwarf    = \ x -> dwarf x
intCN Giant    = \ x -> giant x
intCN Wizard   = \ x -> wizard x
intCN Sword    = \ x -> sword x
intCN Dagger   = \ x -> dagger x

