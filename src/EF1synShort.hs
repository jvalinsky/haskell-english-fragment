module EF1synShort where

import Data.List

data Sent = Sent NP VP deriving Show
data NP   = SNOWWHITE  | ALICE  | DOROTHY | GOLDILOCKS | LITTLEMOOK | ATREYU | Everyone | Someone | NP1 DET CN | NP2 DET RCN deriving Show
data DET  = The | Every | Some | No | Most | A deriving Show
data CN   = Girl   | Boy   | Princess | Dwarf | Giant | Wizard | Sword | Dagger deriving Show
data ADJ  = Fake deriving Show
data RCN  = RCN1 CN That VP | RCN2 CN That NP TV | RCN3 ADJ CN deriving Show
data That = That deriving Show
data VP   = Laughed | Cheered | Shuddered | Smiled | VP1 TV NP  | VP3 AV To INF deriving Show
data TV   = Loved   | Admired | Helped | Defeated | Caught deriving Show
data DV   = Gave deriving Show
data AV   = Hoped | Wanted deriving Show
data INF  = Laugh | Sheer | Shudder | INF TINF NP deriving Show
data TINF = Love | Admire | Help | Defeat | Catch deriving Show
data To   = To deriving Show
