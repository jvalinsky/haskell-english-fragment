module EF2synShort where

data Sent = Sent NP VP deriving Show
data NP   = NP0 Pronoun | NP1 Name | NP2 DET CN | NP3 DET RCN deriving Show
data CN   = Mass MCN    | Sing SCN   | Plur PCN deriving Show

data MCN  = Water    | Wood | Air    | Blood | Metal | Earth  | Foilage |
            Darkness | Clay | Envy   | Fun   | Music | Poetry | Rust    |
            Gold     | Mail | Advice | Honor | Ice   | Fabric | Confusion deriving Show

data SCN = Someone  | Belief | Man    | Woman | Hero  | Heroine  | Sword | 
           Fork     | Spoon  | Knife  | Witch | Boy   | Girl     | Dwarf | Prince |
           Princess | Giant  | Wizard | Dog   | Cat   | Dress    | Shoe  | Bee    | Bird |
           Spy      | Coven  | Court  | Horde | Cabal | Commitee | Pair  | Army   | Pack |
           Hive     | Nest   | Crowd  | Group deriving Show

data PCN = Pl SCN | Glasses | Jeans deriving Show

data RCN = RCN1 CN That VP | RCN2 CN That NP TV | RCN3 ADJ CN deriving Show

data Pronoun = He | She | They | It deriving Show

data ADJ  = Wise  | Foolish | Bad   | Good  | Rich  | Mellow | Discordant |
            Poor  | Young   | Old   | Heavy | Light | Dark   | Evil       | Rusty |
            Clean | Dirty   | Wet   | Dry   | Cold  | Hot    | Magical    | Tall  |
            Short | Long    | Sharp | Dull  | Shiney deriving Show

data DET  = Each | Every | Neither | Either | Many | Few | The |
            Some | No    | Most    | A deriving Show
data That = That deriving Show

data Number = Singular | Plural deriving Show
-- Simple Tense
data Tense  = Past | Present | Future deriving Show

type Name = String

data VP = VP0 INF     | VP1 TV NP   | VP3 AV To INF | VP4 AuxV INF |
          VP5 AuxV TV | VP6 AuxV DV | VP7 AuxV AV   | VP8 AuxV deriving Show

data INF = Laugh | Cheer | Shudder | Smile deriving Show
--data INF = Laugh | Sheer | Shudder | INF TINF NP deriving Show
data TV  = Love | Admire | Help | Defeat | Catch deriving (Show, Eq)
data DV  = Give deriving (Show, Eq)
data AV  = Hope | Want deriving Show
data AuxV = Will | Do | Have | Be deriving Show

data To = To deriving Show