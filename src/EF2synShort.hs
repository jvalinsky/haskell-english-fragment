module EF2synShort where

-- I'm assuming the existence of the determiner phrase 
data Sent = Sent DetP VP deriving Show

data To = To deriving Show
data Of = Of deriving Show
data And = And deriving Show

data That = That deriving Show

-- Some of these are semantically restricted
data DET = Some | Many | Most | Each | Every | Neither | Either | The | No | A deriving Show

-- Some of these construction may not be semantically valid
data DetP = DP1 Name | DP2 DET CN | DP3 DetP Of DetP | DP4 DetP And DetP deriving Show

data CN = Sing SCN | Pl PCN | Mass MCN | Col CCN deriving Show 

-- Mass Common Nouns
data MCN  = Water    | Wood | Air    | Blood | Metal | Earth  | Foilage |
            Darkness | Clay | Envy   | Fun   | Music | Poetry | Rust    |
            Gold     | Mail | Advice | Honor | Ice   | Fabric | Confusion deriving Show

data SCN = Someone  | Belief | Man    | Woman | Hero  | Heroine  | Sword | Drop |
           Fork     | Spoon  | Knife  | Witch | Boy   | Girl     | Dwarf | Prince |
           Princess | Giant  | Wizard | Mouse | Cat   | Dress    | Shoe  | Bird |
           Spy deriving (Show, Bounded, Enum)

-- Note: Some Plural Entities are represented by Singular Common Nouns, ex: Group
-- Singular Nouns that are collective
data CCN = Court | Coven | Crowd | Cabal | Horde | Commitee | Pair 
          | Army | Nest | Flock | Group deriving Show

 -- Plural Common Nouns, most are pluralized version of SCN's
 -- but some nouns have only plural form or their singular form has
 -- a different meaning (ex: glass vs. glasses)
data PCN = PS SCN | PC CCN | Glasses | Jeans

data RCN = RCN1 CN That VP | RCN2 CN That DetP TV | RCN3 ADJ CN deriving Show


pluralCCNShow :: CCN -> String
pluralCCNShow x = case x of
    Army -> "Armies"
    _    -> (show x) ++ "s"

pluralSCNShow :: SCN -> String
pluralSCNShow x = case x of 
    Man      -> "Men"
    Woman    -> "Women"
    Hero     -> "Heroes"
    Dress    -> "Dresses"
    Witch    -> "Witches"
    Knife    -> "Knives"
    Princess -> "Princesses"
    Dwarf    -> "Dwarves"
    Mouse    -> "Mice"
    _        -> (show x) ++ "s"

instance Show PCN where
    show Glasses  = "Glasses"
    show Jeans    = "Jeans"
    show (PS scn) = pluralSCNShow scn 
    show (PC ccn) = pluralCCNShow ccn 

-- They is Third Person Singular here to denote gender neutral pronoun
-- data Pronoun = He | She | They | It deriving Show

data ADJ  = Wise  | Foolish | Bad   | Good  | Rich  | Mellow | Discordant |
            Poor  | Young   | Old   | Heavy | Light | Dark   |  Rusty |
            Clean | Dirty   | Wet   | Dry   | Cold  | Hot    | Magical | Tall  |
            Short | Long    | Sharp | Dull  | Shiney deriving Show

-- Simple Tense
data Tense  = Past | Present | Future deriving Show

type Name = String

data VP = VP0 INF     | VP1 TV DetP   | VP3 AV To INF | VP4 AuxV INF |
          VP5 AuxV TV | VP6 AuxV DV   | VP7 AuxV AV   | VP8 AuxV deriving Show

data INF = Laugh | Cheer | Shudder | Smile deriving Show
--data INF = Laugh | Sheer | Shudder | INF TINF NP deriving Show
data TV  = Love | Admire | Help | Defeat | Catch deriving (Show, Eq)
data DV  = Give deriving (Show, Eq)
data AV  = Hope | Want deriving Show
data AuxV = Will | Do | Have | Be deriving Show