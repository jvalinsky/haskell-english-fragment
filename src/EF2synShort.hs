module EF2synShort where

-- I'm assuming the existence of the determiner phrase 
data Sent = Sent DetP VP deriving Show

data To = To deriving Show
data Of = Of deriving Show
data And = And deriving Show

data That = That deriving Show

data DetP = DP1 Name | DP2 DET CN | DP3 DET RCN | DP4 DetP Of DetP | DP5 DetP And DetP deriving Show

-- Some of these are semantically restricted
data DET = Some | Many | Most | Each | Every | Neither | Either | The | No | A deriving Show


data Name = Alice  | Bob    | Cyrus  | Ellie | Goldilocks  | Irene    | Jim | Kim  | Hillary |
            Noah   | Ollie  | Penny  | Quine | SnowWhite   | Mickey   | Uli | Fred | Linda   |
            Victor | Willie | Xena   | Zorba | The_Genesee | Whiskers | Tom | Sue  | Dorothy |
            Stuart | Gerald | Minnie | Lake_Ontario deriving Show

data CN = Sng SCN | Plur PCN | Ms MCN deriving Show 

-- Mass Common Nouns
data MCN  = Water  | Wood   | Air  | Wine   | Metal  | Earth  | Rust
            Gold   | Advice | Ice  | Fabric deriving Show

 -- Plural Common Nouns, most are pluralized version of SCN's
 -- but some nouns have only plural form or their singular form has
 -- a different meaning (ex: glass vs. glasses)
 data PCN = Pluralize SCN | Glasses | Jeans

 data RCN = RCN1 CN That VP | RCN2 CN That DetP TV | RCN3 ADJ CN deriving Show

data SCN = Lake     | Cup   | Man    | Woman | Hero   | Heroine | Sword  | Drop  | Bottle |
           Puddle   | Fork  | Spoon  | Knife | Witch  | Boy     | Girl   | Dwarf | Nest   |
           Princess | Giant | Wizard | Mouse | Cat    | Dress   | Shoe   | Bird  | Card   |
           Spy      | Court | Couple | Crowd | Coven  | Prince  | Ring   | Deck deriving (Show, Bounded, Enum)

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
    Army     -> "Armies"
    Spy      -> "Spies"
    _        -> (show x) ++ "s"

instance Show PCN where
    show Glasses  = "Glasses"
    show Jeans    = "Jeans"
    show (Pluralize scn) = pluralSCNShow scn 

data ADJ  = Wise  | Foolish | Bad     | Good   | Rich  | Poor    | Young | Old   | 
            Heavy | Light   | Dark    |  Rusty | Clean | Dirty   | Wet   | Dry   | 
            Warm  | Cold    | Magical | Tall   | Short | Long    | Sharp | Dull  |
            Sweet | Shiney deriving Show

data VP = VP0 INF | VP1 TV DetP | VP3 AV To INF | VP4 DV DetP DetP deriving Show

data INF = Laugh | Cheer  | Shudder | Smile deriving Show
data TV  = Love  | Admire | Help | Defeat | Chase | Catch | Drink | Be | Have deriving (Show, Eq)
data DV  = Give  | Send deriving (Show, Eq)
data AV  = Hope  | Want deriving Show