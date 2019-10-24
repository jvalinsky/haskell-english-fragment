module EF2synShort where

-- I'm assuming the existence of the determiner phrase 
data Sent = Sent DetP VP deriving Show

data Of = Of deriving Show
data And = And deriving Show
data That = That deriving Show
data In = In deriving Show

data DetP = DP1 Name | DP2 DET CN | DP3 DET ADJ CN | DP4 DET RCN  | DP5 DET ADJ RCN | DP6 CN deriving Show

-- Some of these are semantically restricted
data DET = Some | Many | Most | Each | Every | Neither | Either | The | No | A deriving Show


data Name = Alice  | Bob    | Cyrus  | Ellie | Goldilocks  | Irene    | Jim | Kim  | Hillary |
            Noah   | Ollie  | Penny  | Quine | SnowWhite   | Mickey   | Uli | Fred | Linda   |
            Victor | Willie | Xena   | Zorba | The_Genesee | Whiskers | Tom | Sue  | Dorothy |
            Stuart | Gerald | Minnie | Dis   | The_Rhine   | Thorin | Mittens | Harry deriving Show

data CN = Sng SCN | Pl PCN deriving Show 

 -- Plural Common Nouns, most are pluralized version of SCN's
 -- but some nouns have only plural form or their singular form has
 -- a different meaning (ex: glass vs. glasses)
data PCN = Plur SCN | Glasses | Jeans deriving Show

data RCN = RCN1 CN That VP | RCN2 CN That DetP TV | RCN4 CN Of DetP | RCN5 CN And DetP | RCN6 CN In DetP deriving Show

data SCN = Lake     | Cup   | Man    | Woman | Sword  | Drop  | Bottle |
           Witch  | Boy     | Girl   | Dwarf | Nest   |
           Raft  | Wizard | Mouse | Cat    | Dress   | Bird  |
           Couple | Crowd | Coven  | Ring   | Person | Giant | Someone deriving (Show, Bounded, Enum)

data ADJ = Wise  | Foolish  | Bad  | Good  | Young | Old   |  Rusty   | Torn |
           Cold  | Magical  | Tall | Short | Long  | Shiny | Numerous | Female | Male |
           Silver | Steel | Gold | Metal | Clean | Dirty  | New deriving (Show, Eq)

data VP = VP0 INF | VP1 TV DetP | VP3 DV DetP DetP deriving Show

data INF = Laugh | Scatter | Gather | Run   deriving (Show, Eq)
data TV  = Help  | Defeat  | Chase  | Drink | Be deriving (Show, Eq)
data DV  = Give deriving (Show, Eq)