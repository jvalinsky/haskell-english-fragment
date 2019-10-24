module EF2synShort where

-- I'm assuming the existence of the determiner phrase 
data Sent = Sent DP VP deriving Show

data That = That deriving Show

data DetP = Empty Name | Some PCN      | Some' ADJ PCN | Many PCN       |  Many' PCN |
            Each SCN   | Each' ADJ SCN | Every SCN     | Every' ADJ SCN | Most PCN   | Most' ADJ PCN |
            The CN     | The' ADJ CN   | A SCN         | A' ADJ SCN     | All PCN    | All' ADJ PCN deriving Show

data DP = DP DetP | DP' DetP RCN deriving Show

data Name = Alice  | Bob    | Cyrus  | Ellie | Goldilocks  | Irene    | Jim | Kim  | Hillary |
            Noah   | Ollie  | Penny  | Quine | SnowWhite   | Mickey   | Uli | Fred | Linda   |
            Victor | Harry  | Xena   | Zorba | The_Genesee | Whiskers | Tom | Sue  | Dorothy |
            Stuart | Gerald | Minnie | Dis   | The_Rhine   | Thorin | Mittens | Harry deriving Show

data CN = Sng SCN | Pl PCN deriving Show 

 -- Plural Common Nouns, most are pluralized version of SCN's
 -- but some nouns have only plural form or their singular form has
 -- a different meaning (ex: glass vs. glasses)
data PCN = Plur SCN | Glasses | Jeans deriving Show

data RCN = RCN1 That VP | RCN2 That DetP TV deriving Show

data SCN = Bottle | Cup    | Sword  | Dagger | River   | Dress | Raft   | Man    | Woman   |
           Witch  | Wizard | Giant  | Dwarf  | Warrior | Boy   | Girl   | Dwarf  | Bird    |
           Mouse  | Cat    | Couple | Crowd  | Coven   | Ring  | Person | Group  | Someone | 
           animal | Thing deriving (Show, Bounded, Enum)

data ADJ = Wise  | Foolish  | Bad  | Good  | Young | Old   |  Rusty   | Torn |
           Cold  | Magical  | Tall | Short | Long  | Shiny | Numerous | Female | Male |
           Silver | Steel | Gold | Metal | Clean | Dirty  | New deriving (Show, Eq)

data VP = VP0 INF | VP1 TV DetP | VP3 DV DetP DetP | VP4 Be' ADJ deriving Show

data INF = Laugh | Scatter | Gather | Run   deriving (Show, Eq)
data TV  = Help  | Defeat  | Chase  | Fight | Drink | Be deriving (Show, Eq)
data Be = Be' deriving (Show, Eq)
data DV  = Give deriving (Show, Eq)