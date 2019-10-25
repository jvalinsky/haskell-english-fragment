module EF2synShort where

-- I'm assuming the existence of the determiner phrase 
data Sent = Sent DP VP deriving Show

data That = That deriving Show

data DP = Empty Name | Some PCN      | Some' ADJ PCN | Many PCN       |  Many' PCN |
            Each SCN   | Each' ADJ SCN | Every SCN     | Every' ADJ SCN | Most PCN   | Most' ADJ PCN |
            The CN     | The' ADJ CN   | A SCN         | A' ADJ SCN     | All PCN    | All' ADJ PCN  |
            No CN      | No' ADJ CN deriving Show

data Name = Alice   | Bob    | Cyrus  | Ellie | Irene   | SnowWhite |
             Ollie  | Quine  | Harry  | Xena   deriving (Eq, Show, Bounded, Enum)

data CN = Sng SCN | Pl PCN deriving Show 

data PCN = Plur SCN deriving Show

data RCN = RCN1 That VP | RCN2 That DP TV deriving Show

data SCN = Bottle | Sword  | Man    | Woman   |
           Witch  | Wizard | Giant  | Dwarf  | Warrior | Boy   | Girl   | 
           Couple | Crowd  | Coven  | Ring  | Person | Group  | Someone | 
           Thing deriving (Show, Bounded, Enum)

data ADJ = Wise  | Foolish  | Bad  | Good  | Young | Old   |  Rusty   | New |
           Cold  | Magical  | Tall | Short | Numerous | Female | Male |
           Metal | Clean | Dirty deriving (Show, Eq)

data VP = VP0 INF | VP1 TV DP | VP3 DV DP DP | VP4 Be' ADJ deriving Show

data INF = Laugh | Scatter | Gather | Run   deriving (Show, Eq)
data TV  = Help  | Defeat  | Chase  | Fight | Drink | Be deriving (Show, Eq)
data Be' = Be' deriving (Show, Eq)
data DV  = Give deriving (Show, Eq)