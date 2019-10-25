module EF2synShort where

-- I'm assuming the existence of the determiner phrase 
data Sent = Sent DP VP deriving Show

data That = That deriving Show

data DET' = Each' | Every' | Some' | Many' | Most' | The' | A' | All' | No' deriving Show

data DP = Empty Name | Some1 PCN      | Some2 ADJ PCN | Many1 PCN       |  Many2 ADJ PCN |
            Each1 SCN   | Each2 ADJ SCN | Every1 SCN     | Every2 ADJ SCN | Most1 PCN   | Most2 ADJ PCN |
            The1 CN     | The2 ADJ CN   | A1 SCN         | A2 ADJ SCN     | All1 PCN    | All2 ADJ PCN  |
            No1 CN      | No2 ADJ CN deriving Show

data Name = Alice   | Bob    | Cyrus  | Ellie | Irene   | SnowWhite |
             Ollie  | Quine  | Harry  | Xena   deriving (Eq, Show, Bounded, Enum)

data CN = Sng SCN | Pl PCN deriving Show 

data PCN = Plur SCN deriving Show

data RCN = RCN1 CN That VP | RCN2 CN That DP TV deriving Show

data SCN = Bottle | Sword  | Man    | Woman   |
           Witch  | Wizard | Giant  | Dwarf  | Warrior | Boy   | Girl   | 
           Couple | Crowd  | Coven  | Ring  | Person | Group  | Someone | 
           Thing deriving (Show, Bounded, Enum)

data ADJ = Wise  | Foolish  | Bad  | Good  | Young | Old   |  Rusty   | New |
           Cold  | Magical  | Tall | Short | Numerous | Female | Male |
           Metal | Clean | Dirty deriving (Show, Eq)

data VP = VP0 INF | VP1 TV DP | VP2 DV DP DP | VP3 Be ADJ deriving Show

data INF = Laugh | Scatter | Gather | Run | Smile | Swim | Walk  deriving (Show, Eq)
data TV  = Help  | Defeat  | Chase  | Fight | Drink deriving (Show, Eq)
data Be = Be deriving (Show, Eq)
data DV  = Give deriving (Show, Eq)