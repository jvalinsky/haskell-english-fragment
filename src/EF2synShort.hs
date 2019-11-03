module EF2synShort where

-- I'm assuming the existence of the determiner phrase 
data Sent = Sent DP VP deriving Show

data That = That deriving Show

data DET' = Each' | Every' | Some' | Most' | The' | A' | All' | No' deriving Show

data DP =   Some1  PCN  | Some2  ADJ PCN | Some3  RPCN | Each1 SCN  | Each2 ADJ SCN | Each3 RSCN |
            Every1 SCN  | Every2 ADJ SCN | Every3 RSCN | Most1 PCN  | Most2 ADJ PCN | Most3 RPCN |
            The1   CN   | The2   ADJ CN  | The3   RCN  | A1    SCN  | A2    ADJ SCN | A3    RSCN |
            All1   PCN  | All2   ADJ PCN | All3   RPCN | No1   CN   | No2   ADJ CN  | No3   RCN  | 
            Empty Name  | Some4 MCN | Most4 MCN | The4 MCN | All4 MCN | No4 MCN deriving Show

data Name = Alice   | Bob    | Cyrus  | Ellie | Irene   | SnowWhite |
             Ollie  | Quine  | Harry  | Xena   deriving (Eq, Show, Bounded, Enum)

data CN = Sng SCN | Pl PCN | Ms MCN deriving Show 

data MCN = Gold_ deriving (Bounded, Enum, Show, Eq)

data PCN = Plur SCN deriving Show

data RCN = RCN1 CN That VP | RCN2 CN That DP TV | RCN3 ADJ CN That VP | RCN4 ADJ CN That DP TV deriving Show

data RSCN = RSCN1 SCN That VP | RSCN2 SCN That DP TV | RSCN3 ADJ SCN That VP | RSCN4 ADJ SCN That DP TV deriving Show

data RPCN = RPCN1 PCN That VP | RPCN2 PCN That DP TV | RPCN3 ADJ PCN That VP | RPCN4 ADJ PCN That DP TV deriving Show

data SCN = Bottle | Sword  | Man    | Woman   |
           Witch  | Wizard | Giant  | Dwarf  | Warrior | Boy   | Girl   | 
           Couple | Crowd  | Coven  | Ring  | Person | Group  | 
           Thing deriving (Show, Bounded, Enum)

data ADJ = Wise  | Foolish | Bad   | Good  | Young | Old   |  Rusty   | New |
           Cold  | Magical | Tall  | Short | Numerous | Female | Male |
           Metal | Gold    | Clean | Dirty deriving (Bounded, Enum, Show, Eq)

data VP = VP0 INF | VP1 TV DP | VP2 DV DP DP | VP3 Be ADJ deriving Show

data INF = Laugh | Scatter | Gather | Run | Smile | Swim | Walk deriving (Bounded, Enum, Show, Eq)
data TV  = Help  | Defeat  | Chase  | Fight | Drink deriving (Bounded, Enum, Show, Eq)
data Be = Be deriving (Show, Eq)
data DV  = Give deriving (Bounded, Enum, Show, Eq)