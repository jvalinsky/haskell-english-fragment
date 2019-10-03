module EF2synShort where

data Sent = NP VP 
data NP  = NP1 Proper | NP2 DET CN | NP3 DET RCN deriving Show
data  CN = Mass MCN | Sing SCN | Plur PCN deriving Show
data MCN = Water | Blood | Earth | Knowledge | Cloth | Metal | Leather | Cultery deriving Show

data SCN = Someone  | Belief | Man | Woman | Hero | Heroine | Sword | Fork | Spoon | Knife | Witch | 
  Boy | Girl | Dwarf | Princess | Prince | Giant | Wizard | Dog | Cat | Dress | Shoe  deriving Show

data PCN = Pl SCN | Everyone | Coven | Court | Horde | Cabal | Commitee deriving Show
data RCN = RCN1 CN That VP | RCN2 CN That NP TV | RCN3 ADJ CN deriving Show

data ADJ  = Fake | Secret | Tall | Short | Merry | Grumpy | Fast | Dangerous | Slow | Real | Red | Blue | Clean | Dirty deriving Show
data DET  = The | Every | Some | No | Most | A deriving Show
data That = That deriving Show


-- I use "neutral" to make distinction between "it" and "they"/"one" for third-person singular
data Gender = Masculine  | Feminine | Neuter | Neutral1 | Neutral2 deriving Show
data Person = First | Second | Third deriving Show
data Number = Singular | Plural deriving Show
-- Simple Tense
data Tense  = Past | Present | Future deriving Show


type Name    = String
data Proper  = Prop Name
data Pronoun = Pro { person_ :: Person, number_ :: Number, gender_ :: Gender }

instance Show Pronoun where
  show (Pro First Singular x)           = "I"
  show (Pro First Plural x)             = "We"
  show (Pro Second x y)                 = "You"
  show (Pro Third Singular Masculine)   = "He"
  show (Pro Third Singular Feminine)    = "She"
  show (Pro Third Singular Neuter)      = "It"
  show (Pro Third Singular Neutral1)    = "They"
  show (Pro Third Singular Neutral2)    = "One"
  show (Pro Third Plural x)             = "They"


data VP = VP0 PNT INF | VP1 PNT TV NP  | VP3 PNT AV To INF deriving Show

data INF = Laugh | Cheer | Shudder | Smile deriving Show
--data INF = Laugh | Sheer | Shudder | INF TINF NP deriving Show

-- English doesn't use gender for verb agreement
data PNT = Pnt { _person :: Person, _number :: Number, _tense :: Tense } deriving Show

data TV  = Love | Admire | Help | Defeat | Catch deriving (Show, Eq)
data DV  = Give deriving (Show, Eq)
data AV  = Hope | Want deriving Show

data To = To deriving Show

{-
showPast :: Vb -> String
showPast (T x) | x `elem` [Love, Admire] = (show x) ++ "d"
               | x `elem` [Help, Defeat] = (show x) ++ "ed"
               | x == Catch = "Caught"

showPast (D Give) = "Gave"

instance Show Verb where
  show (V First Singular Present x) = show x
  show (V First Singular Past x) = showPast x
  show (V First Singular Future x) = "Will " ++ show x 
  -}
