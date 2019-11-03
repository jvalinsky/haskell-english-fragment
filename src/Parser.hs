module Parser where

import EF2synShort
import EF3semShort
import Text.ParserCombinators.ReadP
import Control.Applicative
import Data.Char


-- left <|> right means try the left parser and try the right parser
-- choice (xs :: [ReadP a]) means try each parser in the list

-- Helper to generate all possible parsers for a type
-- f  : function for string representation of type
-- xs : list of values for a type
helperParsers :: (a -> String) -> [a] -> [ReadP a]
helperParsers f xs = map (\x -> (string $ f x) >> return x) xs

-- takes advantage of [minBound..maxBound] to enumerate all
-- values of a Bounded, Enum type
enumParsers :: (Bounded a, Enum a, Show a) => [ReadP a]
enumParsers =  helperParsers lowerShow [minBound..maxBound]
    where lowerShow = (\x -> filter (not . (`elem` "_")) (map toLower (show x)))

-- this is for types with represented as capitalized strings, i.e. Names
enumParsers' :: (Bounded a, Enum a, Show a) => [ReadP a]
enumParsers' = helperParsers show [minBound..maxBound]

name :: ReadP Name
name = choice enumParsers'

proper :: ReadP DP
proper = fmap Empty name

scn :: ReadP SCN
scn = choice enumParsers

mcn :: ReadP MCN
mcn = choice enumParsers

singulars :: [SCN]
singulars = [minBound..maxBound]

plurals :: [PCN]
plurals = (map Plur singulars)

plualize :: PCN -> String
plualize (Plur x) = case x of
    Man   -> "Men"
    Woman -> "Women"
    Witch -> "Witches"
    Dwarf -> "Dwarves"
    _     -> (show x) ++ "s"

pluralParsers :: [ReadP PCN]
pluralParsers = helperParsers (\x -> map toLower (plualize x)) plurals

pcn :: ReadP PCN
pcn = choice pluralParsers

cn :: ReadP CN
cn = do
    noun <- (fmap Sng scn) <|> (fmap Pl pcn) <|> (fmap Ms mcn)
    return noun

-- RCN 
-----------------------------------------------------------------------
rcn' :: ReadP RCN
rcn' = do
    noun <- cn
    skipSpaces
    that
    skipSpaces
    vp1 <- vp
    return (RCN1 noun That vp1)

rcn_adj' :: ReadP RCN
rcn_adj' = do
    ad <- adj
    skipSpaces
    noun <- cn
    skipSpaces
    that
    skipSpaces
    vp1 <- vp
    return (RCN3 ad noun That vp1)

rcn_tv' :: ReadP RCN
rcn_tv' = do
    noun <- cn
    skipSpaces
    that
    skipSpaces
    dp1 <- dp
    skipSpaces
    vp1 <- tv
    return (RCN2 noun That dp1 vp1)

rcn_adj_tv' :: ReadP RCN
rcn_adj_tv' = do
    ad <- adj
    skipSpaces
    noun <- cn
    skipSpaces
    that
    skipSpaces
    dp1 <- dp
    skipSpaces
    vp1 <- tv
    return (RCN4 ad noun That dp1 vp1)

rcn :: ReadP RCN
rcn = rcn' <|> rcn_adj' <|> rcn_tv' <|> rcn_adj_tv'


-----------------------------------------------------------------------
-- RSCN 
-----------------------------------------------------------------------
rscn' :: ReadP RSCN
rscn' = do
    noun <- scn
    skipSpaces
    that
    skipSpaces
    vp1 <- vp
    return (RSCN1 noun That vp1)

rscn_adj' :: ReadP RSCN
rscn_adj' = do
    ad <- adj
    skipSpaces
    noun <- scn
    skipSpaces
    that
    skipSpaces
    vp1 <- vp
    return (RSCN3 ad noun That vp1)

rscn_tv' :: ReadP RSCN
rscn_tv' = do
    noun <- scn
    skipSpaces
    that
    skipSpaces
    dp1 <- dp
    skipSpaces
    vp1 <- tv
    return (RSCN2 noun That dp1 vp1)

rscn_adj_tv' :: ReadP RSCN
rscn_adj_tv' = do
    ad <- adj
    skipSpaces
    noun <- scn
    skipSpaces
    that
    skipSpaces
    dp1 <- dp
    skipSpaces
    vp1 <- tv
    return (RSCN4 ad noun That dp1 vp1)

rscn :: ReadP RSCN
rscn = rscn' <|> rscn_adj' <|> rscn_tv' <|> rscn_adj_tv'


---------------------------------------------------

-- RPCN 
-----------------------------------------------------------------------
rpcn' :: ReadP RPCN
rpcn' = do
    noun <- pcn
    skipSpaces
    that
    skipSpaces
    vp1 <- vp
    return (RPCN1 noun That vp1)

rpcn_adj' :: ReadP RPCN
rpcn_adj' = do
    ad <- adj
    skipSpaces
    noun <- pcn
    skipSpaces
    that
    skipSpaces
    vp1 <- vp
    return (RPCN3 ad noun That vp1)

rpcn_tv' :: ReadP RPCN
rpcn_tv' = do
    noun <- pcn
    skipSpaces
    that
    skipSpaces
    dp1 <- dp
    skipSpaces
    vp1 <- tv
    return (RPCN2 noun That dp1 vp1)

rpcn_adj_tv' :: ReadP RPCN
rpcn_adj_tv' = do
    ad <- adj
    skipSpaces
    noun <- pcn
    skipSpaces
    that
    skipSpaces
    dp1 <- dp
    skipSpaces
    vp1 <- tv
    return (RPCN4 ad noun That dp1 vp1)

rpcn :: ReadP RPCN
rpcn = rpcn' <|> rpcn_adj' <|> rpcn_tv' <|> rpcn_adj_tv'
---------------------------------------------------

each' :: ReadP DP
each' = do
    string "each"
    skipSpaces
    dp <- (fmap Each1 scn) <|> (fmap Each3 rscn)
    return dp

each_adj' :: ReadP DP
each_adj' = do
    string "each"
    skipSpaces
    ad <- adj
    skipSpaces
    dp <- (fmap (Each2 ad) scn)
    return dp

every' :: ReadP DP
every' = do
    string "every"
    skipSpaces
    dp <- (fmap Every1 scn) <|> (fmap Every3 rscn)
    return dp

every_adj' :: ReadP DP
every_adj' = do
    string "every"
    skipSpaces
    ad <- adj
    skipSpaces
    dp <- (fmap (Every2 ad) scn)
    return dp

all' :: ReadP DP
all' = do
    string "all"
    skipSpaces
    dp <- (fmap All1 pcn) <|> (fmap All3 rpcn) <|> (fmap All4 mcn)
    return dp

all_adj' :: ReadP DP
all_adj' = do
    string "all"
    skipSpaces
    ad <- adj
    skipSpaces
    dp <- (fmap (All2 ad) pcn)
    return dp

no' :: ReadP DP
no' = do
    string "no"
    skipSpaces
    dp <- (fmap No1 cn) <|> (fmap No3 rcn) <|> (fmap No4 mcn)
    return dp

no_adj' :: ReadP DP
no_adj' = do
    string "no"
    skipSpaces
    ad <- adj
    skipSpaces
    dp <- (fmap (No2 ad) cn)
    return dp

most' :: ReadP DP
most' = do
    string "most"
    skipSpaces
    dp <- (fmap Most1 pcn) <|> (fmap Most3 rpcn) <|> (fmap Most4 mcn)
    return dp

most_adj' :: ReadP DP
most_adj' = do
    string "most"
    skipSpaces
    ad <- adj
    skipSpaces
    dp <- (fmap (Most2 ad) pcn)
    return dp

a' :: ReadP DP
a' = do
    (string "a") <|> (string "an")
    skipSpaces
    dp <- (fmap A1 scn) <|> (fmap A3 rscn)
    return dp

a_adj' :: ReadP DP
a_adj' = do
    (string "a") <|> (string "an")
    skipSpaces
    ad <- adj
    skipSpaces
    dp <- (fmap (A2 ad) scn)
    return dp

some' :: ReadP DP
some' = do
    string "some"
    skipSpaces
    dp <- (fmap Some1 pcn) <|> (fmap Some4 mcn) <|> (fmap Some3 rpcn)
    return dp

some_adj' :: ReadP DP
some_adj' = do
    string "some"
    skipSpaces
    ad <- adj
    skipSpaces
    dp <- (fmap (Some2 ad) pcn)
    return dp

the' :: ReadP DP
the' = do
    string "the"
    skipSpaces
    dp <- (fmap The1 cn) <|> (fmap The4 mcn) <|> (fmap The3 rcn)
    return dp

the_adj' :: ReadP DP
the_adj' = do
    string "the"
    skipSpaces
    ad <- adj
    skipSpaces
    dp <- (fmap (The2 ad) cn)
    return dp

detThe :: ReadP DP
detThe = the' <|> the_adj'

detSome :: ReadP DP
detSome = some' <|> some_adj'

detA :: ReadP DP
detA = a' <|> a_adj'

detEach :: ReadP DP
detEach = each' <|> each_adj'

detEvery :: ReadP DP
detEvery = every' <|> every_adj'

detMost :: ReadP DP
detMost = most' <|> most_adj'

detAll :: ReadP DP
detAll = all' <|> all_adj'

detNo :: ReadP DP
detNo = no' <|> no_adj'

dp :: ReadP DP
dp = choice [detSome, detThe, detA, detEvery, detEach, detAll, detMost, detNo, proper]

adj :: ReadP ADJ
adj = choice enumParsers

that :: ReadP That
that = string "that" >> return That

infs :: [INF]
infs = [minBound..maxBound]

singVerb :: (Show a) => a -> String
singVerb = (map toLower) . (++ "s") . show

singVerbParser xs = helperParsers singVerb xs

inf :: ReadP INF
inf = choice (enumParsers ++ (singVerbParser infs))

infVP :: ReadP VP
infVP = fmap VP0 inf

beVP :: ReadP VP
beVP = do
    (string "is") <|> (string "are")
    skipSpaces
    ad <- adj
    return (VP3 Be ad)

tvs :: [TV]
tvs = [minBound..maxBound]

tv :: ReadP TV
tv = choice (enumParsers ++ (singVerbParser tvs))

tVP :: ReadP VP
tVP = do
    verb <- tv
    skipSpaces
    dp1 <- dp
    return (VP1 verb dp1)

dvs :: [DV]
dvs = [minBound..maxBound]

dv :: ReadP DV
dv = choice (enumParsers ++ (singVerbParser dvs))

dVP :: ReadP VP
dVP = do
    verb <- dv
    skipSpaces
    dp1 <- dp
    skipSpaces
    dp2 <- dp
    return (VP2 verb dp1 dp2)

vp :: ReadP VP
vp = choice [infVP, beVP, tVP, dVP]

sent :: ReadP Sent
sent = do
    dp1 <- dp
    skipSpaces
    vp1 <- vp
    return (Sent dp1 vp1)


parse' :: ReadP a -> String -> [(a, String)]
parse' p str = readP_to_S p str

parseSent :: String -> [(Sent, String)]
parseSent str = parse' sent str

-- Find full parses and evaluate them using intSent
-- Empty list means no full parse (ill-formed sentence)
eval :: String -> [Bool]
eval str = map (\x -> intSent $ fst x) parses'
    where parses = readP_to_S sent str
          parses' = filter fullyParsed parses
          fullyParsed pair = length (snd pair) == 0