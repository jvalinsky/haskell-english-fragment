module Parser where

import EF2synShort
import Text.ParserCombinators.ReadP
import Control.Applicative
import Data.Char

helperParsers :: (a -> String) -> [a] -> [ReadP a]
helperParsers f xs = map (\x -> (string $ f x) >> return x) xs

enumParsers :: (Bounded a, Enum a, Show a) => [ReadP a]
enumParsers =  helperParsers lowerShow [minBound..maxBound]
    where lowerShow = (\x -> filter (not . (`elem` "_")) (map toLower (show x)))

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

{-
rcn :: ReadP RCN
rcn = do
    noun <- cn
    skipSpaces
    that <- that
    skipSpaces
    verbP <- vp
    return (RCN1 cn that verbP)
-}
each' :: ReadP DP
each' = do
    string "each"
    skipSpaces
    dp <- (fmap Each1 scn)
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
    dp <- (fmap Every1 scn)
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
    dp <- (fmap All1 pcn)
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
    dp <- (fmap No1 cn)
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
    dp <- (fmap Most1 pcn)
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
    string "a"
    skipSpaces
    dp <- (fmap A1 scn)
    return dp

a_adj' :: ReadP DP
a_adj' = do
    string "a"
    skipSpaces
    ad <- adj
    skipSpaces
    dp <- (fmap (A2 ad) scn)
    return dp

some' :: ReadP DP
some' = do
    string "some"
    skipSpaces
    dp <- (fmap Some1 pcn) <|> (fmap Some4 mcn)
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
    dp <- (fmap The1 cn) <|> (fmap The4 mcn)
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

test = readP_to_S sent "Ellie gives Alice the ring"