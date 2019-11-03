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

adj :: ReadP ADJ
adj = choice enumParsers

that :: ReadP That
that = string "that" >> return That

test = readP_to_S cn "bottles"