module Syllable.Base where

data Syllable = Syllable { sylOnset   :: String
                         , sylNucleus :: String
                         , sylCoda    :: String }
                         deriving (Show)

data CV = V { cv :: String } | C { cv :: String }
    deriving Show

vowels :: String
vowels = "aeiouáéíóúàâêôãõüAEIOUÁÉÍÓÚÀÂÊÔÃÕÜ"

digraphs :: String
digraphs = "ss rr ch lh nh gu qu SS RR CH LH NH GU QU"

semivowels :: String
semivowels = "iuIU"

nasalDiphtongs :: String
nasalDiphtongs = "ão ãe õe ÃO ÃE ÕE"

isNucleus :: String -> Bool
isNucleus [s]     = isVowel [s]
isNucleus [s1,s2] = isDiphtong [s1,s2]

isOnset :: String -> Bool
isOnset [s]     = isConsonant [s]
isOnset [s1,s2] = isCluster [s1,s2] || isDigraph [s1,s2]

isCoda :: String -> Bool
isCoda [s]     = isConsonant [s]
isCoda [s1,s2] = s1 `elem` "nN" && isConsonant [s2]

isVowel :: String -> Bool
isVowel [x] = x `elem` vowels

isConsonant :: String -> Bool
isConsonant = not . isVowel

isDigraph :: String -> Bool
isDigraph [c1,c2] = [c1, c2] `elem` words digraphs

isDiphtong :: String -> Bool
isDiphtong vs@[v1,v2]
    | isVowel [v1] && v2 `elem` semivowels = True
    | vs `elem` words nasalDiphtongs     = True
    | otherwise                      = False

isCluster :: String -> Bool
isCluster c = c `elem` [[x, y] | x <- "ctpfgdbv", y <- "lr"]

mkDiphtong :: String -> [String]
mkDiphtong [v1, v2] =
    if isDiphtong [v1,v2]
        then [[v1,v2]]
        else [[v1],[v2]]

mkDigraph :: String -> [String]
mkDigraph [c1, c2] =
    if isDigraph [c1,c2]
        then [[c1,c2]]
        else [[c1],[c2]]
