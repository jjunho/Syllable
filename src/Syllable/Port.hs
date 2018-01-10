module Syllable.Port (mkSyllables,strSyllables) where

import           Syllable.Base

groupLetters :: String -> [CV]
groupLetters (c1:'u':c3:cs)
    | c1 `elem` "qg"       =  C [c1,    'u'] :  groupLetters (c3:cs)
    | c1 `elem` "QG"       =  C [c1,    'U'] :  groupLetters (c3:cs)
    | isDiphtong  [c1,'u'] =  V [c1,    'u'] :  groupLetters (c3:cs)
    | isDiphtong  [c1,'U'] =  V [c1,    'U'] :  groupLetters (c3:cs)
    | isConsonant [c3]     = [C [c1], V "u"] ++ groupLetters (c3:cs)
    | isConsonant [c3]     = [C [c1], V "U"] ++ groupLetters (c3:cs)
groupLetters (c1:c2:cs)
    | isCluster   [c1, c2] =  C [c1, c2]     :  groupLetters cs
    | isDiphtong  [c1, c2] =  V [c1, c2]     :  groupLetters cs
    | isDigraph   [c1, c2] =  C [c1, c2]     :  groupLetters cs
    | isVowel     [c1    ] =  V [c1    ]     :  groupLetters (c2:cs)
    | isConsonant [c1    ] =  C [c1    ]     :  groupLetters (c2:cs)
groupLetters (c1:cs)
    | isVowel     [c1    ] =  V [c1    ]     :  groupLetters cs
    | isConsonant [c1    ] =  C [c1    ]     :  groupLetters cs
groupLetters []            = []

groupSyllables :: [CV] -> [[CV]]
groupSyllables []                                       = []
groupSyllables [C a , V b , C  c                      ] = [[C a, V b, C c]        ]
groupSyllables [      V b , C  c                      ] = [[     V b, C c]        ]
groupSyllables (C a : V b : C "n"  : C "s" : V e : cvs) =  [C a, V b, C "n"       ] : groupSyllables (C "s" : V e : cvs)
groupSyllables (C a : V b : C "ss" : V  e  :       cvs) =  [C a, V b, C "s"       ] : groupSyllables (C "s" : V e : cvs)
groupSyllables (C a : V b : C "rr" : V  e  :       cvs) =  [C a, V b, C "r"       ] : groupSyllables (C "r" : V e : cvs)
groupSyllables (      V b : C "ss" : V  e  :       cvs) =  [     V b, C "s"       ] : groupSyllables (C "s" : V e : cvs)
groupSyllables (      V b : C "rr" : V  e  :       cvs) =  [     V b, C "r"       ] : groupSyllables (C "r" : V e : cvs)
groupSyllables (C a : V b : C "n"  : C "s" :       cvs) =  [C a, V b, C "n", C "s"] : groupSyllables                cvs
groupSyllables (C a : V b : C "N"  : C "S" : V e : cvs) =  [C a, V b, C "N"       ] : groupSyllables (C "S" : V e : cvs)
groupSyllables (C a : V b : C "SS" : V  e  :       cvs) =  [C a, V b, C "S"       ] : groupSyllables (C "S" : V e : cvs)
groupSyllables (C a : V b : C "RR" : V  e  :       cvs) =  [C a, V b, C "R"       ] : groupSyllables (C "R" : V e : cvs)
groupSyllables (      V b : C "SS" : V  e  :       cvs) =  [     V b, C "S"       ] : groupSyllables (C "S" : V e : cvs)
groupSyllables (      V b : C "RR" : V  e  :       cvs) =  [     V b, C "R"       ] : groupSyllables (C "R" : V e : cvs)
groupSyllables (C a : V b : C "N"  : C "S" :       cvs) =  [C a, V b, C "N", C "S"] : groupSyllables                cvs
groupSyllables (C a : V b : C  c   : C  d  :       cvs) =  [C a, V b, C c         ] : groupSyllables (C  d        : cvs)
groupSyllables (      V b : C  c   : C  d  :       cvs) =  [     V b, C c         ] : groupSyllables (C  d        : cvs)
groupSyllables (C a : V b : C  c   : V  d  :       cvs) =  [C a, V b              ] : groupSyllables (C  c  : V d : cvs)
groupSyllables (      V b : C  c   : V  d  :       cvs) =  [     V b              ] : groupSyllables (C  c  : V d : cvs)
groupSyllables (C a : V b : V  c           :       cvs) =  [C a, V b              ] : groupSyllables (V  c        : cvs)
groupSyllables (      V b : V  c           :       cvs) =  [     V b              ] : groupSyllables (V  c        : cvs)
groupSyllables (C a : V b                  :       cvs) =  [C a, V b              ] : groupSyllables                cvs
groupSyllables (      V b                  :       cvs) =  [     V b              ] : groupSyllables                cvs
groupSyllables (C a : C b : V  c           :       cvs) =  [C a, C b, V c         ] : groupSyllables                cvs

showSyllables :: String -> [[CV]]
showSyllables = groupSyllables . groupLetters

mkSyllable :: [CV] -> Syllable
mkSyllable [C a, V b, C c, C d] = Syllable a  b (c ++ d)
mkSyllable [C a, V b, C c     ] = Syllable a  b  c
mkSyllable [C a, V b          ] = Syllable a  b  ""
mkSyllable [     V b, C c     ] = Syllable "" b  c
mkSyllable [     V b          ] = Syllable "" b  ""
mkSyllable [C a, C b, V c     ] = Syllable a  b  c

mkSyllables :: String -> [Syllable]
mkSyllables s = map mkSyllable x
    where x = showSyllables s

strSyllable :: Syllable -> String
strSyllable (Syllable a b c) =  a ++ b ++ c

strSyllables :: String -> String
strSyllables = unwords . map strSyllable . mkSyllables
