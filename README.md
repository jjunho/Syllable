# `Syllable` - A haskell library for syllabification of words in Portuguese

Syllables in Portuguese follow basically the following scheme:

    (C)(L)V(S)(N)(C)

where:
    C = consonant
    L = liquid (/l/ or /r/)
    V = vowel
    S = semivowel (<i> /j/ or <u> /w/)
    N = nasal (/n/)

There are still some exceptions that need to be taken care of but, overall, it is a functioning library.