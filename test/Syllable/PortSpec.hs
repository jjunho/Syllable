module Syllable.PortSpec (main, spec) where

import           Syllable.Port
import           Test.Hspec

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "strSyllables - syllabify:" $ do
    it "Juliano" $
        strSyllables "Juliano" `shouldBe` "Ju li a no"
    it "bola" $
        strSyllables "bola" `shouldBe` "bo la"
    it "banco" $
        strSyllables "banco" `shouldBe` "ban co"
    it "branco" $
        strSyllables "branco" `shouldBe` "bran co"
    it "vento" $
        strSyllables "vento" `shouldBe` "ven to"
    it "ventre" $
        strSyllables "ventre" `shouldBe` "ven tre"
    it "ventres" $
        strSyllables "ventres" `shouldBe` "ven tres"
    it "entre" $
        strSyllables "entre" `shouldBe` "en tre"
    it "mnemônico" $
        strSyllables "mnemônico" `shouldBe` "mne mô ni co"
    it "tmese" $
        strSyllables "tmese" `shouldBe` "tme se"
    it "aritmética" $
        strSyllables "aritmética" `shouldBe` "a rit mé ti ca"
    it "amnésia" $
        strSyllables "amnésia" `shouldBe` "am né si a"
    it "Piauí" $
        strSyllables "Piauí" `shouldBe` "Pi au í"
    it "transclínico" $
        strSyllables "transclínico" `shouldBe` "trans clí ni co"
