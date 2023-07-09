module BelfiusParserTests (
  belfiusParserSpecs,
  ) where

import           Test.Hspec
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian)

import           Lib

belfiusParserSpecs :: SpecWith ()
belfiusParserSpecs = describe "src/BelfiusParser" $ do
  describe "works for examples with empty extract number & empty transaction number" $ do
    it "parses CSV into UnstructuredData" $
      Lib.runUnstructuredDataParser "exampleInputWithout" exampleInputWithout `shouldBe` Right exampleUnstructuredDataWithout
    case columnsToBelfius exampleUnstructuredDataWithout of
      [Right exampleDataWithout1, Right exampleDataWithout2, Right exampleDataWithout3, Right exampleDataWithout4] -> do
        describe "example 1" $ do
          it "account" $ account exampleDataWithout1 `shouldBe` "BE51 0607 0809 1011"
          it "date" $ date exampleDataWithout1 `shouldBe` fromGregorian 2023 6 19
          it "otherAccount" $ otherAccount exampleDataWithout1 `shouldBe` Nothing
          it "otherName" $ otherName exampleDataWithout1 `shouldBe` mkNonBlankText "MAGASIN"
          it "description" $ description exampleDataWithout1 `shouldBe` mkNonBlankText "MAGASIN PARIS BE 13,99 EUR CARTE N\176 5152 5354 5556 5758 - MUSTERMANN FRANK"
          it "amountCents" $ amountCents exampleDataWithout1 `shouldBe` -1399
          it "currency" $ currency exampleDataWithout1 `shouldBe` "EUR"
          it "identifyingComment" $ identifyingComment exampleDataWithout1 `shouldBe`
            "BE51 0607 0809 1011;19/06/2023;;;;MAGASIN;;9999 PARIS;PAIEMENT MAESTRO 18/06 MAGASIN PARIS BE  13,99 EUR CARTE N° 5152 5354 5556 5758 - MUSTERMANN  FRANK  REF. : 0123Q5W789012 VAL. 19-06;19/06/2023;-13,99;EUR;;BE;PAIEMENT MAESTRO 18/06 MAGASIN PARIS BE  13,99 EUR CARTE N° 5152 5354 5556 5758 - MUSTERMANN  FRANK  REF. : 0123Q5W789012 VAL. 19-06"
        describe "example 2" $ do
          it "account" $ account exampleDataWithout2 `shouldBe` "BE51 0607 0809 1011"
          it "date" $ date exampleDataWithout2 `shouldBe` fromGregorian 2023 6 26
          it "otherAccount" $ otherAccount exampleDataWithout2 `shouldBe` Nothing
          it "otherName" $ otherName exampleDataWithout2 `shouldBe` mkNonBlankText "HOWARD CURRYP2P MOBILE"
          it "description" $ description exampleDataWithout2 `shouldBe` mkNonBlankText "HOWARD CURRYP2P MOBILE LE 24/06 A 20:03 AVEC VOTRE CARTE BANCAIRE 5152 5354 5556 5758"
          it "amountCents" $ amountCents exampleDataWithout2 `shouldBe` -1806
          it "currency" $ currency exampleDataWithout2 `shouldBe` "EUR"
          it "identifyingComment" $ identifyingComment exampleDataWithout2 `shouldBe`
            "BE51 0607 0809 1011;26/06/2023;;;;HOWARD CURRYP2P MOBILE;;;PAIEMENT VIA VOTRE APP MOBILE BANKING OU VOTRE         BANCONTACT-APP A HOWARD CURRYP2P MOBILE LE 24/06 A 20:03 AVEC VOTRE CARTE BANCAIRE 5152 5354 5556 5758    REF. : 6667774448888 VAL. 24-06;24/06/2023;-18,06;EUR;;;PAIEMENT VIA VOTRE APP MOBILE BANKING OU VOTRE         BANCONTACT-APP A HOWARD CURRYP2P MOBILE LE 24/06 A 20:03 AVEC VOTRE CARTE BANCAIRE 5152 5354 5556 5758    REF. : 6667774448888 VAL. 24-06"
        describe "example 3" $ do
          it "account" $ account exampleDataWithout3 `shouldBe` "BE51 0607 0809 1011"
          it "date" $ date exampleDataWithout3 `shouldBe` fromGregorian 2023 6 21
          it "otherAccount" $ otherAccount exampleDataWithout3 `shouldBe` mkNonBlankText "777-7777777-77"
          it "otherName" $ otherName exampleDataWithout3 `shouldBe` mkNonBlankText "BANKSYS"
          it "description" $ description exampleDataWithout3 `shouldBe` mkNonBlankText "ARGENT RECU VIA VOTRE APP MOBILE BANKING OU VOTRE BANCONTACT-APP LE 20/06 SUR VOTRE CARTE BANCAIRE."
          it "amountCents" $ amountCents exampleDataWithout3 `shouldBe` 1160
          it "currency" $ currency exampleDataWithout3 `shouldBe` "EUR"
          it "identifyingComment" $ identifyingComment exampleDataWithout3 `shouldBe`
            "BE51 0607 0809 1011;21/06/2023;;;777-7777777-77;BANKSYS;;;ARGENT RECU VIA VOTRE APP MOBILE BANKING OU VOTRE  BANCONTACT-APP LE 20/06 SUR VOTRE CARTE BANCAIRE.  REF. : 123456L654321 VAL. 21-06;21/06/2023;11,60;EUR;AXABBE22;;ARGENT RECU VIA VOTRE APP MOBILE BANKING OU VOTRE  BANCONTACT-APP LE 20/06 SUR VOTRE CARTE BANCAIRE.  REF. : 123456L654321 VAL. 21-06"
        describe "example 4" $ do
          it "account" $ account exampleDataWithout4 `shouldBe` "BE51 0607 0809 1011"
          it "date" $ date exampleDataWithout4 `shouldBe` fromGregorian 2023 6 26
          it "otherAccount" $ otherAccount exampleDataWithout4 `shouldBe` mkNonBlankText "BE54 5556 5758 5960"
          it "otherName" $ otherName exampleDataWithout4 `shouldBe` mkNonBlankText "Dupont-Durand"
          it "description" $ description exampleDataWithout4 `shouldBe` mkNonBlankText "Transfert moitie don Dupont-Delfosse"
          it "amountCents" $ amountCents exampleDataWithout4 `shouldBe` -5003
          it "currency" $ currency exampleDataWithout4 `shouldBe` "EUR"
          it "identifyingComment" $ identifyingComment exampleDataWithout4 `shouldBe`
            "BE51 0607 0809 1011;26/06/2023;;;BE54 5556 5758 5960;Dupont-Durand;;;VIREMENT BELFIUS MOBILE VERS BE54 5556 5758 5960  Dupont-Durand Transfert moitie don Dupont-Delfosse  REF. : 090807060C040 VAL. 26-06;26/06/2023;-50,03;EUR;ARSPBE22;;Transfert moitie don Dupont-Delfosse"
      _ -> error "exampleInputWithout was not parsed correctly"

exampleInputWithout :: T.Text
exampleInputWithout = "Date de comptabilisation à partir de;01/05/2023\n\
                      \Date de comptabilisation jusqu'au;30/06/2023\n\
                      \Montant à partir de;\n\
                      \Montant jusqu'à;\n\
                      \Numéro d'extrait à partir de;\n\
                      \Numéro d'extrait jusqu'au;\n\
                      \Communication;\n\
                      \Nom contrepartie contient;\n\
                      \Compte contrepartie;\n\
                      \Dernier solde;1234.56 EUR\n\
                      \Date/heure du dernier solde;19/06/2023 15:16:12\n\
                      \;\n\
                      \Compte;Date de comptabilisation;Numéro d'extrait;Numéro de transaction;Compte contrepartie;Nom contrepartie contient;Rue et numéro;Code postal et localité;Transaction;Date valeur;Montant;Devise;BIC;Code pays;Communications\n\
                      \BE51 0607 0809 1011;19/06/2023;;;;MAGASIN;;9999 PARIS;PAIEMENT MAESTRO 18/06 MAGASIN PARIS BE  13,99 EUR CARTE N° 5152 5354 5556 5758 - MUSTERMANN  FRANK  REF. : 0123Q5W789012 VAL. 19-06;19/06/2023;-13,99;EUR;;BE;PAIEMENT MAESTRO 18/06 MAGASIN PARIS BE  13,99 EUR CARTE N° 5152 5354 5556 5758 - MUSTERMANN  FRANK  REF. : 0123Q5W789012 VAL. 19-06\n\
                      \BE51 0607 0809 1011;26/06/2023;;;;HOWARD CURRYP2P MOBILE;;;PAIEMENT VIA VOTRE APP MOBILE BANKING OU VOTRE         BANCONTACT-APP A HOWARD CURRYP2P MOBILE LE 24/06 A 20:03 AVEC VOTRE CARTE BANCAIRE 5152 5354 5556 5758    REF. : 6667774448888 VAL. 24-06;24/06/2023;-18,06;EUR;;;PAIEMENT VIA VOTRE APP MOBILE BANKING OU VOTRE         BANCONTACT-APP A HOWARD CURRYP2P MOBILE LE 24/06 A 20:03 AVEC VOTRE CARTE BANCAIRE 5152 5354 5556 5758    REF. : 6667774448888 VAL. 24-06\n\
                      \BE51 0607 0809 1011;21/06/2023;;;777-7777777-77;BANKSYS;;;ARGENT RECU VIA VOTRE APP MOBILE BANKING OU VOTRE  BANCONTACT-APP LE 20/06 SUR VOTRE CARTE BANCAIRE.  REF. : 123456L654321 VAL. 21-06;21/06/2023;11,60;EUR;AXABBE22;;ARGENT RECU VIA VOTRE APP MOBILE BANKING OU VOTRE  BANCONTACT-APP LE 20/06 SUR VOTRE CARTE BANCAIRE.  REF. : 123456L654321 VAL. 21-06\n\
                      \BE51 0607 0809 1011;26/06/2023;;;BE54 5556 5758 5960;Dupont-Durand;;;VIREMENT BELFIUS MOBILE VERS BE54 5556 5758 5960  Dupont-Durand Transfert moitie don Dupont-Delfosse  REF. : 090807060C040 VAL. 26-06;26/06/2023;-50,03;EUR;ARSPBE22;;Transfert moitie don Dupont-Delfosse\n\
                      \"

exampleUnstructuredDataWithout :: UnstructuredData
exampleUnstructuredDataWithout = UnstructuredData {
    udHeaders = [UnstructuredHeader {
                   uhLine = 1,
                   uhKey = "Date de comptabilisation à partir de",
                   uhValue = "01/05/2023"
                 }, UnstructuredHeader {
                   uhLine = 2,
                   uhKey = "Date de comptabilisation jusqu'au",
                   uhValue = "30/06/2023"
                 }, UnstructuredHeader {
                   uhLine = 3,
                   uhKey = "Montant à partir de",
                   uhValue = ""
                 }, UnstructuredHeader {
                   uhLine = 4,
                   uhKey = "Montant jusqu'à",
                   uhValue = ""
                 }, UnstructuredHeader {
                   uhLine = 5,
                   uhKey = "Numéro d'extrait à partir de",
                   uhValue = ""
                 }, UnstructuredHeader {
                   uhLine = 6,
                   uhKey = "Numéro d'extrait jusqu'au",
                   uhValue = ""
                 }, UnstructuredHeader {
                   uhLine = 7,
                   uhKey = "Communication",
                   uhValue = ""
                 }, UnstructuredHeader {
                   uhLine = 8,
                   uhKey = "Nom contrepartie contient",
                   uhValue = ""
                 }, UnstructuredHeader {
                   uhLine = 9,
                   uhKey = "Compte contrepartie",
                   uhValue = ""
                 }, UnstructuredHeader {
                   uhLine = 10,
                   uhKey = "Dernier solde",
                   uhValue = "1234.56 EUR"
                 }, UnstructuredHeader {
                   uhLine = 11,
                   uhKey = "Date/heure du dernier solde",
                   uhValue = "19/06/2023 15:16:12"
                 }],
    udColumnNames = ["Compte", "Date de comptabilisation", "Numéro d'extrait", "Numéro de transaction", "Compte contrepartie", "Nom contrepartie contient", "Rue et numéro", "Code postal et localité", "Transaction", "Date valeur", "Montant", "Devise", "BIC", "Code pays", "Communications"],
    udData = [(14, ["BE51 0607 0809 1011", "19/06/2023", "", "", "", "MAGASIN", "", "9999 PARIS", "PAIEMENT MAESTRO 18/06 MAGASIN PARIS BE  13,99 EUR CARTE N° 5152 5354 5556 5758 - MUSTERMANN  FRANK  REF. : 0123Q5W789012 VAL. 19-06", "19/06/2023", "-13,99", "EUR", "", "BE", "PAIEMENT MAESTRO 18/06 MAGASIN PARIS BE  13,99 EUR CARTE N° 5152 5354 5556 5758 - MUSTERMANN  FRANK  REF. : 0123Q5W789012 VAL. 19-06"]), (15, ["BE51 0607 0809 1011", "26/06/2023", "", "", "", "HOWARD CURRYP2P MOBILE", "", "", "PAIEMENT VIA VOTRE APP MOBILE BANKING OU VOTRE         BANCONTACT-APP A HOWARD CURRYP2P MOBILE LE 24/06 A 20:03 AVEC VOTRE CARTE BANCAIRE 5152 5354 5556 5758    REF. : 6667774448888 VAL. 24-06", "24/06/2023", "-18,06", "EUR", "", "", "PAIEMENT VIA VOTRE APP MOBILE BANKING OU VOTRE         BANCONTACT-APP A HOWARD CURRYP2P MOBILE LE 24/06 A 20:03 AVEC VOTRE CARTE BANCAIRE 5152 5354 5556 5758    REF. : 6667774448888 VAL. 24-06"]), (16, ["BE51 0607 0809 1011", "21/06/2023", "", "", "777-7777777-77", "BANKSYS", "", "", "ARGENT RECU VIA VOTRE APP MOBILE BANKING OU VOTRE  BANCONTACT-APP LE 20/06 SUR VOTRE CARTE BANCAIRE.  REF. : 123456L654321 VAL. 21-06", "21/06/2023", "11,60", "EUR", "AXABBE22", "", "ARGENT RECU VIA VOTRE APP MOBILE BANKING OU VOTRE  BANCONTACT-APP LE 20/06 SUR VOTRE CARTE BANCAIRE.  REF. : 123456L654321 VAL. 21-06"]), (17, ["BE51 0607 0809 1011", "26/06/2023", "", "", "BE54 5556 5758 5960", "Dupont-Durand", "", "", "VIREMENT BELFIUS MOBILE VERS BE54 5556 5758 5960  Dupont-Durand Transfert moitie don Dupont-Delfosse  REF. : 090807060C040 VAL. 26-06", "26/06/2023", "-50,03", "EUR", "ARSPBE22", "", "Transfert moitie don Dupont-Delfosse"])]
                 }
                 }
