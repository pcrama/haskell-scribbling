module ArgentaParserTests (
  argentaParserSpecs,
  ) where

import qualified Data.Text as T
import           Data.Time.Calendar (fromGregorian)
import           Test.Hspec
import           Text.Parsec (runParser)

import           Lib

argentaParserSpecs :: SpecWith ()
argentaParserSpecs = describe "src/ArgentaParser" $ do
  describe "low-level parsing functions" $ do
    it "2x2 matrix" $
      Lib.runArgentaUnstructuredDataParser "2x2 matrix" "h1,h2\nv1,\"1,2\"" `shouldBe` Right (Lib.mkArgentaUnstructuredData ["h1","h2"] [(2,["v1","1,2"])])
    let isLeft x = case x of
          Left _ -> True
          Right _ -> False
    it "fails if there are too many headers" $
      isLeft (Lib.runArgentaUnstructuredDataParser "too many headers" "h1,h2\nv1") `shouldBe` True
    it "fails if there aren't enough headers" $
      isLeft (Lib.runArgentaUnstructuredDataParser "insufficient headers" "h1\nv1,v2") `shouldBe` True
    describe "parses amounts with 1000-separator" $ do
      let testParser input expected =
            it (T.unpack input)
             $ runParser parseArgentaAmountToCents () "amountCents" input `shouldBe` Right expected
      testParser "0,00" 0
      testParser "0,01" 1
      testParser "0,13" 13
      testParser "1,03" 103
      testParser "987,56" 98756
      testParser "1.234,56" 123456
      testParser "12.345,67" 1234567
      testParser "3.212.345,67" 321234567
      testParser "-0,01" (-1)
      testParser "-0,13" (-13)
      testParser "-1,03" (-103)
      testParser "-987,56" (-98756)
      testParser "-1.234,56" (-123456)
      testParser "-12.345,67" (-1234567)
      testParser "-3.212.345,67" (-321234567)
  describe "works for examples" $ do
    let exampleUnstructuredData = mkArgentaUnstructuredData exampleUnstructuredDataHeaders exampleUnstructuredDataRows
    it "parses CSV into UnstructuredData" $
      Lib.runArgentaUnstructuredDataParser "exampleInputWithout" exampleInput `shouldBe` Right exampleUnstructuredData
    case columnsToArgenta exampleUnstructuredData of
      [Right exampleData1, Right exampleData2, Right exampleData3, Right exampleData4, Right exampleData5] -> do
        describe "example 1" $ do
          it "account" $ account exampleData1 `shouldBe` "BE12 3456 3456 3456"
          it "date" $ date exampleData1 `shouldBe` fromGregorian 2023 7 17
          it "otherAccount" $ otherAccount exampleData1 `shouldBe` Nothing
          it "otherName" $ otherName exampleData1 `shouldBe` mkNonBlankText "MAGASIN MG 99 PAR PARIS"
          it "description" $ description exampleData1 `shouldBe` mkNonBlankText "MAGASIN MG 99 PAR PARIS"
          it "amountCents" $ amountCents exampleData1 `shouldBe` -4206
          it "currency" $ currency exampleData1 `shouldBe` "EUR"
          it "identifyingComment" $ identifyingComment exampleData1 `shouldBe`
            "BE12 3456 3456 3456;17/07/2023;17/07/2023;MAGAGM;;-42,06;EUR;17/07/2023;;MAGASIN MG 99 PAR PARIS;"
        describe "example 2" $ do
          it "account" $ account exampleData2 `shouldBe` "BE12 3456 3456 3456"
          it "date" $ date exampleData2 `shouldBe` fromGregorian 2023 7 17
          it "otherAccount" $ otherAccount exampleData2 `shouldBe` mkNonBlankText "BE54 0000 0000 0000"
          it "otherName" $ otherName exampleData2 `shouldBe` mkNonBlankText "SARL BARAK"
          it "description" $ description exampleData2 `shouldBe` mkNonBlankText "SARL BARAK 14-07-2023 14:00 SAAS FEE FR 123456*******4321"
          it "amountCents" $ amountCents exampleData2 `shouldBe` -2150
          it "currency" $ currency exampleData2 `shouldBe` "EUR"
          it "identifyingComment" $ identifyingComment exampleData2 `shouldBe`
            "BE12 3456 3456 3456;17/07/2023;17/07/2023;0NCNC0;;-21,50;EUR;17/07/2023;BE54 0000 0000 0000;SARL BARAK;SARL BARAK 14-07-2023 14:00 SAAS FEE FR 123456*******4321"
        describe "example 3" $ do
          it "account" $ account exampleData3 `shouldBe` "BE12 3456 3456 3456"
          it "date" $ date exampleData3 `shouldBe` fromGregorian 2023 7 17
          it "otherAccount" $ otherAccount exampleData3 `shouldBe` mkNonBlankText "BE55 0055 0055 0055"
          it "otherName" $ otherName exampleData3 `shouldBe` mkNonBlankText "Abc Def Ghi"
          it "description" $ description exampleData3 `shouldBe` mkNonBlankText "Abc Def Ghi +++123/456/78901+++"
          it "amountCents" $ amountCents exampleData3 `shouldBe` -20100
          it "currency" $ currency exampleData3 `shouldBe` "EUR"
          it "identifyingComment" $ identifyingComment exampleData3 `shouldBe`
            "BE12 3456 3456 3456;17/07/2023;17/07/2023;GSRSRG;;-201,00;EUR;17/07/2023;BE55 0055 0055 0055;Abc Def Ghi;+++123/456/78901+++"
        describe "example 4" $ do
          it "account" $ account exampleData4 `shouldBe` "BE12 3456 3456 3456"
          it "date" $ date exampleData4 `shouldBe` fromGregorian 2023 6 28
          it "otherAccount" $ otherAccount exampleData4 `shouldBe` mkNonBlankText "BE09 1011 1213 1415"
          it "otherName" $ otherName exampleData4 `shouldBe` mkNonBlankText "EMPLOYER"
          it "description" $ description exampleData4 `shouldBe` mkNonBlankText "salary"
          it "amountCents" $ amountCents exampleData4 `shouldBe` 102003
          it "currency" $ currency exampleData4 `shouldBe` "EUR"
          it "identifyingComment" $ identifyingComment exampleData4 `shouldBe`
            "BE12 3456 3456 3456;28/06/2023;28/06/2023;ABCDEF;;1020,03;EUR;28/06/2023;BE09 1011 1213 1415;EMPLOYER;salary"
        describe "example 5" $ do
          it "account" $ account exampleData5 `shouldBe` "BE12 3456 3456 3456"
          it "date" $ date exampleData5 `shouldBe` fromGregorian 2023 6 27
          it "otherAccount" $ otherAccount exampleData5 `shouldBe` mkNonBlankText "BE98 9898 9898 9898"
          it "otherName" $ otherName exampleData5 `shouldBe` mkNonBlankText "DUPOND - Dupont"
          it "description" $ description exampleData5 `shouldBe` mkNonBlankText "Transfert"
          it "amountCents" $ amountCents exampleData5 `shouldBe` 123999999
          it "currency" $ currency exampleData5 `shouldBe` "EUR"
          it "identifyingComment" $ identifyingComment exampleData5 `shouldBe`
            "BE12 3456 3456 3456;27/06/2023;27/06/2023;XYZXYZ;;1239999,99;EUR;27/06/2023;BE98 9898 9898 9898;DUPOND - Dupont;Transfert"
      x -> it "columnsToArgenta failed" $ x `shouldBe` []

exampleInput :: T.Text
exampleInput = "Rekening,Boekdatum,Valutadatum,Referentie,Beschrijving,Bedrag,Munt,Verrichtingsdatum,Rekening tegenpartij,Naam tegenpartij,Mededeling\n\
               \BE12 3456 3456 3456,17-07-2023,17-07-2023,MAGAGM,,\"-42,06\",EUR,17-07-2023,,MAGASIN MG 99 PAR PARIS,\n\
               \BE12 3456 3456 3456,17-07-2023,17-07-2023,0NCNC0,,\"-21,50\",EUR,17-07-2023,BE54 0000 0000 0000,SARL BARAK,SARL BARAK 14-07-2023 14:00 SAAS FEE FR 123456*******4321\n\
               \BE12 3456 3456 3456,17-07-2023,17-07-2023,GSRSRG,,\"-201,00\",EUR,17-07-2023,BE55 0055 0055 0055,Abc Def Ghi,+++123/456/78901+++\n\
               \BE12 3456 3456 3456,28-06-2023,28-06-2023,ABCDEF,,\"1.020,03\",EUR,28-06-2023,BE09 1011 1213 1415,EMPLOYER,salary\n\
               \BE12 3456 3456 3456,27-06-2023,27-06-2023,XYZXYZ,,\"1.239.999,99\",EUR,27-06-2023,BE98 9898 9898 9898,DUPOND - Dupont,Transfert\n\
               \"

exampleUnstructuredDataHeaders :: [T.Text]
exampleUnstructuredDataHeaders = [
    "Rekening"
  , "Boekdatum"
  , "Valutadatum"
  , "Referentie"
  , "Beschrijving"
  , "Bedrag"
  , "Munt"
  , "Verrichtingsdatum"
  , "Rekening tegenpartij"
  , "Naam tegenpartij"
  , "Mededeling"
  ]

exampleUnstructuredDataRows :: [(Int, [T.Text])]
exampleUnstructuredDataRows = [
  (2, ["BE12 3456 3456 3456", "17-07-2023", "17-07-2023", "MAGAGM", "", "-42,06", "EUR", "17-07-2023", "", "MAGASIN MG 99 PAR PARIS", ""])
  , (3, ["BE12 3456 3456 3456", "17-07-2023", "17-07-2023", "0NCNC0", "", "-21,50", "EUR", "17-07-2023", "BE54 0000 0000 0000", "SARL BARAK", "SARL BARAK 14-07-2023 14:00 SAAS FEE FR 123456*******4321"])
  , (4, ["BE12 3456 3456 3456", "17-07-2023", "17-07-2023", "GSRSRG", "", "-201,00", "EUR", "17-07-2023", "BE55 0055 0055 0055", "Abc Def Ghi", "+++123/456/78901+++"])
  , (5, ["BE12 3456 3456 3456", "28-06-2023", "28-06-2023", "ABCDEF", "", "1.020,03", "EUR", "28-06-2023", "BE09 1011 1213 1415", "EMPLOYER", "salary"])
  , (6, ["BE12 3456 3456 3456", "27-06-2023", "27-06-2023", "XYZXYZ", "", "1.239.999,99", "EUR", "27-06-2023", "BE98 9898 9898 9898", "DUPOND - Dupont", "Transfert"])]
