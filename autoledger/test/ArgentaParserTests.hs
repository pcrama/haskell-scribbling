module ArgentaParserTests (
  argentaParserSpecs,
  ) where

import           Codec.Xlsx (Cell(..), CellValue(..), RowIndex(..))
import           Codec.Xlsx.Parser.Stream
import qualified Data.Text as T
import           Data.Time.Calendar (fromGregorian)
import qualified Data.IntMap.Strict as IntMap
import           Test.Hspec
import           Text.Parsec (runParser)

import           Lib
import qualified ArgentaParser as Argenta

argentaParserSpecs :: SpecWith ()
argentaParserSpecs = describe "src/ArgentaParser" $ do
  describe "low-level parsing functions" $ do
    describe "parses amounts with 1000-separator" $ do
      let testParser input expected =
            it (T.unpack input)
             $ runParser Argenta.parseAmountToCents () "amountCents" input `shouldBe` Right expected
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
  describe "getArgentaDescription" $ do
    let testDesc comm tran otherNam otherAcc expected = it (show (comm, tran, otherNam, otherAcc)) $
          (Argenta.getArgentaDescription
           $ Argenta.ArgentaTransaction {
              Argenta._account = mempty
              , Argenta._accountingDate = fromGregorian 1970 1 1
              , Argenta._valueDate = fromGregorian 1970 1 1
              , Argenta._reference = uninitializedNonBlankText
              , Argenta._amountCents = 0
              , Argenta._currency = mempty
              , Argenta._transactionDate = fromGregorian 1970 1 1
              , Argenta._otherAccount = mkNonBlankText otherAcc
              , Argenta._communication = mkNonBlankText comm
              , Argenta._otherName = mkNonBlankText otherNam
              , Argenta._transactionDescription = mkNonBlankText tran
              }
          ) `shouldBe` mkNonBlankText expected
    testDesc "BOUCHERIE  SANZOS 31-08-2023 13:00  LIBIN BE  123456*******7890" "Betaling Maestro" " Boucherie Sanzos" "" "BOUCHERIE SANZOS 31-08-2023 13:00 LIBIN BE 123456*******7890"
    testDesc "I   P" "Doorlopende betalingsopdracht" " DUPONT " "" "I P DUPONT"
    testDesc "" "Doorlopende   betalingsopdracht" "" "" "Doorlopende betalingsopdracht"
    testDesc "" "Doorlopende   betalingsopdracht" "" "BE 123456*******7890" "BE 123456*******7890"
    testDesc "+++123/4567/98765+++" "Doorlopende betalingsopdracht" "B  A " "" "B A +++123/4567/98765+++"
    testDesc "+++123/4567/98765+++" "Doorlopende betalingsopdracht" "B  A " "BE 123456*******7890" "B A +++123/4567/98765+++"
    testDesc "/A/  W/S  00/20" "Inkomende overschrijving" "" "BE  123456*******7890" "/A/ W/S 00/20 BE 123456*******7890"
    testDesc "Dupont,  Martin,  trombone" "Uitgaande overschrijving" "Ecole Muzikk" "" "Dupont, Martin, trombone Ecole Muzikk"
    testDesc "Dupont,  Chloe,  harpe" "Uitgaande instantoverschrijving" "Ecole Muzikk" "" "Dupont, Chloe, harpe Ecole Muzikk"
    testDesc "" "Betaling Bancontact" "1000PATTES S.A.          LIBIN" "" "1000PATTES S.A. LIBIN"
  describe "works for examples" $ do
    describe "parses xlsx into UnstructuredData" $ do
      let unstructured = Lib.parseXlsxRows exampleInput
      it "gets column names" $ Argenta.udColumnNames <$> unstructured `shouldBe` (Right exampleUnstructuredDataHeaders)
      let expectedDataRowCount = length exampleInput - 1
      it ("gets " <> show expectedDataRowCount <> " data rows") $
        (length . Argenta.udData) <$> unstructured `shouldBe` (Right expectedDataRowCount)
    case columnsToArgenta $ Argenta.UnstructuredData { Argenta.udColumnNames = exampleUnstructuredDataHeaders
                                                     , Argenta.udData = tail exampleInput } of
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
            "BE12 3456 3456 3456;17/07/2023;17/07/2023;GSRSRG;Betaling Maestro;-201,00;EUR;17/07/2023;BE55 0055 0055 0055;Abc Def Ghi;+++123/456/78901+++"
        describe "example 4" $ do
          it "account" $ account exampleData4 `shouldBe` "BE12 3456 3456 3456"
          it "date" $ date exampleData4 `shouldBe` fromGregorian 2023 6 28
          it "otherAccount" $ otherAccount exampleData4 `shouldBe` mkNonBlankText "BE09 1011 1213 1415"
          it "otherName" $ otherName exampleData4 `shouldBe` mkNonBlankText "EMPLOYER"
          it "description" $ description exampleData4 `shouldBe` mkNonBlankText "salary EMPLOYER"
          it "amountCents" $ amountCents exampleData4 `shouldBe` 102003
          it "currency" $ currency exampleData4 `shouldBe` "EUR"
          it "identifyingComment" $ identifyingComment exampleData4 `shouldBe`
            "BE12 3456 3456 3456;28/06/2023;28/06/2023;ABCDEF;;1020,03;EUR;28/06/2023;BE09 1011 1213 1415;EMPLOYER;salary"
        describe "example 5" $ do
          it "account" $ account exampleData5 `shouldBe` "BE12 3456 3456 3456"
          it "date" $ date exampleData5 `shouldBe` fromGregorian 2023 6 27
          it "otherAccount" $ otherAccount exampleData5 `shouldBe` mkNonBlankText "BE98 9898 9898 9898"
          it "otherName" $ otherName exampleData5 `shouldBe` mkNonBlankText "DUPOND - Dupont"
          it "description" $ description exampleData5 `shouldBe` mkNonBlankText "Transfert DUPOND - Dupont"
          it "amountCents" $ amountCents exampleData5 `shouldBe` 123999999
          it "currency" $ currency exampleData5 `shouldBe` "EUR"
          it "identifyingComment" $ identifyingComment exampleData5 `shouldBe`
            "BE12 3456 3456 3456;27/06/2023;27/06/2023;XYZXYZ;;1239999,99;EUR;27/06/2023;BE98 9898 9898 9898;DUPOND - Dupont;Transfert"
      x -> it "columnsToArgenta failed" $ x `shouldBe` []

nothingCell :: Cell
nothingCell = Cell {_cellStyle = Nothing, _cellValue = Nothing, _cellComment = Nothing, _cellFormula = Nothing}

textCell :: T.Text -> Cell
textCell txt = Cell {_cellStyle = Nothing, _cellValue = Just (CellText txt), _cellComment = Nothing, _cellFormula = Nothing}

doubleCell :: Double -> Cell
doubleCell dbl = Cell {_cellStyle = Just 1, _cellValue = Just (CellDouble dbl), _cellComment = Nothing, _cellFormula = Nothing} 

dateCell :: Double -> Cell
dateCell dbl = Cell {_cellStyle = Just 2, _cellValue = Just (CellDouble dbl), _cellComment = Nothing, _cellFormula = Nothing} 

makeRow :: Int -> [(Int, Cell)] -> Row
makeRow row cells = MkRow {_ri_row_index = RowIndex row
                          , _ri_cell_row = IntMap.fromList cells}
  
exampleInput :: [Row]
exampleInput =
    [makeRow 1 $ zip [1..]
                     [textCell "Rekening"
                     ,textCell "Boekdatum"
                     ,textCell "Valutadatum"
                     ,textCell "Referentie"
                     ,textCell "Beschrijving"
                     ,textCell "Bedrag"
                     ,textCell "Munt"
                     ,textCell "Verrichtingsdatum"
                     ,textCell "Rekening tegenpartij"
                     ,textCell "Naam tegenpartij"
                     ,textCell "Mededeling"]
    ,makeRow 2 $ filter isNotEmptyTextCell
               $ zip [1..]
                     [textCell "BE12 3456 3456 3456"
                     ,dateCell 45124.0 -- 2023-07-17
                     ,dateCell 45124.0 -- 2023-07-17
                     ,textCell "MAGAGM"
                     ,textCell ""
                     ,doubleCell (-42.06)
                     ,textCell "EUR"
                     ,dateCell 45124.0 -- 2023-07-17
                     ,textCell ""
                     ,textCell "MAGASIN MG 99 PAR PARIS"
                     ,textCell ""]
    ,makeRow 3 $ zip [1..]
                     [textCell "BE12 3456 3456 3456"
                     ,dateCell 45124.0 -- 17-07-2023
                     ,dateCell 45124.0 -- 17-07-2023
                     ,textCell "0NCNC0"
                     ,nothingCell -- textCell ""
                     ,doubleCell (-21.50)
                     ,textCell "EUR"
                     ,dateCell 45124.0 -- 17-07-2023
                     ,textCell "BE54 0000 0000 0000"
                     ,textCell "SARL BARAK"
                     ,textCell "SARL BARAK 14-07-2023 14:00 SAAS FEE FR 123456*******4321"]
    ,makeRow 4 $ zip [1..]
                     [textCell "BE12 3456 3456 3456"
                     ,dateCell 45124.0 -- 17-07-2023
                     ,dateCell 45124.0 -- 17-07-2023
                     ,textCell "GSRSRG"
                     ,textCell "Betaling Maestro"
                     ,textCell "-201,00" -- doubleCell (-201.00)
                     ,textCell "EUR"
                     ,dateCell 45124.0 -- 17-07-2023
                     ,textCell "BE55 0055 0055 0055"
                     ,textCell "Abc Def Ghi"
                     ,textCell "+++123/456/78901+++"]
    ,makeRow 5 $ filter isNotEmptyTextCell
               $ zip [1..]
                     [textCell "BE12 3456 3456 3456"
                     ,dateCell 45105.0 -- 28-06-2023
                     ,dateCell 45105.0 -- 28-06-2023
                     ,textCell "ABCDEF"
                     ,textCell ""
                     ,doubleCell 1020.03
                     ,textCell "EUR"
                     ,dateCell 45105.0 -- 28-06-2023
                     ,textCell "BE09 1011 1213 1415"
                     ,textCell "EMPLOYER"
                     ,textCell "salary"]
    ,makeRow 6 $ zip [1..]
                     [textCell "BE12 3456 3456 3456"
                     ,dateCell 45104.0 -- 27-06-2023
                     ,dateCell 45104.0 -- 27-06-2023
                     ,textCell "XYZXYZ"
                     ,textCell "      "
                     ,doubleCell 1239999.99
                     ,textCell "EUR"
                     ,dateCell 45104.0 -- 27-06-2023
                     ,textCell "BE98 9898 9898 9898"
                     ,textCell "DUPOND - Dupont"
                     ,textCell "Transfert"]]

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

isNotEmptyTextCell :: (Int, Cell) -> Bool
isNotEmptyTextCell (_, Cell {_cellValue = Just (CellText txt)}) = not $ T.null txt
isNotEmptyTextCell (_, _) = True
