{-# LANGUAGE RecordWildCards #-}
-- stack exec --package QuickCheck -- ghci test/Spec.hs import Test.Hspec
import Control.Monad (forM_)
import Debug.Trace
import Data.List.NonEmpty (NonEmpty(..))
import Text.ParserCombinators.ReadP (ReadP, readP_to_S)
import Test.HUnit
import Test.QuickCheck
import Test.Hspec

import ParseRipSpec
import Shell

trSh :: Show a => String -> a -> a
trSh p x = trace (p ++ "=<" ++ show x ++ ">") x

-- Property: expanding (A) or (B) generates the same list.
-- (A) - Title : t            (B) - Title : t
--       - Album : a                Album : a
--         - Genre : g              Genre : g
prop_NestedOrFlat :: CDRipSpec -> Bool
prop_NestedOrFlat cdRS =
    let tFlat = expandOverrides emptyTrackRipSpec cdRS
        deepened = deepen cdRS
        tDeep = expandOverrides emptyTrackRipSpec deepened
    in tFlat == tDeep
  where deepen :: CDRipSpec -> CDRipSpec -- transform (B) to (A)
        deepen c@(CRS { info = _ :| [] }) = c
        deepen (CRS { info = x :| (y:ys), overrides = os }) =
                 CRS { info = x :| []
                     , overrides = [deepen $ CRS { info = y :| ys
                                                 , overrides = os }] }

prop_Reorder :: CDRipSpec -> Bool
prop_Reorder x@(CRS { overrides = o }) =
     (reverse $ expandOverrides emptyTrackRipSpec x)
  == (expandOverrides emptyTrackRipSpec $ x { overrides = reverse o })

prop_Idempotent :: CDRipSpec -> Bool
prop_Idempotent x =
  let expandedOnce = expandOverrides emptyTrackRipSpec x :: [TrackRipSpec]
      expandedTwice = map (\e -> expandOverrides e x) expandedOnce :: [[TrackRipSpec]]
  in and $ [ expandedOnce !! i == (expandedTwice !! i) !! i
           | i <- [0..length expandedOnce - 1] ]

-- This property is weakened by the usage of PreciseTrackRipSpec in
-- translateSpec: each sublist of CDRipSpec needs to have an explicit
-- (Track, "<integer>") at least in the first element to really have
-- ``concatMap translateSpec == translateSpec . concat''
prop_Translate :: Int -> [[CDRipSpec]] -> Bool
prop_Translate maxLen x =
  let xShortened = map (take maxLen) $ take maxLen x
      concatThenTranslate = translateSpec $ concatMap constifyTrackNumber xShortened
      translateThenConcat = concatMap (translateSpec . constifyTrackNumber) xShortened
      -- recursively descend into the first CDRipSpec to make sure there is
      -- (or impose) a Track starting number
      constifyTrackNumber :: [CDRipSpec] -> [CDRipSpec]
      constifyTrackNumber [] = []
      constifyTrackNumber allXs@(x:xs)
        | hasFixedTrackNumber x = allXs
        | otherwise = case overrides x of
            [] -> let hd :| tl = info x in (x { info = (Track, "1") :| (hd:tl) }):xs
            ovs@(_:_) -> x { overrides = constifyTrackNumber ovs }:xs
      -- recursively descend into the first CDRipSpec to see if there is
      -- a Track starting number
      hasFixedTrackNumber :: CDRipSpec -> Bool
      hasFixedTrackNumber (CRS { .. }) = case overrides of
        [] -> any ((== Track) . fst) info
        (x:_) -> hasFixedTrackNumber x
  in concatThenTranslate == translateThenConcat

prop_OnlySafeNames :: PreciseTrackRipSpec -> Bool
prop_OnlySafeNames = and . map isSafeCharForShell . safeTrackName
  where isSafeCharForShell = not . (`elem` " \n\t|&;()<>'\"[]{}")

makeCharPredicate :: [Char] -> (Char -> Bool)
makeCharPredicate [] x = x < 'W'
makeCharPredicate [x] y = x < y
makeCharPredicate [x, y] z
  | x <= y = x <= z && z <= y
  | otherwise = z < y || x < z
makeCharPredicate (x:y:_) z = makeCharPredicate [x, y] z

prop_breakOnPredicateReassemble :: [Char] -> [Char] -> Bool
prop_breakOnPredicateReassemble p s =
    let predicate = makeCharPredicate p
        broken = breakOnPredicate predicate s
    in concat broken == s

prop_breakOnPredicateAlternatingPredForParts :: [Char] -> [Char] -> Bool
prop_breakOnPredicateAlternatingPredForParts p s =
    let predicate = makeCharPredicate p
        broken = breakOnPredicate predicate s
        predResult = map (map predicate) broken
    in (   (not $ or $ map (== []) broken)
        && (and $ map constantPredResult predResult)
        && alternatingPredResult predResult)
  where constantPredResult [] = False -- empty lists are not allowed
        constantPredResult [_] = True
        constantPredResult xs = allTrue xs || allFalse xs
        allTrue = and
        allFalse = not . or
        alternatingPredResult [] = True
        alternatingPredResult [_] = True
        alternatingPredResult ((x:_):ys@((y:_):_)) = x /= y && alternatingPredResult ys
        -- input should never contain empty sublists, but the
        -- compiler doesn't know
        alternatingPredResult _ = False

filterCompleteParse :: [(a, [b])] -> [(a, [b])]
filterCompleteParse = filter (null . snd)

assertParseFails :: (Eq a, Show a) => String -> ReadP a -> String -> Assertion
assertParseFails msg parser input =
  assertEqual msg [] $ filterCompleteParse $ readP_to_S parser input

testParseCellData :: String -> String -> SpecWith ()
testParseCellData s expected =
  it ("should accept cell data " ++ show s)
   $ let obs = readP_to_S parseCellData s
     in assertEqual (show obs) [(expected, "")] $ filterCompleteParse obs

testParseCRSDataRow :: String -> NonEmpty Key -> NonEmpty (Key, String) -> SpecWith ()
testParseCRSDataRow s headers expected =
  it ("should accept cells " ++ show s ++ " for " ++ show headers)
   $ let obs = readP_to_S (parseCRSDataRow headers) s
     in assertEqual (show obs) [(expected, "")] $ filterCompleteParse obs

testParseCRSDataRowFail :: String -> NonEmpty Key -> SpecWith ()
testParseCRSDataRowFail s headers =
  it ("should reject cells " ++ show s ++ " for " ++ show headers)
   $ assertParseFails "returned parse success" (parseCRSDataRow headers) s

testParseCRSHeaderRow :: String -> NonEmpty Key -> SpecWith ()
testParseCRSHeaderRow s expected =
  it ("parses header row " ++ show s ++ " as " ++ show expected)
   $ let obs = readP_to_S parseCRSHeaderRow s
     in assertEqual (show obs) [(expected, "")] $ filterCompleteParse obs

testParseCRSTable :: Int -> [Char] -> [CDRipSpec] -> SpecWith ()
testParseCRSTable indent s expected =
  it ("should parse a table indented " ++ show indent)
   $ assertEqual ("testParseCRSTable " ++ show indent)
                 [(expected, "")]
               $ filterCompleteParse $ readP_to_S (parseCRSTable indent) s

testParseCRSTableFail :: String -> Int -> String -> SpecWith ()
testParseCRSTableFail message indent s =
  it ("should reject a table indented " ++ show indent ++ " [" ++ message ++ "]")
   $ assertParseFails message (parseCRSTable indent) s

testParseCRS :: Int -> [Char] -> CDRipSpec -> SpecWith ()
testParseCRS indent s expected =
  it ("should work for '" ++ s ++ "'")
   $ assertEqual ("testParseCRS of " ++ s)
                 [(expected, "")]
               $ filterCompleteParse $ readP_to_S (parseCRS indent) s

testParseCRSfail :: Int -> [Char] -> SpecWith ()
testParseCRSfail indent s =
  it ("should fail for '" ++ s ++ "'")
   $ assertEqual ("testParseCRS of " ++ s)
                 []
               $ readP_to_S (parseCRS indent) s

main :: IO ()
main = hspec $ do
  describe "parseCRS" $ do
    testParseCRS 0 "- album: a" $ CRS { info = (Album, "a") :| []
                                      , overrides = [] }
    testParseCRS 0 "- artist: b" $ CRS { info = (Artist, "b") :| []
                                       , overrides = [] }
    testParseCRS 2 "  - Album: a" $ CRS { info = (Album, "a") :| []
                                        , overrides = [] }
    testParseCRS 4 "    - ARTIST: b" $ CRS { info = (Artist, "b") :| []
                                           , overrides = [] }
    testParseCRS 0 "- track: t" $ CRS { info = (Track, "t") :| []
                                      , overrides = [] }
    testParseCRS 0 "- title:  this is a title  " $ CRS { info = (Title, "this is a title") :| []
                                                 , overrides = [] }
    testParseCRS 1 " - total:  2 " $ CRS { info = (Total, "2") :| []
                                         , overrides = [] }
    testParseCRS 3 "   -    Genre   :  g:g " $ CRS { info = (Genre, "g:g") :| []
                                                   , overrides = [] }
    testParseCRS 0 "-  album: a\n   Genre :  g" $ CRS { info = (Album, "a") :| [(Genre, "g")]
                                                      , overrides = [] }
    testParseCRS 1
                 " - album: a\n   Genre :  g\n   - artist: 1\n   - artist: 2\n     title : t"
               $ CRS { info = (Album, "a") :| [(Genre, "g")]
                     , overrides = [CRS { info = (Artist, "1") :| [], overrides = [] }
                                   , CRS { info = (Artist, "2") :| [(Title, "t")]
                                         , overrides = [] }] }
    testParseCRSfail 0 "-  unknown key   :  g:g "
    testParseCRSfail 2 "-  Title   :  dedent "
    testParseCRSfail 0 "  -  Title   :  indent "
  describe "parseCellData" $ do
    testParseCellData "   " ""
    testParseCellData " A  " "A"
    testParseCellData " Ab  " "Ab"
    testParseCellData " A b  " "A b"
  describe "parseCRSDataRow" $ do
    testParseCRSDataRow " A | b |  c  | d"
                        (Genre :| [Album, Artist, Title])
                      $ (Genre, "A") :| [(Album, "b"), (Artist, "c"), (Title, "d")]
    testParseCRSDataRow " A " (Genre :| []) $ (Genre, "A") :| []
    testParseCRSDataRowFail " A " (Genre :| [Album])
    testParseCRSDataRowFail " A | b | c " (Genre :| [Album])
    forM_ [(minBound :: Key)..maxBound] $ \k ->
      case k of
        Track -> -- Exception: Track may be empty
          testParseCRSDataRow " A | | b "
                              (Genre :| [Track, Album])
                            $ (Genre, "A") :| [(Track, ""), (Album, "b")]
        _ -> do -- All other keys may not be empty
          testParseCRSDataRowFail " A | " (Track :| [k])
          testParseCRSDataRowFail " | A " (k :| [Track])
  describe "parseCRSHeaderRow" $ do
    testParseCRSHeaderRow "album | artist" $ Album :| [Artist]
    testParseCRSHeaderRow "album   |   artist" $ Album :| [Artist]
    testParseCRSHeaderRow "gEnRe" $ Genre :| []
  describe "parseCRSTable" $ do
    testParseCRSTable 0
                      "| album     | artist     |\n| Breakfast | Supertramp |\n| Dinner    | Tramp      |"
                      $ [CRS { info = (Album, "Breakfast") :| [(Artist, "Supertramp")]
                             , overrides = [] }
                        , CRS { info = (Album, "Dinner") :| [(Artist, "Tramp")]
                              , overrides = [] }]
    testParseCRSTable 2
                      "  | album     | artist     |\n  | Breakfast | Supertramp |\n  | Dinner    | Tramp      |"
                      $ [CRS { info = (Album, "Breakfast") :| [(Artist, "Supertramp")]
                             , overrides = [] }
                        , CRS { info = (Album, "Dinner") :| [(Artist, "Tramp")]
                              , overrides = [] }]
    testParseCRSTable 0
                      "| album     | TracK      |\n| Breakfast | Supertramp |\n| Empty     |            |"
                      $ [CRS { info = (Album, "Breakfast") :| [(Track, "Supertramp")]
                             , overrides = [] }
                        , CRS { info = (Album, "Empty") :| [(Track, "")]
                              , overrides = [] }]
    testParseCRSTableFail "Column mismatch" 0 "| album | artist |\n| A |"
    testParseCRSTableFail "Indent grows" 2 "  | album | artist |\n   | A | b |"
    testParseCRSTableFail "Indent shrinks" 2 "  | album | artist |\n | A | b |"
  describe "nested table in CRS" $ do
    testParseCRS 0
                 "- album: a\n  | genre | title |\n  | g | t |\n  | h | i |"
               $ CRS { info = (Album, "a") :| []
                     , overrides = [CRS { info = (Genre, "g") :| [(Title, "t")]
                                        , overrides = [] }
                                   , CRS { info = (Genre, "h") :| [(Title, "i")]
                                          , overrides = [] }] }
    testParseCRS 2
                 "  - album: a\n    | genre | title |\n    | g | t |\n    | h | ij |"
               $ CRS { info = (Album, "a") :| []
                     , overrides = [CRS { info = (Genre, "g") :| [(Title, "t")]
                                        , overrides = [] }
                                   , CRS { info = (Genre, "h") :| [(Title, "ij")]
                                          , overrides = [] }] }
    testParseCRS 0
                 "- album: a\n  total: 4\n  | track | genre | title |\n  | 1 | g | t |\n  | | h | i |"
               $ CRS { info = (Album, "a") :| [(Total, "4")]
                     , overrides = [CRS { info = (Track, "1") :| [(Genre, "g"), (Title, "t")]
                                        , overrides = [] }
                                   , CRS { info = (Track, "") :| [(Genre, "h"), (Title, "i")]
                                          , overrides = [] }] }
  describe "expandOverrides" $ do
    it "of nested or flattened info is same" $
      property prop_NestedOrFlat
    it "of reversed overrides is reverse of expanded of overrides" $
      property prop_Reorder
    it "is idempotent" $
      property prop_Idempotent
    it "handles an example found (& fixed) by prop_NestedOrFlat" $
      let input = CRS { info = (Title, "discarded") :| [(Title, "kept")]
                      , overrides = [] }
          result = expandOverrides emptyTrackRipSpec input
          expected = expandOverrides emptyTrackRipSpec
                                   $ CRS { info = (Title, "kept") :| []
                                         , overrides = [] }
      in assertEqual "Latest key overrules prior keys"
                     expected
                     result
  describe "translateSpec" $
    it "handles a list of specs" $
      property $ prop_Translate 10
  describe "safe file name" $
    it "generates only safe names" $
      property prop_OnlySafeNames
  describe "breakOnPredicate" $ do
    it "breaks into parts that can be reassembled without loss" $
      property prop_breakOnPredicateReassemble
    it "results in parts with alternating truth values for the predicate" $
      property prop_breakOnPredicateAlternatingPredForParts
