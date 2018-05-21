-- stack exec --package QuickCheck -- ghci test/Spec.hs import Test.Hspec
import Debug.Trace
import Data.List.NonEmpty (NonEmpty(..))
import Text.ParserCombinators.ReadP (readP_to_S)
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

prop_Translate :: Int -> [[CDRipSpec]] -> Bool
prop_Translate maxLen x =
  let xShortened = take maxLen $ map (take maxLen) x
      concatThenTranslate = translateSpec $ concat xShortened
      translateThenConcat = concatMap translateSpec xShortened
  in concatThenTranslate == translateThenConcat

prop_OnlySafeNames :: TrackRipSpec -> Bool
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

testParseCRS :: Int -> [Char] -> CDRipSpec -> SpecWith ()
testParseCRS indent s expected =
  it ("should work for '" ++ s ++ "'")
   $ assertEqual ("testParseCRS of " ++ s)
                 [(expected, "")]
               $ readP_to_S (parseCRS indent) s

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
    testParseCRSfail 0 "-  unknown key   :  g:g "
    testParseCRSfail 2 "-  Title   :  dedent "
    testParseCRSfail 0 "  -  Title   :  indent "
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
