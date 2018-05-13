-- stack exec --package QuickCheck -- ghci test/Spec.hs import Test.Hspec
import Test.HUnit
import Test.QuickCheck
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSize, modifyMaxSuccess)

import Data.List.NonEmpty (NonEmpty(..))
import Text.ParserCombinators.ReadP

import ParseRipSpec

instance Arbitrary CDRipSpec where
  arbitrary = sized $ \size ->
    case size of
      0 -> do
             k <- elements [Title, Artist, Album, Track, Total, Genre]
             v <- elements ["ab", "cd", "efg", "hi"]
             return CRS { info = (k, v) :| [], overrides = [] }
      _ -> do
        lftSize <- choose (1, size)
        let rgtSize = max 0 $ size - lftSize - 1
        let f (CRS { info = (k, v) :| _ }) = (k, v)
        kvs <- mapM (fmap f . resize 0) $ replicate lftSize arbitrary
        overs <- mapM (resize $ min 3 $ rgtSize `div` 2)
                    $ replicate rgtSize arbitrary
        return $ CRS { info = head kvs :| tail kvs
                     , overrides = overs }
  shrink input@(CRS { info = hd :| tl, overrides = overs }) =
             case (null tl, null overs) of
               (True, True) -> []
               (True, False) -> [shrunkOvers]
               (False, True) -> [shrunkTail]
               (False, False) -> [shrunkBoth, shrunkOvers, shrunkTail]
           where shrunkOvers = input { overrides = [] }
                 shrunkTail = input { info = hd :| [] }
                 shrunkBoth = CRS { info = hd :| [], overrides = [] }

-- Property: expanding (A) or (B) generates the same list.
-- (A) - Title : t            (B) - Title : t
--       - Album : a                Album : a
--         - Genre : g              Genre : g
prop_NestedOrFlat :: CDRipSpec -> Bool
prop_NestedOrFlat x =
    let tFlat = expandOverrides emptyTrackRipSpec x
        deepened = deepen x
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

testParseCRS indent s exp =
  it ("should work for '" ++ s ++ "'")
   $ assertEqual ("testParseCRS of " ++ s)
                 [(exp, "")]
               $ readP_to_S (parseCRS indent) s

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
