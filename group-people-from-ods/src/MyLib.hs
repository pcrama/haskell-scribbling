module MyLib (
  groups
  , odsByteString
  , printGroups
  , readPeople
  , readRows
  )
where

import qualified Codec.Archive.Zip as Zip
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Maybe (fromMaybe)
import           Text.XML

odsByteString :: String -> IO (Either T.Text BL.ByteString)
odsByteString fname = do
    ods <- BL.readFile fname

    let archive = Zip.toArchive ods

    case Zip.findEntryByPath "content.xml" archive of
        Nothing -> return $ Left "The ODS file does not contain content.xml"
        Just entry -> return $ Right $ Zip.fromEntry entry


data Answer
    = Yes
    | No
    | Unknown
    | Missing
    deriving (Eq, Show)

data Person = Person
    { personLastName  :: T.Text
    , personFirstName :: T.Text
    , acceptPrivateGroup :: Answer
    , refusePrivateGroup :: Answer
    , acceptPortrait :: Answer
    , acceptGroupPhoto :: Answer
    , acceptAudioRecording :: Answer
    , acceptPrivateMediaExchange :: Answer
    , acceptPublicMediaUsage :: Answer
    , acceptDiffusionRight :: Answer
    , acceptPublicMediaUsage2 :: Answer
    , refusePublicMediaUsage2 :: Answer
    } deriving (Show)


personName :: Person -> T.Text
personName (Person {personLastName=lst, personFirstName=frst}) = frst <> " " <> lst

-- Convert spreadsheet text to our domain type.
parseAnswer :: T.Text -> Answer
parseAnswer t =
    case T.toLower (T.strip t) of
        "oui" -> Yes
        "non"  -> No
        "yes" -> Yes
        "no"  -> No
        "?"   -> Unknown
        ""    -> Missing
        _     -> Unknown


-- Get all descendant elements.
descendants :: Element -> [Element]
descendants e =
    concatMap go (elementNodes e)
  where
    go (NodeElement child) =
        child : descendants child
    go _ = []


-- Find all elements with the given local name.
elementsNamed :: T.Text -> Element -> [Element]
elementsNamed wanted e =
    [ x
    | x <- e : descendants e
    , nameLocalName (elementName x) == wanted
    ]


-- Extract visible text from an XML element.
elementText :: Element -> T.Text
elementText e =
    T.concat (map nodeText (elementNodes e))
  where
    nodeText (NodeContent t) = t
    nodeText (NodeElement x) = elementText x
    nodeText _               = ""


-- ODS can say that one cell represents N repeated cells.
--
-- For example:
--
--   <table:table-cell table:number-columns-repeated="3"/>
--
-- means three consecutive empty cells.
cellValues :: Element -> [T.Text]
cellValues cell =
    let repeated =
            case attr ((elementName cell){nameLocalName="number-columns-repeated"}) cell of
                Nothing -> 1
                Just n  -> read (T.unpack n)

        value = T.strip (elementText cell)
    in replicate repeated value



attr :: Name -> Element -> Maybe T.Text
attr wanted e = Map.lookup wanted $ elementAttributes e


-- Read every table row from content.xml.
readRows :: BL.ByteString -> [[T.Text]]
readRows bytes =
    let document = parseLBS_ def bytes
        root     = documentRoot document
        rows     = elementsNamed "table-row" root
    in map readRow rows
  where
    readRow row =
        concatMap cellValues
            [ cell
            | cell <- descendants row
            , nameLocalName (elementName cell) == "table-cell"
            ]

shortenRow :: [T.Text] -> [T.Text]
shortenRow = go 0
  where go count ("":tl) = go (count + 1) tl
        go _ [] = []
        go count (x:tl) = (replicate count "") <> (x:go 0 tl)

acceptPrivateGroupTxt, refusePrivateGroupTxt, acceptPortraitTxt, acceptGroupPhotoTxt, acceptAudioRecordingTxt, acceptPrivateMediaExchangeTxt, acceptPublicMediaUsageTxt, acceptDiffusionRightTxt, acceptPublicMediaUsage2Txt, refusePublicMediaUsage2Txt :: T.Text
acceptPrivateGroupTxt = "J'accepte que mes coordonn\233es personnelles soient utilis\233es pour la cr\233ation de groupes priv\233s sur les r\233seaux sociaux afin de partager des communications internes \224 la SRH"
refusePrivateGroupTxt = "Je n'accepte pas ce traitement de mes coordonn\233es personnelles"
acceptPortraitTxt = "J'accepte d'\234tre photographi\233(e) et film\233(e) en gros plan lors des activit\233s de la SRH (moins de 3 personnes reconnaissables sur l\8217image)"
acceptGroupPhotoTxt = "J'accepte d'\234tre photographi\233(e) et film\233(e) en groupe lors des activit\233s de la SRH (3 personnes ou plus reconnaissables sur l\8217image)"
acceptAudioRecordingTxt = "J'accepte d'\234tre enregistr\233(e) sur tout support sonore lors des activit\233s de la SRH"
acceptPrivateMediaExchangeTxt = "J'accepte que les images, vid\233ogrammes et enregistrements sonores dans lesquels je figure soient utilis\233s par la SRH et ses membres afin d'\233changer les exp\233riences v\233cues dans le cadre de ses activit\233s et de promouvoir les activit\233s de la SRH"
acceptPublicMediaUsageTxt = "J'accepte que les photographies, vid\233ogrammes et enregistrements sonores soient post\233s sur les r\233seaux sociaux aux fins de diffusion et de promotion des activit\233s de la SRH"
acceptDiffusionRightTxt = "Je d\233laisse \224 la SRH le montant des droits d'auteur et des droits voisins qui seraient g\233n\233r\233s par l'exploitation des productions sonores et/ou audio-visuelles r\233alis\233es dans le cadre des activit\233s de la SRH"
acceptPublicMediaUsage2Txt = "J'accepte que les photographies, les vid\233ogrammes et les enregistrements dans lesquels j'apparais, soient diffus\233s par la SRH sur internet et sur les r\233seaux sociaux afin de diffuser et promouvoir les activit\233s de la SRH"
refusePublicMediaUsage2Txt = "Je n'accepte pas que les photographies, les vid\233ogrammes et les enregistrements dans lesquels j'apparais soient publi\233s ou diffus\233s sur internet et les r\233seaux sociaux"

-- Turn rows into records based on the header row.
readPeople :: [[T.Text]] -> [Person]
readPeople [] = []
readPeople (header : rows) =
    let footer = concatMap (\rw -> [ (T.strip h, i) | (i, h) <- zip [0..] rw ])
                         $ filter (acceptPrivateGroupTxt `elem`) rows
        columnIndex =
            Map.fromList $ [ (T.strip h, i) | (i, h) <- zip [0..] header ]
                           <> footer
    in filter (\p -> personLastName p /= "" || personFirstName p /= "")
            $ map (toPerson columnIndex) rows

toPerson :: Map.Map T.Text Int -> [T.Text] -> Person
toPerson indices row =
    Person
        { personLastName  = get "Nom"
        , personFirstName = get "Prénom"
        , acceptPrivateGroup = parseAnswer (get acceptPrivateGroupTxt)
        , refusePrivateGroup = parseAnswer (get refusePrivateGroupTxt)
        , acceptPortrait = parseAnswer (get acceptPortraitTxt)
        , acceptGroupPhoto = parseAnswer (get acceptGroupPhotoTxt)
        , acceptAudioRecording = parseAnswer (get acceptAudioRecordingTxt)
        , acceptPrivateMediaExchange = parseAnswer (get acceptPrivateMediaExchangeTxt)
        , acceptPublicMediaUsage = parseAnswer (get acceptPublicMediaUsageTxt)
        , acceptDiffusionRight = parseAnswer (get acceptDiffusionRightTxt)
        , acceptPublicMediaUsage2 = parseAnswer (get acceptPublicMediaUsage2Txt)
        , refusePublicMediaUsage2 = parseAnswer (get refusePublicMediaUsage2Txt)
        }
  where
    get column =
        case Map.lookup column indices of
            Nothing -> ""
            Just i  -> fromMaybe "" (atMay i row)


atMay :: Int -> [a] -> Maybe a
atMay n xs
    | n < 0 = Nothing
    | otherwise =
        case drop n xs of
            y : _ -> Just y
            []    -> Nothing


winnow :: [a] -> [(T.Text, a -> Maybe b)] -> ([(T.Text, [b])], [a])
winnow [] _ = ([], [])
winnow people [] = ([], people)
winnow people ((group, cond):rls) =
      case splitMaybe cond people of
        ([], rest) -> winnow rest rls
        (pps, rest) -> let (otherGroups, outcasts) = winnow rest rls
                        in ((group, pps):otherGroups, outcasts)

splitMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
splitMaybe cond = foldr step ([], [])
  where step elt (bs, as) = case cond elt of
          Just b -> (b:bs, as)
          Nothing -> (bs, elt:as)

data Rule = Rule { ruleName :: T.Text
                 , ruleCondition :: Person -> Bool
                 , ruleSummary :: Person -> [T.Text]
                 }

justPersonName :: Person -> [T.Text]
justPersonName = (:[]) . personName

mutuallyExclusiveChoices :: (Person -> Answer) -> (Person -> Answer) -> T.Text -> Person -> T.Text
mutuallyExclusiveChoices yes no txt p = case (yes p, no p) of
  (Yes, No) -> "oui " <> txt
  (No, Yes) -> "NON " <> txt
  (_, _) -> "??? " <> txt

describeChoice :: (Person -> Answer) -> T.Text -> Person -> T.Text
describeChoice chc txt p = case chc p of
  Yes -> "oui " <> txt
  No -> "NON " <> txt
  Unknown -> "??? " <> txt
  Missing -> "N/A " <> txt

personNameWithChoices :: Person -> [T.Text]
personNameWithChoices p =
  [ personName p
  , mutuallyExclusiveChoices acceptPrivateGroup refusePrivateGroup acceptPrivateGroupTxt p
  , describeChoice acceptPortrait acceptPortraitTxt p
  , describeChoice acceptGroupPhoto acceptGroupPhotoTxt p
  , describeChoice acceptAudioRecording acceptAudioRecordingTxt p
  , describeChoice acceptPrivateMediaExchange acceptPrivateMediaExchangeTxt p
  , describeChoice acceptPublicMediaUsage acceptPublicMediaUsageTxt p
  , describeChoice acceptDiffusionRight acceptDiffusionRightTxt p
  , mutuallyExclusiveChoices acceptPublicMediaUsage2 refusePublicMediaUsage2 acceptPublicMediaUsage2Txt p
  ]

rules :: [Rule]
rules = [ Rule {
            ruleName = "Donne son accord à tous les traitements"
          , ruleCondition = \p -> acceptPrivateGroup p == Yes
               && refusePrivateGroup p == No
               && acceptPortrait p == Yes
               && acceptGroupPhoto p == Yes
               && acceptAudioRecording p == Yes
               && acceptPrivateMediaExchange p == Yes
               && acceptPublicMediaUsage p == Yes
               && acceptDiffusionRight p == Yes
               && acceptPublicMediaUsage2 p == Yes
               && refusePublicMediaUsage2 p == No
          , ruleSummary = justPersonName
          }
        , Rule {
            ruleName = "Donne son accord à tous les traitements sauf les photos/vidéos en gros plan"
          , ruleCondition = \p -> acceptPrivateGroup p == Yes
               && refusePrivateGroup p == No
               && acceptPortrait p == No
               && acceptGroupPhoto p == Yes
               && acceptAudioRecording p == Yes
               && acceptPrivateMediaExchange p == Yes
               && acceptPublicMediaUsage p == Yes
               && acceptDiffusionRight p == Yes
               && acceptPublicMediaUsage2 p == Yes
               && refusePublicMediaUsage2 p == No
          , ruleSummary = justPersonName
          }
        , Rule {
            ruleName = "Document mal rempli?"
          , ruleCondition = \p -> acceptPrivateGroup p == refusePrivateGroup p
               || acceptPublicMediaUsage2 p == refusePublicMediaUsage2 p
          , ruleSummary = personNameWithChoices
          }
        , Rule {
            ruleName = "Compliqué à résumer"
          , ruleCondition = const True
          , ruleSummary = personNameWithChoices
          }
        ]

groups :: [Person] -> [(T.Text, [[T.Text]])]
groups people =
  let (grouped, []) = winnow people
                             (map (\rul -> (ruleName rul, \p -> if (ruleCondition rul $ p) then (Just $ ruleSummary rul $ p) else Nothing))
                                  rules)
   in grouped           

printGroups :: [(T.Text, [[T.Text]])] -> IO ()
printGroups = mapM_ printGroup
  where
    printGroup (title, names) = do
        TIO.putStrLn title
        mapM_ (TIO.putStrLn . ("- " <>) . T.intercalate "\n  ") names
        TIO.putStrLn ""
