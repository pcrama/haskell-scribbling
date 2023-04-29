#! /usr/bin/env nix-shell
#! nix-shell -i runghc -p "haskellPackages.ghcWithPackages (p: [p.turtle])"

{-# LANGUAGE OverloadedStrings #-}

import GHC.IO.Encoding (setLocaleEncoding, utf8)

import qualified Data.Text as T
import qualified Data.List as L
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Turtle


-- ./HaskellVaigoDownloader.hs --session 'ey...3D' --url 'https://vaigo.me/registrations/approved?budget=&budgetId=&budgetReasonId=&serviceKey=all&startDate=2023-03-01&endDate=2023-03-31'


data CliArgs = CliArgs
  { vaigoSession :: Text
  , startUrl :: Text
  } deriving (Show, Eq)


optionsParser :: Parser CliArgs
optionsParser = CliArgs <$> optText "session" 's' "Vaigo.me session cookie"
                        <*> optText "url" 'u' "Vaigo.me start URL"

main = sh $ do
  liftIO $ setLocaleEncoding utf8
  cliArgs <- options "Vaigo Downloader" optionsParser
  alreadySeen <- liftIO $ newIORef []
  stdout $ crawlUrl (doCurl $ vaigoSession cliArgs) (startUrl cliArgs) alreadySeen


doCurl sessionCookie url = inproc "curl" ["--silent", "--location", "--cookie", "vaigo_session=" <> sessionCookie, url] empty


crawlUrl :: (Text -> Shell Line) -> Text -> IORef [Text] -> Shell Line
crawlUrl getUrl url alreadySeenRef = do
  line <- grep (has "<a ") $ getUrl url
  let relNextAhtmlElements = L.filter ("rel=\"next\"" `T.isInfixOf`) $ T.splitOn "<a " $ lineToText line
  case L.nub $ L.filter ("https://vaigo.me" `T.isPrefixOf`) $ concatMap (T.split (== '"')) relNextAhtmlElements of
    [] -> select []
    [nextUrl] -> do
      alreadySeen <- liftIO $ readIORef alreadySeenRef
      let decodedUrl = T.intercalate "&" $ T.splitOn "&amp;" nextUrl in
        if decodedUrl `elem` alreadySeen
        then do
          -- liftIO $ putStrLn . T.unpack $ "Skipping " <> url <> ", already in " <> T.intercalate ", " alreadySeen
          select [] -- fail "Nothing more to do"
        else do
          liftIO $ writeIORef alreadySeenRef $ decodedUrl:alreadySeen
          -- liftIO $ putStrLn . T.unpack $ "recurse into " <> decodedUrl
          select (textToLines decodedUrl) <|> crawlUrl getUrl decodedUrl alreadySeenRef
    moreUrls -> error $ "More than one next URL: " <> show moreUrls

-- Local Variables:
-- mode: haskell-mode
-- haskell-process-type: ghci
-- haskell-process-path-ghci: ("nixshellpp" "-p" "cabal-install" "-p" "haskellPackages.ghcWithPackages (p: [p.turtle p.haskell-language-server])" "++" "ghci")
-- End:
