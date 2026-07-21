-- After https://chrispenner.ca/posts/arrow-effects
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Penner (ioPenner, recordPenner) where

import Control.Category
import Control.Arrow

import Effect (URL)

class (Arrow k) => GetPost k where
  -- Get has no interesting input, so we use () as input type.
  get :: URL -> [String] -> k () String

  -- We track the inputs for the writeLine directly in the Category structure.
  post :: URL -> [String] -> k String ()

  getDyn :: [String] -> k URL String

instance GetPost (Kleisli IO) where
  get url params = Kleisli $ const $ do
    putStrLn $ "kleisli.get " <> url <> "?" <> show params
    return $ "get " <> show params
  post url params = Kleisli $ \body ->
    putStrLn $ "kleisli.post " <> (show $ length body) <> " characters to "<> url <> "?" <> show params
  getDyn params = Kleisli $ \url -> do
    putStrLn $ "kleisli.getDyn " <> url <> "?" <> show params
    return $ "getDyn " <> url <> "?" <> show params

-- pennerArrow :: GetPost k => String -> URL -> k () ()
-- pennerArrow firstBody getUrl =
--   let string2URL = Prelude.id in proc () -> do
--     post "https://post.example.com" ["1st", "post"] -< firstBody 
--     s1 <- get getUrl ["get"] -< ()
--     bDyn <- getDyn ["dyn"] -< string2URL s1
--     post "https://direct.post.com" ["direct"] -< s1
--     post "https://post.example.com" ["2nd", "post"] -< bDyn

pennerArrow :: GetPost k => String -> URL -> k () ()
pennerArrow firstBody getUrl =
  let string2URL = Prelude.id in proc () -> do
    post "https://post.example.com" ["1st", "post"] -< firstBody 
    s1 <- get getUrl ["get"] -< ()
    (
      (arr string2URL >>> getDyn ["dyn"] >>> post "https://post.getdyn.com" [])
      &&&
      post "https://direct.post.com" ["direct"]
      &&&
      (getDyn ["2nd", "dyn"] >>> arr (const ()))
      ) -< s1
    post "https://final.com" [] -< s1
ioPenner :: String -> URL -> IO ()
ioPenner firstBody getUrl = flip runKleisli () $ pennerArrow firstBody getUrl

data GetPostCommand =
  Get URL  -- get params elided on purpose
  | GetDyn [String]
  | Post URL [String]
  deriving (Show, Eq)

data CommandTree eff =
  Eff eff
  | Identity
  | Composed (CommandTree eff) (CommandTree eff) [CommandTree eff]
  | Parallel (CommandTree eff) (CommandTree eff) [CommandTree eff]
  | Choice (CommandTree eff) (CommandTree eff) [CommandTree eff]
  deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data CommandRecorder c i o = Cmd (CommandTree c)
  deriving (Show, Eq)

instance Category (CommandRecorder c) where
  id = Cmd Identity
  Cmd e2 . Cmd Identity = Cmd e2
  Cmd Identity . Cmd e1 = Cmd e1
  Cmd (Composed f1 f2 fs) . Cmd (Composed e1 e2 es) = Cmd $ Composed e1 e2 $ es <> (f1:f2:fs)
  Cmd c . Cmd (Composed e1 e2 es) = Cmd $ Composed e1 e2 $ es <> [c]
  Cmd (Composed e1 e2 es) . Cmd c = Cmd $ Composed c e1 $ e2:es
  Cmd f . Cmd e = Cmd $ Composed e f []

instance Arrow (CommandRecorder c) where
  arr _ = Cmd Identity
  Cmd e1 *** Cmd Identity = Cmd e1
  Cmd Identity *** Cmd e2 = Cmd e2
  Cmd (Parallel e1 e2 es) *** Cmd (Parallel f1 f2 fs) = Cmd $ Parallel e1 e2 $ es <> (f1:f2:fs)
  Cmd (Parallel e1 e2 es) *** Cmd c = Cmd $ Parallel e1 e2 $ es <> [c]
  Cmd c *** Cmd (Parallel e1 e2 es) = Cmd $ Parallel c e1 $ e2:es
  Cmd f *** Cmd e = Cmd $ Parallel f e []

instance ArrowChoice (CommandRecorder c) where
  Cmd e1 +++ Cmd Identity = Cmd e1
  Cmd Identity +++ Cmd e2 = Cmd e2
  Cmd (Choice e1 e2 es) +++ Cmd (Choice f1 f2 fs) = Cmd $ Choice e1 e2 $ es <> (f1:f2:fs)
  Cmd (Choice e1 e2 es) +++ Cmd c = Cmd $ Choice e1 e2 $ es <> [c]
  Cmd c +++ Cmd (Choice e1 e2 es) = Cmd $ Choice c e1 $ e2:es
  Cmd f +++ Cmd e = Cmd $ Choice f e []

instance GetPost (CommandRecorder GetPostCommand) where
  get url _ = Cmd $ Eff $ Get url
  post url params = Cmd $ Eff $ Post url params
  getDyn params = Cmd $ Eff $ GetDyn params

recordPenner :: String -> URL -> CommandRecorder GetPostCommand () ()
recordPenner = pennerArrow
