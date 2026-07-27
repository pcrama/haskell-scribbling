-- After https://chrispenner.ca/posts/arrow-effects
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Penner (
  ioPenner
  , ioPennerHttpAndConsole
  , recordPenner
  , approxMax
  , approxMin
  , pennerArrow
  , pennerHttpAndConsoleArrow
  , CommandRecorder(..)
  , CommandTree(Eff)
  , GetPost(..)
  , GetPostCommand
  , ReadWriteCommand
  ) where

import Control.Category
import Control.Arrow

import Effect (URL)

class Arrow k => GetPost k where
  -- Get has no interesting input, so we use () as input type.
  get :: URL -> [String] -> k () String

  -- We track the inputs for the writeLine directly in the Category structure.
  post :: URL -> [String] -> k String ()

  getDyn :: [String] -> k URL String

class Arrow k => ReadWriteConsole k where
  readLine :: k () String
  writeLine :: k String ()

instance GetPost (Kleisli IO) where
  get url params = Kleisli $ const $ do
    putStrLn $ "kleisli.get " <> url <> "?" <> show params
    return $ "get " <> show params
  post url params = Kleisli $ \body ->
    putStrLn $ "kleisli.post " <> (show $ length body) <> " characters to "<> url <> "?" <> show params
  getDyn params = Kleisli $ \url -> do
    putStrLn $ "kleisli.getDyn " <> url <> "?" <> show params
    return $ "getDyn " <> url <> "?" <> show params

instance ReadWriteConsole (Kleisli IO) where
  readLine = Kleisli $ const $ do
    putStrLn "Simulating readLine..."
    return "ReadWriteConsole/fake/input"
  writeLine = Kleisli putStrLn

pennerArrow :: (ArrowChoice k, GetPost k) => String -> URL -> k () ()
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
    case length s1 of
      0 -> post "https://zero.com" [] -< "zero"
      1 -> post "https://one.com" [] -< "1" <> s1
      2 -> post "https://two.com" [] -< "2" <> s1 <> "2"
      _ -> post "https://three-or-more.com" [] -< s1

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

newtype CommandRecorder c i o = CommandRecorder (CommandTree c)
  deriving (Show, Eq)

approxMax :: Monoid m => (c -> m) -> CommandTree c -> m
approxMax = foldMap

approxMin :: Monoid m => (c -> m) -> CommandTree c -> m
approxMin f = go
  where go Identity = mempty
        go (Eff eff) = f eff
        go (Composed e1 e2 es) = approxMin f e1 <> approxMin f e2 <> foldMap (approxMin f) es
        go (Parallel e1 e2 es) = approxMin f e1 <> approxMin f e2 <> foldMap (approxMin f) es
        go (Choice _ _ _) = mempty
        
instance Category (CommandRecorder c) where
  id = CommandRecorder Identity
  CommandRecorder e2 . CommandRecorder Identity = CommandRecorder e2
  CommandRecorder Identity . CommandRecorder e1 = CommandRecorder e1
  CommandRecorder (Composed f1 f2 fs) . CommandRecorder (Composed e1 e2 es) = CommandRecorder $ Composed e1 e2 $ es <> (f1:f2:fs)
  CommandRecorder c . CommandRecorder (Composed e1 e2 es) = CommandRecorder $ Composed e1 e2 $ es <> [c]
  CommandRecorder (Composed e1 e2 es) . CommandRecorder c = CommandRecorder $ Composed c e1 $ e2:es
  CommandRecorder f . CommandRecorder e = CommandRecorder $ Composed e f []

instance Arrow (CommandRecorder c) where
  arr _ = CommandRecorder Identity
  CommandRecorder e1 *** CommandRecorder Identity = CommandRecorder e1
  CommandRecorder Identity *** CommandRecorder e2 = CommandRecorder e2
  CommandRecorder (Parallel e1 e2 es) *** CommandRecorder (Parallel f1 f2 fs) = CommandRecorder $ Parallel e1 e2 $ es <> (f1:f2:fs)
  CommandRecorder (Parallel e1 e2 es) *** CommandRecorder c = CommandRecorder $ Parallel e1 e2 $ es <> [c]
  CommandRecorder c *** CommandRecorder (Parallel e1 e2 es) = CommandRecorder $ Parallel c e1 $ e2:es
  CommandRecorder f *** CommandRecorder e = CommandRecorder $ Parallel f e []

instance ArrowChoice (CommandRecorder c) where
  CommandRecorder e1 +++ CommandRecorder Identity = CommandRecorder e1
  CommandRecorder Identity +++ CommandRecorder e2 = CommandRecorder e2
  CommandRecorder (Choice e1 e2 es) +++ CommandRecorder (Choice f1 f2 fs) = CommandRecorder $ Choice e1 e2 $ es <> (f1:f2:fs)
  CommandRecorder (Choice e1 e2 es) +++ CommandRecorder c = CommandRecorder $ Choice e1 e2 $ es <> [c]
  CommandRecorder c +++ CommandRecorder (Choice e1 e2 es) = CommandRecorder $ Choice c e1 $ e2:es
  CommandRecorder f +++ CommandRecorder e = CommandRecorder $ Choice f e []

instance GetPost (CommandRecorder GetPostCommand) where
  get url _ = CommandRecorder $ Eff $ Get url
  post url params = CommandRecorder $ Eff $ Post url params
  getDyn params = CommandRecorder $ Eff $ GetDyn params

instance {-# OVERLAPPABLE #-} GetPost (CommandRecorder (Maybe a)) where
  get _ _ = CommandRecorder $ Eff $ Nothing
  post _ _ = CommandRecorder $ Eff $ Nothing
  getDyn _ = CommandRecorder $ Eff $ Nothing

instance GetPost (CommandRecorder (Maybe GetPostCommand)) where
  get url _ = CommandRecorder $ Eff $ Just $ Get url
  post url params = CommandRecorder $ Eff $ Just $ Post url params
  getDyn params = CommandRecorder $ Eff $ Just $ GetDyn params

data ReadWriteCommand = ReadLine | WriteLine deriving (Show, Eq)

instance ReadWriteConsole (CommandRecorder ReadWriteCommand) where
  readLine = CommandRecorder $ Eff $ ReadLine
  writeLine = CommandRecorder $ Eff $ WriteLine

instance {-# OVERLAPPABLE #-} ReadWriteConsole (CommandRecorder (Maybe a)) where
  readLine = CommandRecorder $ Eff $ Nothing
  writeLine = CommandRecorder $ Eff $ Nothing

instance ReadWriteConsole (CommandRecorder (Maybe ReadWriteCommand)) where
  readLine = CommandRecorder $ Eff $ Just $ ReadLine
  writeLine = CommandRecorder $ Eff $ Just $ WriteLine

recordPenner :: String -> URL -> CommandRecorder GetPostCommand () ()
recordPenner = pennerArrow

pennerHttpAndConsoleArrow :: (GetPost k, ReadWriteConsole k) => k () ()
pennerHttpAndConsoleArrow =
  let string2URL = Prelude.id in proc () -> do
    writeLine -< "Enter an URL"
    sUrl <- readLine -< ()
    response <- getDyn [] -< string2URL sUrl
    writeLine -< ("'" <> sUrl <> "' returned:")
    writeLine -< response

ioPennerHttpAndConsole :: IO ()
ioPennerHttpAndConsole = runKleisli pennerHttpAndConsoleArrow ()
