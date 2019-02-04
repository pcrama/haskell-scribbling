{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
-- The paper has its own implementation of MonadError
module TheirMonadError (
  MonadError(..)
  )
where

import Control.Monad.State (StateT(..), lift)
-- Need the same MaybeT as e.g. MTParser2 to add correct instance -> do not reimplement
import Control.Monad.Trans.Maybe (MaybeT(..))

class Monad m => MonadError m where
  type Error m :: *
  catchError :: m a -> (Error m -> m a) -> m a
  throwError :: Error m -> m a

instance MonadError (Either a) where
  type Error (Either a) = a
  catchError (Left e) f = f e
  catchError m@(Right _) _ = m
  throwError = Left

-- newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }
-- e.g. StateT s IO a === s -> IO (a, s)
--      StateT s (Either e) a === (s -> Either e (a, s))
instance (MonadError m) => MonadError (StateT s m) where
  type Error (StateT s m) = Error m
  -- catchError :: StateT s m a
  --            -> (Error (StateT s m a) -> StateT s m a)
  --            -> StateT s m a
  --
  -- catcher :: Error (StateT s m) -> StateT s m a
  --        === Error m -> "StateT (s -> m (a, s))"
  -- c :: s -> m (a, s)
  catchError (StateT c) catcher = StateT $ \s ->
    catchError (c s) $ \e -> runStateT (catcher e) s
  -- throwError :: Error (StateT s m a) -> StateT s m a
  --           === Error m -> "StateT (s -> m (a, s))"
  -- throwError e = StateT $ \s -> fmap (, s) $ throwError e
  throwError = lift . throwError

-- newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
instance (MonadError m) => MonadError (MaybeT m) where
  type Error (MaybeT m) = Error m
  -- catchError :: MaybeT m a
  --            -> (Error (MaybeT m) -> MaybeT m a)
  --            -> MaybeT m a
  --
  -- "inner catchError" :: m (Maybe a) -> (Error m -> m (Maybe a)) -> m (Maybe a)
  -- f :: Error (MaybeT m) -> MaybeT m a
  --  === Error m -> "MaybeT (m (Maybe a))"
  -- c :: MaybeT m a
  catchError c f = MaybeT $ catchError (runMaybeT c) (runMaybeT . f)
  -- throwError :: Error (MaybeT m) -> MaybeT m a
  -- "inner throwError" :: Error m -> m a
  -- throwError = MaybeT . fmap (Just) . throwError
  throwError = lift . throwError
