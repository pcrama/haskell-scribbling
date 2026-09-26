{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module FreeArrow
  ( FreeChoiceArrow(..)
  , interpFca
  )
where

import Control.Category
import Control.Arrow

data FreeChoiceArrow c i o where
  Arr :: (i -> o) -> FreeChoiceArrow c i o
  Carry :: c i o -> FreeChoiceArrow c i o
  Composition :: FreeChoiceArrow c i p -> FreeChoiceArrow c p o -> FreeChoiceArrow c i o
  StarStarStar :: FreeChoiceArrow c i o -> FreeChoiceArrow c j p -> FreeChoiceArrow c (i, j) (o, p)
  PlusPlusPlus :: FreeChoiceArrow c i o -> FreeChoiceArrow c j p -> FreeChoiceArrow c (Either i j) (Either o p)

instance Category (FreeChoiceArrow c) where
  id = Arr Prelude.id
  Arr f . Arr g = Arr $ g >>> f
  aft . bef = Composition bef aft

instance Arrow (FreeChoiceArrow c) where
  arr = Arr
  first (Arr f) = Arr $ \(x, c) -> (f x, c)
  first c@(Carry _) = StarStarStar c $ Arr Prelude.id
  first (Composition f g) = Composition (first f) (first g)
  first sss@(StarStarStar _ _) = StarStarStar sss $ Arr Prelude.id
  first ppp@(PlusPlusPlus _ _) = StarStarStar ppp $ Arr Prelude.id

instance ArrowChoice (FreeChoiceArrow c) where
  left (Arr f) = Arr $ left f
  left c@(Carry _) = PlusPlusPlus c $ Arr Prelude.id
  left (Composition f g) = Composition (left f) (left g)
  left sss@(StarStarStar _ _) = PlusPlusPlus sss $ Arr Prelude.id
  left ppp@(PlusPlusPlus _ _) = PlusPlusPlus ppp $ Arr Prelude.id

type p :-> q = forall a b. p a b -> q a b

interpFca :: ArrowChoice a => (c :-> a) -> FreeChoiceArrow c i o -> a i o
interpFca _ (Arr f) = arr f
interpFca mbd (Carry c) = mbd c
interpFca mbd (Composition f g) = interpFca mbd f >>> interpFca mbd g
interpFca mbd (StarStarStar s t) = interpFca mbd s *** interpFca mbd t
interpFca mbd (PlusPlusPlus p q) = interpFca mbd p +++ interpFca mbd q
