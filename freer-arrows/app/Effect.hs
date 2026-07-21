{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Effect where

import Data.Kind (Type)

type URL = String

data WebServiceOps :: Type -> Type -> Type where
    Get :: URL -- ^ url
        -> [String] -- ^ params
        -> WebServiceOps () String -- ^ WebServiceOps without input producing a String
    Post :: URL -- ^ url
         -> [String] -- ^ params
         -> WebServiceOps String () -- ^ WebServiceOps accepting a String as body and producing ()

nameThatOp :: forall a b. WebServiceOps a b -> String
nameThatOp (Get _ _) = "GET"
nameThatOp (Post _ _) = "POST"
