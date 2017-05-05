{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll #-}

module Iona.Resolve
  ( ResolveError(..)
  , Context(..)
  , resolveInExpr
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (ReaderT)
import Data.Map (Map)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import GHC.TypeLits (KnownNat, natVal)
import Iona.Syntax.Expr (Expr(..))
import Iona.Syntax.Name (Name(..))
import Iona.Syntax.Pos (Pos)

import qualified Control.Monad.Reader as Reader
import qualified Data.Map as Map

data ResolveError
  = NoSuchVariable Pos Integer Text

data Context = Context
  { packages :: Map [Text] [Text]
  , variables :: Map (Integer, Text) (Name 'True)
  }

type Resolve = ReaderT Context (Either ResolveError)

resolveInExpr
  :: forall s n
   . KnownNat n
  => Expr s 'False n
  -> Resolve (Expr s 'True n)
resolveInExpr (Set p) = pure $ Set p
resolveInExpr (Var p x) = do
  uni <- pure $ natVal (Proxy :: Proxy n)
  ctx <- Reader.ask
  Var p <$> case x of
    Pending Nothing x' ->
      case Map.lookup (uni, x') (variables ctx) of
        Nothing -> throwError $ NoSuchVariable p uni x'
        Just n -> pure n
    Pending (Just s) x' ->
      case Map.lookup s (packages ctx) of
        Nothing -> pure $ Global s x'
        Just s' -> pure $ Global s' x'
resolveInExpr (Abs p xs e) =
  Reader.local (\c -> c { variables = insertAll (variables c) xs }) $
    Abs p xs <$> resolveInExpr e
  where insertAll = foldl $ \a x -> Map.insert (0, x) (Local x) a
resolveInExpr (App p e es) =
  App p <$> resolveInExpr e <*> traverse resolveInExpr es
