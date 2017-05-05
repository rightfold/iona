{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Iona.Check
  ( Symbol(..)
  , Context
  , Contexts(..)

  , CheckError(..)
  , Check
  , runCheck

  , checkExpr
  ) where

import Control.Monad (join)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.ST (ST)
import Control.Monad.Trans (lift)
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Proxy (Proxy(..))
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Text (Text)
import GHC.TypeLits (type (+), KnownNat, natVal)
import Iona.Syntax.Expr (Expr(..), UniVar(..))
import Iona.Syntax.Name (Name)
import Iona.Syntax.Pos (Pos)

import qualified Control.Monad.Reader as Reader
import qualified Data.Map as Map

infixr 5 :::

--------------------------------------------------------------------------------

data Symbol s n = Symbol
  { symbolType :: Expr s 'True (1 + n)
  }

type Context s n = Map (Name 'True) (Symbol s n)

data Contexts s n = (:::)
  { contextsHead :: Context s n
  , contextsTail :: Contexts s (1 + n)
  }

--------------------------------------------------------------------------------

data CheckError s
  = NoSuchVariable Pos Integer (Name 'True)
  | forall n. TypeMismatch (Expr s 'True n) (Expr s 'True n)

type Check s n = ReaderT (Contexts s n) (ExceptT (CheckError s) (ST s))

runCheck :: Check s n a -> Contexts s n -> ST s (Either (CheckError s) a)
runCheck a cs = runExceptT $ runReaderT a cs

--------------------------------------------------------------------------------

freshUniVar :: Text -> Check s n (UniVar s m)
freshUniVar x = MkUniVar `flip` x <$> lift (lift (newSTRef Nothing))

--------------------------------------------------------------------------------

unifyExprs :: Expr s 'True (1 + n) -> Expr s 'True (1 + n) -> Check s m ()
unifyExprs = \a b -> join $ go <$> purge a <*> purge b
  where
  purge :: Expr s 'True (1 + n) -> Check s m (Expr s 'True (1 + n))
  purge e@(UniVar _ (MkUniVar r _)) =
    maybe (pure e) purge =<< lift (lift (readSTRef r))
  purge e = pure e

  go :: Expr s 'True (1 + n) -> Expr s 'True (1 + n) -> Check s m ()
  go (UniVar _ a) (UniVar _ b) | a == b = pure ()
  go (UniVar _ a) b = solve a b
  go a (UniVar _ b) = solve b a
  go (Set _) (Set _) = pure ()
  go (Var _ a) (Var _ b) | a == b = pure ()
  go (App _ a as) (App _ b bs) | length as == length bs = do
    unifyExprs a b
    traverse_ (uncurry unifyExprs) (zip as bs)
  go (Fun _ as a) (Fun _ bs b) | length as == length bs = do
    traverse_ (uncurry unifyExprs) (zip as bs)
    unifyExprs a b
  go a b = throwError $ TypeMismatch a b

  solve :: UniVar s (1 + n) -> Expr s 'True (1 + n) -> Check s m ()
  solve (MkUniVar r _) t = lift . lift $ writeSTRef r (Just t)

checkExpr
  :: forall s n
   . KnownNat n
  => Expr s 'True n
  -> Check s n (Expr s 'True (1 + n))
checkExpr (UniVar p _) = pure $ Set p
checkExpr (Set p) = pure $ Set p
checkExpr (Var p x) =
  Reader.asks (Map.lookup x . contextsHead)
  >>= maybe (throwError $ NoSuchVariable p uni x)
            (pure . symbolType)
  where uni = natVal (Proxy :: Proxy n)
checkExpr (App p e es) = do
  et <- checkExpr e
  ets <- traverse checkExpr es
  rt <- UniVar p <$> freshUniVar "r"
  let ft = Fun p ets rt
  unifyExprs et ft
  pure rt
checkExpr (Fun p es e) =
  Fun p <$> traverse checkExpr es <*> checkExpr e
