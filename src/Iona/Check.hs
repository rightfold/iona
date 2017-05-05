{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
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

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.ST (ST)
import Data.Map (Map)
import GHC.TypeLits (type (+))
import Iona.Syntax.Expr (Expr(..))
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

data CheckError
  = NoSuchVariable Pos (Name 'True)

type Check s n = ReaderT (Contexts s n) (ExceptT CheckError (ST s))

runCheck :: Check s n a -> Contexts s n -> ST s (Either CheckError a)
runCheck a cs = runExceptT $ runReaderT a cs

--------------------------------------------------------------------------------

checkExpr :: Expr s 'True n -> Check s n (Expr s 'True (1 + n))
checkExpr (Set p) = pure $ Set p
checkExpr (Var p x) =
  Reader.asks (Map.lookup x . contextsHead)
  >>= maybe (throwError $ NoSuchVariable p x)
            (pure . symbolType)
