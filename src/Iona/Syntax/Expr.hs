{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Iona.Syntax.Expr
  ( UniVar(..)
  , Expr(..)
  , purge
  ) where

import Control.Monad.ST (ST)
import Data.STRef (STRef, readSTRef)
import Data.Text (Text)
import GHC.TypeLits (type (+), Nat)
import Iona.Syntax.Name (Name)
import Iona.Syntax.Pos (Pos)

data UniVar s n = MkUniVar (STRef s (Maybe (Expr s 'True n))) Text

instance Eq (UniVar s n) where
  MkUniVar a _ == MkUniVar b _ = a == b

instance Show (UniVar s n) where
  showsPrec d (MkUniVar _ n) =
    showParen (d > 10) $
      showString "UniVar _ " . showsPrec 11 n

data Expr :: * -> Bool -> Nat -> * where
  UniVar :: Pos -> UniVar s (1 + n) -> Expr s 'True (1 + n)
  Set :: Pos -> Expr s q (1 + n)
  Var :: Pos -> Name q -> Expr s q n
  Abs :: Pos -> [Text] -> Expr s q 0 -> Expr s q 0
  App :: Pos -> Expr s q n -> [Expr s q n] -> Expr s q n
  Fun :: Pos -> [Expr s q (1 + n)] -> Expr s q (1 + n) -> Expr s q (1 + n)

deriving instance Eq (Expr s q n)
deriving instance Show (Expr s q n)

purge :: Expr s 'True (1 + n) -> ST s (Expr s 'True (1 + n))
purge e@(UniVar _ (MkUniVar r _)) = maybe (pure e) purge =<< readSTRef r
purge e = pure e
