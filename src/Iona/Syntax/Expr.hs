{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Iona.Syntax.Expr
  ( Expr(..)
  ) where

import Data.Text (Text)
import GHC.TypeLits (type (+), Nat, KnownNat)
import Iona.Syntax.Name (Name)

data Expr :: * -> Bool -> Nat -> * where
  Set :: KnownNat n => Expr s q (1 + n)
  Var :: Name q -> Expr s q n
  Abs :: Text -> Expr s q 0 -> Expr s q 0
  App :: Expr s q n -> Expr s q n -> Expr s q n
