{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Iona.Syntax.Decl
  ( VarKind(..)
  , DeclScope(..)
  , Decl(..)
  ) where

import Data.Text (Text)
import GHC.TypeLits (KnownNat, Nat)
import Iona.Syntax.Expr (Expr)

data DeclScope
  = GlobalDecl
  | LocalDecl

data VarKind :: Nat -> * where
  Let :: VarKind n
  Def :: VarKind 0
  Lazy :: VarKind 0

data Decl s q
  = PackageDecl DeclScope [Text] [Decl s q]
  | forall n. KnownNat n => VarDecl DeclScope (VarKind n) Text [Expr s q n]
