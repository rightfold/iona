{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Iona.Syntax.Expr
  ( Expr(..)
  ) where

import Data.Text (Text)
import GHC.TypeLits (type (+), Nat)
import Iona.Syntax.Name (Name)
import Iona.Syntax.Pos (Pos)

data Expr :: * -> Bool -> Nat -> * where
  Set :: Pos -> Expr s q (1 + n)
  Var :: Pos -> Name q -> Expr s q n
  Abs :: Pos -> [Text] -> Expr s q 0 -> Expr s q 0
  App :: Pos -> Expr s q n -> [Expr s q n] -> Expr s q n
  Fun :: Pos -> [Expr s q (1 + n)] -> Expr s q (1 + n) -> Expr s q (1 + n)

deriving instance Eq (Expr s q n)
deriving instance Show (Expr s q n)
