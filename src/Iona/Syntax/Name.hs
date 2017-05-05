{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Iona.Syntax.Name
  ( Name(..)
  ) where

import Data.Semigroup ((<>))
import Data.Text (Text)

data Name :: Bool -> * where
  Pending :: Maybe [Text] -> Text -> Name 'False
  Global :: [Text] -> Text -> Name 'True
  Local :: Text -> Name 'True

deriving instance Eq (Name q)

instance Ord (Name q) where
  compare (Pending s1 x1) (Pending s2 x2) = compare s1 s2 <> compare x1 x2
  compare (Global s1 x1) (Global s2 x2) = compare s1 s2 <> compare x1 x2
  compare (Local x1) (Local x2) = compare x1 x2
  compare (Global _ _) (Local _) = LT
  compare (Local _) (Global _ _) = GT

deriving instance Show (Name q)
