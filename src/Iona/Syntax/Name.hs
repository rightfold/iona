{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Iona.Syntax.Name
  ( Name(..)
  ) where

import Data.Text (Text)

data Name :: Bool -> * where
  Pending :: Maybe [Text] -> Text -> Name 'False
  Global :: [Text] -> Text -> Name 'True
  Local :: Text -> Name 'True
