module Iona.Syntax.Pos
  ( Pos(..)
  ) where

import Data.Text (Text)

data Pos = Pos
  { posFile :: Text
  , posOffset :: {-# UNPACK #-} !Int
  , posLine :: {-# UNPACK #-} !Int
  , posColumn :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)
