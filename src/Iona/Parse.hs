module Iona.Parse
  ( scan
  , parse
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Iona.Parse.Lex (AlexPosn(..), Token, alexScanTokens)
import Iona.Parse.Syntax (parse)
import Iona.Syntax.Pos (Pos(..))

scan :: Text -> ByteString -> [Token Pos]
scan file = map (fmap xpos) . alexScanTokens
  where xpos (AlexPn o l c) = Pos file o l c
