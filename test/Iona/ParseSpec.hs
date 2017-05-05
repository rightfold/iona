{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Iona.ParseSpec
  ( spec
  ) where

import Data.ByteString.Lazy (ByteString)
import GHC.TypeLits (type (+))
import Iona.Parse (scan, parse)
import Iona.Syntax.Expr (Expr(..))
import Iona.Syntax.Pos (Pos(..))
import Test.Hspec (Spec, describe, it, shouldBe)

go :: ByteString -> Expr s q (1 + n)
go = parse . scan ""

spec :: Spec
spec =
  describe "parse" $
    it "Set" $ go "set" `shouldBe` Set (Pos "" 0 1 1)
