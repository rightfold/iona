{-# LANGUAGE OverloadedStrings #-}

module Iona.CodeGen.PHPSpec
  ( spec
  ) where

import Iona.CodeGen.PHP (runCodeGen, writeExpr)
import Iona.Syntax.Expr (Expr(..))
import Iona.Syntax.Name (Name(..))
import Iona.Syntax.Pos (Pos(..))
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "writeExpr" $
    it "works" $ do
      runCodeGen . writeExpr $
        Var (Pos "" 0 1 1) (Global ["a", "b", "c"] "x")
      runCodeGen . writeExpr $
        Abs (Pos "" 0 1 1)
          ["x", "y", "z"]
          (Var (Pos "" 0 2 3) (Local "x"))
      runCodeGen . writeExpr $
        App (Pos "" 0 1 1)
          (Var (Pos "" 0 1 1) (Global ["a", "b", "c"] "x"))
          [Var (Pos "" 0 2 1) (Global ["a", "b", "c"] "y")]
