{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Iona.CodeGen.PHP
  ( CodeGen
  , runCodeGen

  , writeExpr
  , writeName
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, evalStateT)
import Data.Foldable (for_)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Iona.Syntax.Expr (Expr(..))
import Iona.Syntax.Name (Name(..))
import Iona.Syntax.Pos (Pos, posLine)

import qualified Control.Monad.State as State
import qualified Data.Text.IO as Text.IO

--------------------------------------------------------------------------------

type CodeGen = StateT Int IO

runCodeGen :: CodeGen a -> IO a
runCodeGen = evalStateT `flip` 1

write :: Pos -> Text -> CodeGen ()
write pos text = do
  let targetLine = posLine pos
  newlineCount <- State.gets (targetLine -)
  State.put targetLine
  liftIO $ do
    putStr $ take newlineCount (repeat '\n')
    Text.IO.putStr text

--------------------------------------------------------------------------------

writeExpr :: Expr s 'True 0 -> CodeGen ()
writeExpr (Var p x) = writeName p x
writeExpr (Abs p xs e) = do
  write p "(function("
  for_ ([0 :: Int ..] `zip` xs) $ \(i, x) -> do
    when (i /= 0) $ write p ", "
    write p $ "$" <> x
  write p ") { return "
  writeExpr e
  write p "; })"
writeExpr (App p e es) = do
  write p "("
  writeExpr e
  write p "("
  for_ ([0 :: Int ..] `zip` es) $ \(i, e') -> do
    when (i /= 0) $ write p ", "
    writeExpr e'
  write p "))"

writeName :: Pos -> Name 'True -> CodeGen ()
writeName p (Global s x) = write p $
  "(" <> foldMap ("\\" <>) s <> "\\" <> x <> "::value())"
writeName p (Local x) = write p $ "($" <> x <> ")"
