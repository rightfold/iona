{
module Iona.Parse.Syntax where

import Iona.Syntax.Expr (Expr(..))
import Iona.Syntax.Pos (Pos)

import qualified Iona.Parse.Lex as Lex
}

%name parse
%error { error . show }

%tokentype { Lex.Token Pos }

%token

  set { Lex.Set _ }

%%

Expr : SetExpr { $1 }

SetExpr : set { Set $ Lex.pos $1 }
