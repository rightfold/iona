{
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module Iona.Parse.Lex where

import Data.ByteString.Lazy (toStrict)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
}

%wrapper "posn-bytestring"

tokens :-

  [ \t\r\n]+ ;

  \-> { \p _ -> Arrow p }
  = { \p _ -> Equals p }
  \{ { \p _ -> LeftBrace p }
  \( { \p _ -> LeftParen p }
  \} { \p _ -> RightBrace p }
  \) { \p _ -> RightParen p }
  \; { \p _ -> Semicolon p }

  data { \p _ -> Data p }
  def { \p _ -> Def p }
  fun { \p _ -> Fun p }
  kind { \p _ -> Kind p }
  lam { \p _ -> Lam p }
  lazy { \p _ -> Lazy p }
  let { \p _ -> Let p }
  package { \p _ -> Package p }
  set { \p _ -> Set p }
  sort { \p _ -> Sort p }
  type { \p _ -> Type p }
  unique { \p _ -> Unique p }
  universe { \p _ -> Universe p }

  [a-zA-Z_][a-zA-Z0-9_]* { \p s -> Identifier p (decodeUtf8 (toStrict s)) }

{
data Token p
  = Arrow p
  | Equals p
  | LeftBrace p
  | LeftParen p
  | RightBrace p
  | RightParen p
  | Semicolon p

  | Data p
  | Def p
  | Fun p
  | Kind p
  | Lam p
  | Lazy p
  | Let p
  | Package p
  | Set p
  | Sort p
  | Type p
  | Unique p
  | Universe p

  | Identifier p Text
  deriving (Eq, Ord, Show, Foldable, Functor)

pos :: Token p -> p
pos = head . foldMap pure
}
