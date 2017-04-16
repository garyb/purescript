-- |
-- Case binders
--
module Language.PureScript.AST.Binders where

import Prelude.Compat

import Language.PureScript.AST.SourcePos
import Language.PureScript.AST.Literals
import Language.PureScript.Names
import Language.PureScript.Comments
import Language.PureScript.Types

-- |
-- Data type for binders
--
data Binder ka ta va
  -- |
  -- Wildcard binder
  --
  = NullBinder va
  -- |
  -- A binder which matches a literal
  --
  | LiteralBinder va (Literal (Binder ka ta va))
  -- |
  -- A binder which binds an identifier
  --
  | VarBinder va Ident
  -- |
  -- A binder which matches a data constructor
  --
  | ConstructorBinder va (Qualified (ProperName 'ConstructorName)) [Binder ka ta va]
  -- |
  -- A operator alias binder. During the rebracketing phase of desugaring,
  -- this data constructor will be removed.
  --
  | OpBinder va (Qualified (OpName 'ValueOpName))
  -- |
  -- Binary operator application. During the rebracketing phase of desugaring,
  -- this data constructor will be removed.
  --
  | BinaryNoParensBinder va (Binder ka ta va) (Binder ka ta va) (Binder ka ta va)
  -- |
  -- Explicit parentheses. During the rebracketing phase of desugaring, this
  -- data constructor will be removed.
  --
  -- Note: although it seems this constructor is not used, it _is_ useful,
  -- since it prevents certain traversals from matching.
  --
  | ParensInBinder va (Binder ka ta va)
  -- |
  -- A binder which binds its input to an identifier
  --
  | NamedBinder va Ident (Binder ka ta va)
  -- |
  -- A binder with source position information
  --
  | PositionedBinder va SourceSpan [Comment] (Binder ka ta va)
  -- |
  -- A binder with a type annotation
  --
  | TypedBinder va (Type ka ta) (Binder ka ta va)
  deriving (Show, Eq, Ord)

-- |
-- Collect all names introduced in binders in an expression
--
binderNames :: Binder ka ta va -> [Ident]
binderNames = go []
  where
    go ns (LiteralBinder _ b) = lit ns b
    go ns (VarBinder _ name) = name : ns
    go ns (ConstructorBinder _ _ bs) = foldl go ns bs
    go ns (BinaryNoParensBinder _ b1 b2 b3) = foldl go ns [b1, b2, b3]
    go ns (ParensInBinder _ b) = go ns b
    go ns (NamedBinder _ name b) = go (name : ns) b
    go ns (PositionedBinder _ _ _ b) = go ns b
    go ns (TypedBinder _ _ b) = go ns b
    go ns _ = ns
    lit ns (ObjectLiteral bs) = foldl go ns (fmap snd bs)
    lit ns (ArrayLiteral bs) = foldl go ns bs
    lit ns _ = ns

isIrrefutable :: Binder ka ta va -> Bool
isIrrefutable NullBinder{} = True
isIrrefutable VarBinder{} = True
isIrrefutable (PositionedBinder _ _ _ b) = isIrrefutable b
isIrrefutable (TypedBinder _ _ b) = isIrrefutable b
isIrrefutable _ = False
