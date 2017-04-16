module Language.PureScript.Parser.Types
  ( parseType
  , parsePolyType
  , noWildcards
  , parseTypeAtom
  ) where

import Prelude.Compat

import Control.Monad (when, unless)
import Control.Applicative ((<|>))
import Data.Functor (($>))
import qualified Data.Text as T

import Language.PureScript.AST.SourcePos
import Language.PureScript.Environment
import Language.PureScript.Parser.Common
import Language.PureScript.Parser.Kinds
import Language.PureScript.Parser.Lexer
import Language.PureScript.Types
import Language.PureScript.Label (Label(..))

import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as P

parseFunction :: TokenParser (Type SourceSpan)
parseFunction = parens rarrow *> return tyFunction

parseObject :: TokenParser (Type SourceSpan)
parseObject = withSourceSpan' (flip TypeApp tyRecord) (braces parseRow)

parseTypeLevelString :: TokenParser (Type SourceSpan)
parseTypeLevelString = withSourceSpan' TypeLevelString stringLiteral

parseTypeWildcard :: TokenParser (Type SourceSpan)
parseTypeWildcard = do
  start <- P.getPosition
  let end = P.incSourceColumn start 1
  underscore
  let ss = SourceSpan (P.sourceName start) (toSourcePos start) (toSourcePos end)
  return $ TypeWildcard ss ss

parseTypeVariable :: TokenParser (Type SourceSpan)
parseTypeVariable = withSourceSpan' TypeVar $ do
  ident <- identifier
  when (ident `elem` reservedTypeNames) $ P.unexpected (T.unpack ident)
  return ident

parseTypeConstructor :: TokenParser (Type SourceSpan)
parseTypeConstructor = withSourceSpan' TypeConstructor (parseQualified typeName)

parseForAll :: TokenParser (Type SourceSpan)
parseForAll = withSourceSpan' (\ss (vs, ty) -> mkForAll ss vs ty) $
    (,)
      <$> ((reserved "forall" <|> reserved "∀") *> P.many1 (indented *> identifier) <* indented <* dot)
      <*> parseType

-- |
-- Parse a type as it appears in e.g. a data constructor
--
parseTypeAtom :: TokenParser (Type SourceSpan)
parseTypeAtom =
  indented *> P.choice
    [ P.try parseFunction
    , parseTypeLevelString
    , parseObject
    , parseTypeWildcard
    , parseForAll
    , parseTypeVariable
    , parseTypeConstructor
    -- This try is needed due to some unfortunate ambiguities between rows and kinded types
    , P.try (parens parseRow)
    , withSourceSpan' ParensInType (parens parsePolyType)
    ]

parseConstrainedType :: TokenParser ([Constraint SourceSpan], Type SourceSpan)
parseConstrainedType = do
  constraints <- parens (commaSep1 parseConstraint) <|> pure <$> parseConstraint
  _ <- rfatArrow
  indented
  ty <- parseType
  return (constraints, ty)
  where
  parseConstraint :: TokenParser (Constraint SourceSpan)
  parseConstraint =
    withSourceSpan' (\ss (className, ty) -> Constraint ss className ty Nothing) $ do
      className <- parseQualified properName
      indented
      ty <- P.many parseTypeAtom
      return (className, ty)

-- This is here to improve the error message when the user
-- tries to use the old style constraint contexts.
-- TODO: Remove this before 1.0
typeOrConstrainedType :: TokenParser (SourceSpan -> Type SourceSpan)
typeOrConstrainedType = do
  e <- P.try (Left <$> parseConstrainedType) <|> Right <$> parseTypeAtom
  case e of
    Left ([c], ty) -> pure $ \ss -> ConstrainedType ss c ty
    Left _ ->
      P.unexpected $
        unlines [ "comma in constraints."
                , ""
                , "Class constraints in type annotations can no longer be grouped in parentheses."
                , "Each constraint should now be separated by `=>`, for example:"
                , "    `(Applicative f, Semigroup a) => a -> f a -> f a`"
                , "  would now be written as:"
                , "    `Applicative f => Semigroup a => a -> f a -> f a`."
                ]
    Right ty -> pure (const ty)

-- TODO-ann: this is really quite weird now, I'm not sure if there's a better way to do it?
parseAnyType :: TokenParser (Type SourceSpan)
parseAnyType = withSourceSpan' (flip ($)) p
  where
    p :: TokenParser (SourceSpan -> Type SourceSpan)
    p = P.buildExpressionParser operators (buildPostfixParser postfixTable typeOrConstrainedType) P.<?> "type"
    operators :: [[TokenOperator (SourceSpan -> Type SourceSpan)]]
    operators =
      [ [ P.Infix (return $ \lhs rhs ss -> TypeApp ss (lhs ss) (rhs ss)) P.AssocLeft ]
      , [ P.Infix op P.AssocRight ]
      , [ P.Infix (rarrow $> (\lhs rhs ss -> function ss (lhs ss) (rhs ss))) P.AssocRight ]
      ]
    postfixTable :: [(SourceSpan -> Type SourceSpan) -> TokenParser (SourceSpan -> Type SourceSpan)]
    postfixTable =
      [ \t -> (\k ss -> KindedType ss (t ss) k) <$> (indented *> doubleColon *> parseKind)
      ]
    op :: TokenParser ((SourceSpan -> Type SourceSpan) -> (SourceSpan -> Type SourceSpan) -> SourceSpan -> Type SourceSpan)
    op = do
      ident <- P.try $ withSourceSpan' TypeOp (parseQualified parseOperator)
      return $ \lhs rhs ss -> BinaryNoParensType ss ident (lhs ss) (rhs ss)

-- |
-- Parse a monotype
--
parseType :: TokenParser (Type SourceSpan)
parseType = do
  ty <- parseAnyType
  unless (isMonoType ty) $ P.unexpected "polymorphic type"
  return ty

-- |
-- Parse a polytype
--
parsePolyType :: TokenParser (Type SourceSpan)
parsePolyType = parseAnyType

-- |
-- Parse an atomic type with no wildcards
--
noWildcards :: TokenParser (Type a) -> TokenParser (Type a)
noWildcards p = do
  ty <- p
  when (containsWildcards ty) $ P.unexpected "type wildcard"
  return ty

parseNameAndType :: TokenParser t -> TokenParser (SourceSpan, Label, t)
parseNameAndType p =
  withSourceSpan' (\ss (label, t) -> (ss, label, t)) $
    (,) <$> (indented *> (Label <$> parseLabel) <* indented <* doubleColon) <*> p

parseRowEnding :: TokenParser (Type SourceSpan)
parseRowEnding = do
  pos <- P.getPosition
  let ss = SourceSpan (P.sourceName pos) (toSourcePos pos) (toSourcePos pos)
  P.option (REmpty ss) $ indented *> pipe *> indented *> parseType

parseRow :: TokenParser (Type SourceSpan)
parseRow = (curry rowFromList <$> commaSep (parseNameAndType parsePolyType) <*> parseRowEnding) P.<?> "row"
