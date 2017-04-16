{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

-- |
-- Data types for modules and declarations
--
module Language.PureScript.AST.Declarations where

import Prelude.Compat

import Control.Monad.Identity

import Data.Aeson.TH
import qualified Data.Map as M
import Data.Text (Text)

import Language.PureScript.AST.Ann
import Language.PureScript.AST.Binders
import Language.PureScript.AST.Literals
import Language.PureScript.AST.Operators
import Language.PureScript.AST.SourcePos
import Language.PureScript.Types
import Language.PureScript.PSString (PSString)
import Language.PureScript.Label (Label)
import Language.PureScript.Names
import Language.PureScript.Kinds
import Language.PureScript.TypeClassDictionaries
import Language.PureScript.Comments
import Language.PureScript.Environment
import qualified Language.PureScript.Bundle as Bundle

import qualified Text.Parsec as P

-- | A map of locally-bound names in scope.
type Context = [(Ident, Type KindAnn TypeAnn)]

-- | Holds the data necessary to do type directed search for typed holes
data TypeSearch
  = TSBefore Environment
  -- ^ An Environment captured for later consumption by type directed search
  | TSAfter
    { tsAfterIdentifiers :: [(Qualified Text, Type KindAnn TypeAnn)]
    -- ^ The identifiers that fully satisfy the subsumption check
    , tsAfterRecordFields :: Maybe [(Label, Type KindAnn TypeAnn)]
    -- ^ Record fields that are available on the first argument to the typed
    -- hole
    }
  -- ^ Results of applying type directed search to the previously captured
  -- Environment
  deriving Show

onTypeSearchTypes :: (Type KindAnn TypeAnn -> Type KindAnn TypeAnn) -> TypeSearch -> TypeSearch
onTypeSearchTypes f = runIdentity . onTypeSearchTypesM (Identity . f)

onTypeSearchTypesM :: Applicative m => (Type KindAnn TypeAnn -> m (Type KindAnn TypeAnn)) -> TypeSearch -> m TypeSearch
onTypeSearchTypesM f (TSAfter i r) = TSAfter <$> traverse (traverse f) i <*> traverse (traverse (traverse f)) r
onTypeSearchTypesM _ (TSBefore env) = pure (TSBefore env)

-- | A type of error messages
data SimpleErrorMessage
  = ModuleNotFound ModuleName
  | ErrorParsingFFIModule FilePath (Maybe Bundle.ErrorMessage)
  | ErrorParsingModule P.ParseError
  | MissingFFIModule ModuleName
  | MultipleFFIModules ModuleName [FilePath]
  | UnnecessaryFFIModule ModuleName FilePath
  | MissingFFIImplementations ModuleName [Ident]
  | UnusedFFIImplementations ModuleName [Ident]
  | InvalidFFIIdentifier ModuleName Text
  | CannotGetFileInfo FilePath
  | CannotReadFile FilePath
  | CannotWriteFile FilePath
  | InfiniteType (Type KindAnn TypeAnn)
  | InfiniteKind (Kind KindAnn)
  | MultipleValueOpFixities (OpName 'ValueOpName)
  | MultipleTypeOpFixities (OpName 'TypeOpName)
  | OrphanTypeDeclaration Ident
  | RedefinedIdent Ident
  | OverlappingNamesInLet
  | UnknownName (Qualified Name)
  | UnknownImport ModuleName Name
  | UnknownImportDataConstructor ModuleName (ProperName 'TypeName) (ProperName 'ConstructorName)
  | UnknownExport Name
  | UnknownExportDataConstructor (ProperName 'TypeName) (ProperName 'ConstructorName)
  | ScopeConflict Name [ModuleName]
  | ScopeShadowing Name (Maybe ModuleName) [ModuleName]
  | DeclConflict Name Name
  | ExportConflict (Qualified Name) (Qualified Name)
  | DuplicateModule ModuleName [SourceSpan]
  | DuplicateTypeArgument Text
  | InvalidDoBind
  | InvalidDoLet
  | CycleInDeclaration Ident
  | CycleInTypeSynonym (Maybe (ProperName 'TypeName))
  | CycleInModules [ModuleName]
  | NameIsUndefined Ident
  | UndefinedTypeVariable (ProperName 'TypeName)
  | PartiallyAppliedSynonym (Qualified (ProperName 'TypeName))
  | EscapedSkolem Text (Maybe SourceSpan) (Type KindAnn TypeAnn)
  | TypesDoNotUnify (Type KindAnn TypeAnn) (Type KindAnn TypeAnn)
  | KindsDoNotUnify (Kind KindAnn) (Kind KindAnn)
  | ConstrainedTypeUnified (Type KindAnn TypeAnn) (Type KindAnn TypeAnn)
  | OverlappingInstances (Qualified (ProperName 'ClassName)) [Type KindAnn TypeAnn] [Qualified Ident]
  | NoInstanceFound (Constraint KindAnn TypeAnn)
  | AmbiguousTypeVariables (Type KindAnn TypeAnn) (Constraint KindAnn TypeAnn)
  | UnknownClass (Qualified (ProperName 'ClassName))
  | PossiblyInfiniteInstance (Qualified (ProperName 'ClassName)) [Type KindAnn TypeAnn]
  | CannotDerive (Qualified (ProperName 'ClassName)) [Type KindAnn TypeAnn]
  | InvalidDerivedInstance (Qualified (ProperName 'ClassName)) [Type KindAnn TypeAnn] Int
  | ExpectedTypeConstructor (Qualified (ProperName 'ClassName)) [Type KindAnn TypeAnn] (Type KindAnn TypeAnn)
  | InvalidNewtypeInstance (Qualified (ProperName 'ClassName)) [Type KindAnn TypeAnn]
  | CannotFindDerivingType (ProperName 'TypeName)
  | DuplicateLabel Label (Maybe (Expr KindAnn TypeAnn ValueAnn))
  | DuplicateValueDeclaration Ident
  | ArgListLengthsDiffer Ident
  | OverlappingArgNames (Maybe Ident)
  | MissingClassMember Ident
  | ExtraneousClassMember Ident (Qualified (ProperName 'ClassName))
  | ExpectedType (Type KindAnn TypeAnn) (Kind KindAnn)
  | IncorrectConstructorArity (Qualified (ProperName 'ConstructorName))
  | ExprDoesNotHaveType (Expr KindAnn TypeAnn ValueAnn) (Type KindAnn TypeAnn)
  | PropertyIsMissing Label
  | AdditionalProperty Label
  | TypeSynonymInstance
  | OrphanInstance Ident (Qualified (ProperName 'ClassName)) [Type KindAnn TypeAnn]
  | InvalidNewtype (ProperName 'TypeName)
  | InvalidInstanceHead (Type KindAnn TypeAnn)
  | TransitiveExportError DeclarationRef [DeclarationRef]
  | TransitiveDctorExportError DeclarationRef (ProperName 'ConstructorName)
  | ShadowedName Ident
  | ShadowedTypeVar Text
  | UnusedTypeVar Text
  | WildcardInferredType (Type KindAnn TypeAnn) Context
  | HoleInferredType Text (Type KindAnn TypeAnn) Context TypeSearch
  | MissingTypeDeclaration Ident (Type KindAnn TypeAnn)
  | OverlappingPattern [[Binder KindAnn TypeAnn ValueAnn]] Bool
  | IncompleteExhaustivityCheck
  | MisleadingEmptyTypeImport ModuleName (ProperName 'TypeName)
  | ImportHidingModule ModuleName
  | UnusedImport ModuleName
  | UnusedExplicitImport ModuleName [Name] (Maybe ModuleName) [DeclarationRef]
  | UnusedDctorImport ModuleName (ProperName 'TypeName) (Maybe ModuleName) [DeclarationRef]
  | UnusedDctorExplicitImport ModuleName (ProperName 'TypeName) [ProperName 'ConstructorName] (Maybe ModuleName) [DeclarationRef]
  | DuplicateSelectiveImport ModuleName
  | DuplicateImport ModuleName ImportDeclarationType (Maybe ModuleName)
  | DuplicateImportRef Name
  | DuplicateExportRef Name
  | IntOutOfRange Integer Text Integer Integer
  | ImplicitQualifiedImport ModuleName ModuleName [DeclarationRef]
  | ImplicitImport ModuleName [DeclarationRef]
  | HidingImport ModuleName [DeclarationRef]
  | CaseBinderLengthDiffers Int [Binder KindAnn TypeAnn ValueAnn]
  | IncorrectAnonymousArgument
  | InvalidOperatorInBinder (Qualified (OpName 'ValueOpName)) (Qualified Ident)
  | CannotGeneralizeRecursiveFunction Ident (Type KindAnn TypeAnn)
  | CannotDeriveNewtypeForData (ProperName 'TypeName)
  | ExpectedWildcard (ProperName 'TypeName)
  | CannotUseBindWithDo Ident
  -- | instance name, type class, expected argument count, actual argument count
  | ClassInstanceArityMismatch Ident (Qualified (ProperName 'ClassName)) Int Int
  -- | a user-defined warning raised by using the Warn type class
  | UserDefinedWarning (Type KindAnn TypeAnn)
  -- | a declaration couldn't be used because there wouldn't be enough information
  -- | to choose an instance
  | UnusableDeclaration Ident
  deriving (Show)

-- | Error message hints, providing more detailed information about failure.
data ErrorMessageHint
  = ErrorUnifyingTypes (Type KindAnn TypeAnn) (Type KindAnn TypeAnn)
  | ErrorInExpression (Expr KindAnn TypeAnn ValueAnn)
  | ErrorInModule ModuleName
  | ErrorInInstance (Qualified (ProperName 'ClassName)) [Type KindAnn TypeAnn]
  | ErrorInSubsumption (Type KindAnn TypeAnn) (Type KindAnn TypeAnn)
  | ErrorCheckingAccessor (Expr KindAnn TypeAnn ValueAnn) PSString
  | ErrorCheckingType (Expr KindAnn TypeAnn ValueAnn) (Type KindAnn TypeAnn)
  | ErrorCheckingKind (Type KindAnn TypeAnn)
  | ErrorCheckingGuard
  | ErrorInferringType (Expr KindAnn TypeAnn ValueAnn)
  | ErrorInApplication (Expr KindAnn TypeAnn ValueAnn) (Type KindAnn TypeAnn) (Expr KindAnn TypeAnn ValueAnn)
  | ErrorInDataConstructor (ProperName 'ConstructorName)
  | ErrorInTypeConstructor (ProperName 'TypeName)
  | ErrorInBindingGroup [Ident]
  | ErrorInDataBindingGroup [ProperName 'TypeName]
  | ErrorInTypeSynonym (ProperName 'TypeName)
  | ErrorInValueDeclaration Ident
  | ErrorInTypeDeclaration Ident
  | ErrorInTypeClassDeclaration (ProperName 'ClassName)
  | ErrorInForeignImport Ident
  | ErrorSolvingConstraint (Constraint KindAnn TypeAnn)
  | PositionedError SourceSpan
  deriving (Show)

-- | Categories of hints
data HintCategory
  = ExprHint
  | KindHint
  | CheckHint
  | PositionHint
  | SolverHint
  | OtherHint
  deriving (Show, Eq)

data ErrorMessage = ErrorMessage
  [ErrorMessageHint]
  SimpleErrorMessage
  deriving (Show)

-- |
-- A module declaration, consisting of comments about the module, a module name,
-- a list of declarations, and a list of the declarations that are
-- explicitly exported. If the export list is Nothing, everything is exported.
--
data Module ka ta va = Module SourceSpan [Comment] ModuleName [Declaration ka ta va] (Maybe [DeclarationRef])
  deriving (Show)

-- | Return a module's name.
getModuleName :: Module ka ta va -> ModuleName
getModuleName (Module _ _ name _ _) = name

-- | Return a module's source span.
getModuleSourceSpan :: Module ka ta va -> SourceSpan
getModuleSourceSpan (Module ss _ _ _ _) = ss

-- |
-- Add an import declaration for a module if it does not already explicitly import it.
--
addDefaultImport :: va -> ModuleName -> Module ka ta va -> Module ka ta va
addDefaultImport ann toImport m@(Module ss coms mn decls exps)  =
  if isExistingImport `any` decls || mn == toImport then m
  else Module ss coms mn (ImportDeclaration ann toImport Implicit Nothing : decls) exps
  where
  isExistingImport (ImportDeclaration _ mn' _ _) | mn' == toImport = True
  isExistingImport (PositionedDeclaration _ _ _ d) = isExistingImport d
  isExistingImport _ = False

-- TODO-ann: DeclarationRefs should have annotations too probably?
-- |
-- An item in a list of explicit imports or exports
--
data DeclarationRef
  -- |
  -- A type constructor with data constructors
  --
  = TypeRef (ProperName 'TypeName) (Maybe [ProperName 'ConstructorName])
  -- |
  -- A type operator
  --
  | TypeOpRef (OpName 'TypeOpName)
  -- |
  -- A value
  --
  | ValueRef Ident
  -- |
  -- A value-level operator
  --
  | ValueOpRef (OpName 'ValueOpName)
  -- |
  -- A type class
  --
  | TypeClassRef (ProperName 'ClassName)
  -- |
  -- A type class instance, created during typeclass desugaring (name, class name, instance types)
  --
  | TypeInstanceRef Ident
  -- |
  -- A module, in its entirety
  --
  | ModuleRef ModuleName
  -- |
  -- A named kind
  --
  | KindRef (ProperName 'KindName)
  -- |
  -- A value re-exported from another module. These will be inserted during
  -- elaboration in name desugaring.
  --
  | ReExportRef ModuleName DeclarationRef
  -- |
  -- A declaration reference with source position information
  --
  | PositionedDeclarationRef SourceSpan [Comment] DeclarationRef
  deriving (Show)

instance Eq DeclarationRef where
  (TypeRef name dctors) == (TypeRef name' dctors') = name == name' && dctors == dctors'
  (TypeOpRef name) == (TypeOpRef name') = name == name'
  (ValueRef name) == (ValueRef name') = name == name'
  (ValueOpRef name) == (ValueOpRef name') = name == name'
  (TypeClassRef name) == (TypeClassRef name') = name == name'
  (TypeInstanceRef name) == (TypeInstanceRef name') = name == name'
  (ModuleRef name) == (ModuleRef name') = name == name'
  (KindRef name) == (KindRef name') = name == name'
  (ReExportRef mn ref) == (ReExportRef mn' ref') = mn == mn' && ref == ref'
  (PositionedDeclarationRef _ _ r) == r' = r == r'
  r == (PositionedDeclarationRef _ _ r') = r == r'
  _ == _ = False

-- enable sorting lists of explicitly imported refs when suggesting imports in linting, IDE, etc.
-- not an Ord because this implementation is not consistent with its Eq instance.
-- think of it as a notion of contextual, not inherent, ordering.
compDecRef :: DeclarationRef -> DeclarationRef -> Ordering
compDecRef (TypeRef name _) (TypeRef name' _) = compare name name'
compDecRef (TypeOpRef name) (TypeOpRef name') = compare name name'
compDecRef (ValueRef ident) (ValueRef ident') = compare ident ident'
compDecRef (ValueOpRef name) (ValueOpRef name') = compare name name'
compDecRef (TypeClassRef name) (TypeClassRef name') = compare name name'
compDecRef (TypeInstanceRef ident) (TypeInstanceRef ident') = compare ident ident'
compDecRef (ModuleRef name) (ModuleRef name') = compare name name'
compDecRef (KindRef name) (KindRef name') = compare name name'
compDecRef (ReExportRef name _) (ReExportRef name' _) = compare name name'
compDecRef (PositionedDeclarationRef _ _ ref) ref' = compDecRef ref ref'
compDecRef ref (PositionedDeclarationRef _ _ ref') = compDecRef ref ref'
compDecRef ref ref' = compare
  (orderOf ref) (orderOf ref')
    where
      orderOf :: DeclarationRef -> Int
      orderOf (TypeClassRef _) = 0
      orderOf (TypeOpRef _) = 1
      orderOf (TypeRef _ _) = 2
      orderOf (ValueRef _) = 3
      orderOf (ValueOpRef _) = 4
      orderOf (KindRef _) = 5
      orderOf _ = 6

getTypeRef :: DeclarationRef -> Maybe (ProperName 'TypeName, Maybe [ProperName 'ConstructorName])
getTypeRef (TypeRef name dctors) = Just (name, dctors)
getTypeRef (PositionedDeclarationRef _ _ r) = getTypeRef r
getTypeRef _ = Nothing

getTypeOpRef :: DeclarationRef -> Maybe (OpName 'TypeOpName)
getTypeOpRef (TypeOpRef op) = Just op
getTypeOpRef (PositionedDeclarationRef _ _ r) = getTypeOpRef r
getTypeOpRef _ = Nothing

getValueRef :: DeclarationRef -> Maybe Ident
getValueRef (ValueRef name) = Just name
getValueRef (PositionedDeclarationRef _ _ r) = getValueRef r
getValueRef _ = Nothing

getValueOpRef :: DeclarationRef -> Maybe (OpName 'ValueOpName)
getValueOpRef (ValueOpRef op) = Just op
getValueOpRef (PositionedDeclarationRef _ _ r) = getValueOpRef r
getValueOpRef _ = Nothing

getTypeClassRef :: DeclarationRef -> Maybe (ProperName 'ClassName)
getTypeClassRef (TypeClassRef name) = Just name
getTypeClassRef (PositionedDeclarationRef _ _ r) = getTypeClassRef r
getTypeClassRef _ = Nothing

getKindRef :: DeclarationRef -> Maybe (ProperName 'KindName)
getKindRef (KindRef name) = Just name
getKindRef (PositionedDeclarationRef _ _ r) = getKindRef r
getKindRef _ = Nothing

isModuleRef :: DeclarationRef -> Bool
isModuleRef (PositionedDeclarationRef _ _ r) = isModuleRef r
isModuleRef (ModuleRef _) = True
isModuleRef _ = False

-- |
-- The data type which specifies type of import declaration
--
data ImportDeclarationType
  -- |
  -- An import with no explicit list: `import M`.
  --
  = Implicit
  -- |
  -- An import with an explicit list of references to import: `import M (foo)`
  --
  | Explicit [DeclarationRef]
  -- |
  -- An import with a list of references to hide: `import M hiding (foo)`
  --
  | Hiding [DeclarationRef]
  deriving (Eq, Show)

isImplicit :: ImportDeclarationType -> Bool
isImplicit Implicit = True
isImplicit _ = False

isExplicit :: ImportDeclarationType -> Bool
isExplicit (Explicit _) = True
isExplicit _ = False

-- |
-- The data type of declarations
--
data Declaration ka ta va
  -- |
  -- A data type declaration (data or newtype, name, arguments, data constructors)
  --
  = DataDeclaration va DataDeclType (ProperName 'TypeName) [(Text, Maybe (Kind ka))] [(ProperName 'ConstructorName, [Type ka ta])]
  -- |
  -- A minimal mutually recursive set of data type declarations
  --
  | DataBindingGroupDeclaration va [Declaration ka ta va]
  -- |
  -- A type synonym declaration (name, arguments, type)
  --
  | TypeSynonymDeclaration va (ProperName 'TypeName) [(Text, Maybe (Kind ka))] (Type ka ta)
  -- |
  -- A type declaration for a value (name, ty)
  --
  | TypeDeclaration va Ident (Type ka ta)
  -- |
  -- A value declaration (name, top-level binders, optional guard, value)
  --
  | ValueDeclaration va Ident NameKind [Binder ka ta va] [GuardedExpr ka ta va]
  -- |
  -- A declaration paired with pattern matching in let-in expression (binder, optional guard, value)
  | BoundValueDeclaration va (Binder ka ta va) (Expr ka ta va)
  -- |
  -- A minimal mutually recursive set of value declarations
  --
  | BindingGroupDeclaration va [(Ident, NameKind, Expr ka ta va)]
  -- |
  -- A foreign import declaration (name, type)
  --
  | ExternDeclaration va Ident (Type ka ta)
  -- |
  -- A data type foreign import (name, kind)
  --
  | ExternDataDeclaration va (ProperName 'TypeName) (Kind ka)
  -- |
  -- A foreign kind import (name)
  --
  | ExternKindDeclaration va (ProperName 'KindName)
  -- |
  -- A fixity declaration
  --
  | FixityDeclaration va (Either ValueFixity TypeFixity)
  -- |
  -- A module import (module name, qualified/unqualified/hiding, optional "qualified as" name)
  --
  | ImportDeclaration va ModuleName ImportDeclarationType (Maybe ModuleName)
  -- |
  -- A type class declaration (name, argument, implies, member declarations)
  --
  | TypeClassDeclaration va (ProperName 'ClassName) [(Text, Maybe (Kind ka))] [Constraint ka ta] [FunctionalDependency] [Declaration ka ta va]
  -- |
  -- A type instance declaration (name, dependencies, class name, instance types, member
  -- declarations)
  --
  | TypeInstanceDeclaration va Ident [Constraint ka ta] (Qualified (ProperName 'ClassName)) [Type ka ta] (TypeInstanceBody ka ta va)
  -- |
  -- A declaration with source position information
  --
  | PositionedDeclaration va SourceSpan [Comment] (Declaration ka ta va)
  deriving (Show)

data ValueFixity = ValueFixity Fixity (Qualified (Either Ident (ProperName 'ConstructorName))) (OpName 'ValueOpName)
  deriving (Eq, Ord, Show)

data TypeFixity = TypeFixity Fixity (Qualified (ProperName 'TypeName)) (OpName 'TypeOpName)
  deriving (Eq, Ord, Show)

pattern ValueFixityDeclaration
  :: va
  -> Fixity
  -> Qualified (Either Ident (ProperName 'ConstructorName))
  -> OpName 'ValueOpName
  -> Declaration ka ta va
pattern ValueFixityDeclaration ann fixity name op =
  FixityDeclaration ann (Left (ValueFixity fixity name op))

pattern TypeFixityDeclaration
  :: va
  -> Fixity
  -> Qualified (ProperName 'TypeName)
  -> OpName 'TypeOpName
  -> Declaration ka ta va
pattern TypeFixityDeclaration ann fixity name op =
  FixityDeclaration ann (Right (TypeFixity fixity name op))

-- | The members of a type class instance declaration
data TypeInstanceBody ka ta va
  = DerivedInstance
  -- ^ This is a derived instance
  | NewtypeInstance
  -- ^ This is an instance derived from a newtype
  | NewtypeInstanceWithDictionary (Expr ka ta va)
  -- ^ This is an instance derived from a newtype, desugared to include a
  -- dictionary for the type under the newtype.
  | ExplicitInstance [Declaration ka ta va]
  -- ^ This is a regular (explicit) instance
  deriving (Show)

mapTypeInstanceBody :: ([Declaration ka ta va] -> [Declaration ka ta va]) -> TypeInstanceBody ka ta va -> TypeInstanceBody ka ta va
mapTypeInstanceBody f = runIdentity . traverseTypeInstanceBody (Identity . f)

-- | A traversal for TypeInstanceBody
traverseTypeInstanceBody :: Applicative f => ([Declaration ka ta va] -> f [Declaration ka ta va]) -> TypeInstanceBody ka ta va -> f (TypeInstanceBody ka ta va)
traverseTypeInstanceBody f (ExplicitInstance ds) = ExplicitInstance <$> f ds
traverseTypeInstanceBody _ other = pure other

-- |
-- Test if a declaration is a value declaration
--
isValueDecl :: Declaration ka ta va -> Bool
isValueDecl ValueDeclaration{} = True
isValueDecl (PositionedDeclaration _ _ _ d) = isValueDecl d
isValueDecl _ = False

-- |
-- Test if a declaration is a data type or type synonym declaration
--
isDataDecl :: Declaration ka ta va -> Bool
isDataDecl DataDeclaration{} = True
isDataDecl TypeSynonymDeclaration{} = True
isDataDecl (PositionedDeclaration _ _ _ d) = isDataDecl d
isDataDecl _ = False

-- |
-- Test if a declaration is a module import
--
isImportDecl :: Declaration ka ta va-> Bool
isImportDecl ImportDeclaration{} = True
isImportDecl (PositionedDeclaration _ _ _ d) = isImportDecl d
isImportDecl _ = False

-- |
-- Test if a declaration is a data type foreign import
--
isExternDataDecl :: Declaration ka ta va-> Bool
isExternDataDecl ExternDataDeclaration{} = True
isExternDataDecl (PositionedDeclaration _ _ _ d) = isExternDataDecl d
isExternDataDecl _ = False

-- |
-- Test if a declaration is a foreign kind import
--
isExternKindDecl :: Declaration ka ta va-> Bool
isExternKindDecl ExternKindDeclaration{} = True
isExternKindDecl (PositionedDeclaration _ _ _ d) = isExternKindDecl d
isExternKindDecl _ = False

-- |
-- Test if a declaration is a fixity declaration
--
isFixityDecl :: Declaration ka ta va-> Bool
isFixityDecl FixityDeclaration{} = True
isFixityDecl (PositionedDeclaration _ _ _ d) = isFixityDecl d
isFixityDecl _ = False

getFixityDecl :: Declaration ka ta va-> Maybe (Either ValueFixity TypeFixity)
getFixityDecl (FixityDeclaration _ fixity) = Just fixity
getFixityDecl (PositionedDeclaration _ _ _ d) = getFixityDecl d
getFixityDecl _ = Nothing

-- |
-- Test if a declaration is a foreign import
--
isExternDecl :: Declaration ka ta va-> Bool
isExternDecl ExternDeclaration{} = True
isExternDecl (PositionedDeclaration _ _ _ d) = isExternDecl d
isExternDecl _ = False

-- |
-- Test if a declaration is a type class instance declaration
--
isTypeClassInstanceDeclaration :: Declaration ka ta va-> Bool
isTypeClassInstanceDeclaration TypeInstanceDeclaration{} = True
isTypeClassInstanceDeclaration (PositionedDeclaration _ _ _ d) = isTypeClassInstanceDeclaration d
isTypeClassInstanceDeclaration _ = False

-- |
-- Test if a declaration is a type class declaration
--
isTypeClassDeclaration :: Declaration ka ta va-> Bool
isTypeClassDeclaration TypeClassDeclaration{} = True
isTypeClassDeclaration (PositionedDeclaration _ _ _ d) = isTypeClassDeclaration d
isTypeClassDeclaration _ = False

-- |
-- Recursively flatten data binding groups in the list of declarations
flattenDecls :: [Declaration ka ta va] -> [Declaration ka ta va]
flattenDecls = concatMap flattenOne
    where
      flattenOne :: Declaration ka ta va-> [Declaration ka ta va]
      flattenOne (DataBindingGroupDeclaration _ decls) = concatMap flattenOne decls
      flattenOne d = [d]

-- |
-- A guard is just a boolean-valued expression that appears alongside a set of binders
--
data Guard ka ta va
  = ConditionGuard va (Expr ka ta va)
  | PatternGuard va (Binder ka ta va) (Expr ka ta va)
  deriving (Show)

-- |
-- The right hand side of a binder in value declarations
-- and case expressions.
data GuardedExpr ka ta va = GuardedExpr va [Guard ka ta va] (Expr ka ta va)
  deriving (Show)

pattern MkUnguarded :: va -> Expr ka ta va -> GuardedExpr ka ta va
pattern MkUnguarded ann e = GuardedExpr ann [] e

-- |
-- Data type for expressions and terms
--
data Expr ka ta va
  -- |
  -- A literal value
  --
  = Literal va (Literal (Expr ka ta va))
  -- |
  -- A prefix -, will be desugared
  --
  | UnaryMinus va (Expr ka ta va)
  -- |
  -- Binary operator application. During the rebracketing phase of desugaring, this data constructor
  -- will be removed.
  --
  | BinaryNoParens va (Expr ka ta va) (Expr ka ta va) (Expr ka ta va)
  -- |
  -- Explicit parentheses. During the rebracketing phase of desugaring, this data constructor
  -- will be removed.
  --
  -- Note: although it seems this constructor is not used, it _is_ useful, since it prevents
  -- certain traversals from matching.
  --
  | Parens va (Expr ka ta va)
  -- |
  -- An record property accessor expression (e.g. `obj.x` or `_.x`).
  -- Anonymous arguments will be removed during desugaring and expanded
  -- into a lambda that reads a property from a record.
  --
  | Accessor va PSString (Expr ka ta va)
  -- |
  -- Partial record update
  --
  | ObjectUpdate va (Expr ka ta va) [(PSString, Expr ka ta va)]
  -- |
  -- Object updates with nested support: `x { foo { bar = e } }`
  -- Replaced during desugaring into a `Let` and nested `ObjectUpdate`s
  --
  | ObjectUpdateNested va (Expr ka ta va) (PathTree (Expr ka ta va))
  -- |
  -- Function introduction
  --
  | Abs va (Binder ka ta va) (Expr ka ta va)
  -- |
  -- Function application
  --
  | App va (Expr ka ta va) (Expr ka ta va)
  -- |
  -- Variable
  --
  | Var va (Qualified Ident)
  -- |
  -- An operator. This will be desugared into a function during the "operators"
  -- phase of desugaring.
  --
  | Op va (Qualified (OpName 'ValueOpName))
  -- |
  -- Conditional (if-then-else expression)
  --
  | IfThenElse va (Expr ka ta va) (Expr ka ta va) (Expr ka ta va)
  -- |
  -- A data constructor
  --
  | Constructor va (Qualified (ProperName 'ConstructorName))
  -- |
  -- A case expression. During the case expansion phase of desugaring, top-level binders will get
  -- desugared into case expressions, hence the need for guards and multiple binders per branch here.
  --
  | Case va [Expr ka ta va] [CaseAlternative ka ta va]
  -- |
  -- A value with a type annotation
  --
  | TypedValue va Bool (Expr ka ta va) (Type ka ta)
  -- |
  -- A let binding
  --
  | Let va [Declaration ka ta va] (Expr ka ta va)
  -- |
  -- A do-notation block
  --
  | Do va [DoNotationElement ka ta va]
  -- |
  -- An application of a typeclass dictionary constructor. The value should be
  -- an ObjectLiteral.
  --
  | TypeClassDictionaryConstructorApp va (Qualified (ProperName 'ClassName)) (Expr ka ta va)
  -- |
  -- A placeholder for a type class dictionary to be inserted later. At the end of type checking, these
  -- placeholders will be replaced with actual expressions representing type classes dictionaries which
  -- can be evaluated at runtime. The constructor arguments represent (in order): whether or not to look
  -- at superclass implementations when searching for a dictionary, the type class name and
  -- instance type, and the type class dictionaries in scope.
  --
  | TypeClassDictionary
      va
      (Constraint ka ta)
      (M.Map (Maybe ModuleName) (M.Map (Qualified (ProperName 'ClassName)) (M.Map (Qualified Ident) NamedDict)))
      [ErrorMessageHint]
  -- |
  -- A typeclass dictionary accessor, the implementation is left unspecified until CoreFn desugaring.
  --
  | TypeClassDictionaryAccessor va (Qualified (ProperName 'ClassName)) Ident
  -- |
  -- A placeholder for a superclass dictionary to be turned into a TypeClassDictionary during typechecking
  --
  | DeferredDictionary va (Qualified (ProperName 'ClassName)) [Type ka ta]
  -- |
  -- A placeholder for an anonymous function argument
  --
  | AnonymousArgument va
  -- |
  -- A typed hole that will be turned into a hint/error duing typechecking
  --
  | Hole va Text
  -- |
  -- A value with source position information
  --
  | PositionedValue va SourceSpan [Comment] (Expr ka ta va)
  deriving (Show)

-- |
-- An alternative in a case statement
--
data CaseAlternative ka ta va = CaseAlternative
  { caseAlternativeAnn :: va
  -- ^ The annotation for the caseAlternative
  , caseAlternativeBinders :: [Binder ka ta va]
  -- ^ A collection of binders with which to match the inputs
  , caseAlternativeResult :: [GuardedExpr ka ta va]
  -- ^ The result expression or a collect of guarded expressions
  } deriving (Show)

-- |
-- A statement in a do-notation block
--
data DoNotationElement ka ta va
  -- |
  -- A monadic value without a binder
  --
  = DoNotationValue va (Expr ka ta va)
  -- |
  -- A monadic value with a binder
  --
  | DoNotationBind va (Binder ka ta va) (Expr ka ta va)
  -- |
  -- A let statement, i.e. a pure value with a binder
  --
  | DoNotationLet va [Declaration ka ta va]
  -- |
  -- A do notation element with source position information
  --
  | PositionedDoNotationElement va SourceSpan [Comment] (DoNotationElement ka ta va)
  deriving (Show)


-- For a record update such as:
--
--  x { foo = 0
--    , bar { baz = 1
--          , qux = 2 } }
--
-- We represent the updates as the `PathTree`:
--
--  [ ("foo", Leaf 3)
--  , ("bar", Branch [ ("baz", Leaf 1)
--                   , ("qux", Leaf 2) ]) ]
--
-- Which we then convert to an expression representing the following:
--
--   let x' = x
--   in x' { foo = 0
--         , bar = x'.bar { baz = 1
--                        , qux = 2 } }
--
-- The `let` here is required to prevent re-evaluating the object expression `x`.
-- However we don't generate this when using an anonymous argument for the object.
--

newtype PathTree t = PathTree (AssocList PSString (PathNode t))
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data PathNode t = Leaf t | Branch (PathTree t)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

newtype AssocList k t = AssocList { runAssocList :: [(k, t)] }
  deriving (Show, Eq, Ord, Foldable, Functor, Traversable)

$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''DeclarationRef)
$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''ImportDeclarationType)

isTrueExpr :: Expr ka ta va -> Bool
isTrueExpr (Literal _ (BooleanLiteral True)) = True
isTrueExpr (Var _ (Qualified (Just (ModuleName [ProperName "Prelude"])) (Ident "otherwise"))) = True
isTrueExpr (Var _ (Qualified (Just (ModuleName [ProperName "Data", ProperName "Boolean"])) (Ident "otherwise"))) = True
isTrueExpr (TypedValue _ _ e _) = isTrueExpr e
isTrueExpr (PositionedValue _ _ _ e) = isTrueExpr e
isTrueExpr _ = False
