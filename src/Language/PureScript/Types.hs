{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Data types for types
--
module Language.PureScript.Types where

import Prelude.Compat
import Protolude (ordNub)

import Control.Arrow (first)
import Control.DeepSeq (NFData)
import Control.Monad ((<=<))
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.Functor (void)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Language.PureScript.AST.SourcePos
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.Label (Label)
import Language.PureScript.PSString (PSString)

-- |
-- An identifier for the scope of a skolem variable
--
newtype SkolemScope = SkolemScope { runSkolemScope :: Int }
  deriving (Show, Eq, Ord, A.ToJSON, A.FromJSON, Generic)

instance NFData SkolemScope

-- |
-- The type of types
--
data Type ka ta
  -- | A unification variable of type Type
  = TUnknown ta Int
  -- | A named type variable
  | TypeVar ta Text
  -- | A type-level string
  | TypeLevelString ta PSString
  -- | A type wildcard, as would appear in a partial type synonym
  | TypeWildcard ta SourceSpan
  -- | A type constructor
  | TypeConstructor ta (Qualified (ProperName 'TypeName))
  -- | A type operator. This will be desugared into a type constructor during the
  -- "operators" phase of desugaring.
  | TypeOp ta (Qualified (OpName 'TypeOpName))
  -- | A type application
  | TypeApp ta (Type ka ta) (Type ka ta)
  -- | Forall quantifier
  | ForAll ta Text (Type ka ta) (Maybe SkolemScope)
  -- | A type with a set of type class constraints
  | ConstrainedType ta (Constraint ka ta) (Type ka ta)
  -- | A skolem constant
  | Skolem ta Text Int SkolemScope (Maybe SourceSpan)
  -- | An empty row
  | REmpty ta
  -- | A non-empty row
  | RCons ta Label (Type ka ta) (Type ka ta)
  -- | A type with a kind annotation
  | KindedType ta (Type ka ta) (Kind ka)
  -- | A placeholder used in pretty printing
  | PrettyPrintFunction ta (Type ka ta) (Type ka ta)
  -- | A placeholder used in pretty printing
  | PrettyPrintObject ta (Type ka ta)
  -- | A placeholder used in pretty printing
  | PrettyPrintForAll ta [Text] (Type ka ta)
  -- | Binary operator application. During the rebracketing phase of desugaring,
  -- this data constructor will be removed.
  | BinaryNoParensType ta (Type ka ta) (Type ka ta) (Type ka ta)
  -- | Explicit parentheses. During the rebracketing phase of desugaring, this
  -- data constructor will be removed.
  --
  -- Note: although it seems this constructor is not used, it _is_ useful,
  -- since it prevents certain traversals from matching.
  | ParensInType ta (Type ka ta)
  deriving (Show, Eq, Ord, Generic, Functor)

instance (NFData ka, NFData ta) => NFData (Type ka ta)

-- | Extracts the annotation from a type.
typeAnn :: Type ka ta -> ta
typeAnn (TUnknown ann _) = ann
typeAnn (TypeVar ann _) = ann
typeAnn (TypeLevelString ann _) = ann
typeAnn (TypeWildcard ann _) = ann
typeAnn (TypeConstructor ann _) = ann
typeAnn (TypeOp ann _) = ann
typeAnn (TypeApp ann _ _) = ann
typeAnn (ForAll ann _ _ _) = ann
typeAnn (ConstrainedType ann _ _) = ann
typeAnn (Skolem ann _ _ _ _) = ann
typeAnn (REmpty ann) = ann
typeAnn (RCons ann _ _ _) = ann
typeAnn (KindedType ann _ _) = ann
typeAnn (PrettyPrintFunction ann _ _) = ann
typeAnn (PrettyPrintObject ann _) = ann
typeAnn (PrettyPrintForAll ann _ _) = ann
typeAnn (BinaryNoParensType ann _ _ _) = ann
typeAnn (ParensInType ann _) = ann

eraseTypeAnns :: Type ka ta -> Type () ()
eraseTypeAnns (TUnknown _ x) = TUnknown () x
eraseTypeAnns (TypeVar _ x) = TypeVar () x
eraseTypeAnns (TypeLevelString _ x) = TypeLevelString () x
eraseTypeAnns (TypeWildcard _ x) = TypeWildcard () x
eraseTypeAnns (TypeConstructor _ x) = TypeConstructor () x
eraseTypeAnns (TypeOp _ x) = TypeOp () x
eraseTypeAnns (TypeApp _ x y) = TypeApp () (eraseTypeAnns x) (eraseTypeAnns y)
eraseTypeAnns (ForAll _ x y z) = ForAll () x (eraseTypeAnns y) z
eraseTypeAnns (ConstrainedType _ x y) = ConstrainedType () (eraseConstraintAnns x) (eraseTypeAnns y)
eraseTypeAnns (Skolem _ x y z w) = Skolem () x y z w
eraseTypeAnns (REmpty _) = REmpty ()
eraseTypeAnns (RCons _ x y z) = RCons () x (eraseTypeAnns y) (eraseTypeAnns z)
eraseTypeAnns (KindedType _ x y) = KindedType () (eraseTypeAnns x) (void y)
eraseTypeAnns (PrettyPrintFunction _ x y) = PrettyPrintFunction () (eraseTypeAnns x) (eraseTypeAnns y)
eraseTypeAnns (PrettyPrintObject _ x) = PrettyPrintObject () (eraseTypeAnns x)
eraseTypeAnns (PrettyPrintForAll _ x y) = PrettyPrintForAll () x (eraseTypeAnns y)
eraseTypeAnns (BinaryNoParensType _ x y z) = BinaryNoParensType () (eraseTypeAnns x) (eraseTypeAnns y) (eraseTypeAnns z)
eraseTypeAnns (ParensInType _ x) = ParensInType () (eraseTypeAnns x)

-- | Additional data relevant to type class constraints
data ConstraintData
  = PartialConstraintData [[Text]] Bool
  -- ^ Data to accompany a Partial constraint generated by the exhaustivity checker.
  -- It contains (rendered) binder information for those binders which were
  -- not matched, and a flag indicating whether the list was truncated or not.
  -- Note: we use 'Text' here because using 'Binder' would introduce a cyclic
  -- dependency in the module graph.
  deriving (Show, Eq, Ord, Generic)

instance NFData ConstraintData

-- | A typeclass constraint
data Constraint ka ta = Constraint
  { constraintAnn :: ta
  -- ^ constraint annotation
  , constraintClass :: Qualified (ProperName 'ClassName)
  -- ^ constraint class name
  , constraintArgs :: [Type ka ta]
  -- ^ type arguments
  , constraintData :: Maybe ConstraintData
  -- ^ additional data relevant to this constraint
  } deriving (Show, Eq, Ord, Generic, Functor)

instance (NFData ka, NFData ta) => NFData (Constraint ka ta)

eraseConstraintAnns :: Constraint ka ta -> Constraint () ()
eraseConstraintAnns Constraint{..} =
  Constraint () constraintClass (fmap eraseTypeAnns constraintArgs) constraintData

mapConstraintArgs :: ([Type ka ta] -> [Type ka ta]) -> Constraint ka ta -> Constraint ka ta
mapConstraintArgs f c = c { constraintArgs = f (constraintArgs c) }

overConstraintArgs :: Functor f => ([Type ka ta] -> f [Type ka ta]) -> Constraint ka ta -> f (Constraint ka ta)
overConstraintArgs f c = (\args -> c { constraintArgs = args }) <$> f (constraintArgs c)

-- TODO-ann: see Kinds for problem with deriving `fromJSON` here :/
-- $(A.deriveJSON A.defaultOptions ''Type)
-- $(A.deriveJSON A.defaultOptions ''Constraint)
$(A.deriveJSON A.defaultOptions ''ConstraintData)

-- | Convert a row to a list of pairs of labels and types
rowToList :: Type ka ta -> ([(Label, Type ka ta)], Type ka ta)
rowToList = go
  where
    go (RCons _ name ty row) = first ((name, ty) :) (rowToList row)
    go r = ([], r)

-- | Convert a row to a list of pairs of labels and types, sorted by the labels.
rowToSortedList :: Type ka ta -> ([(Label, Type ka ta)], Type ka ta)
rowToSortedList = first (sortBy (comparing fst)) . rowToList

-- | Convert a list of labels and types to a row
rowFromList :: ([(ta, Label, Type ka ta)], Type ka ta) -> Type ka ta
rowFromList (xs, r) = foldr (\(ann, l, ty) -> RCons ann l ty) r xs

-- | Check whether a type is a monotype
isMonoType :: Type ka ta -> Bool
isMonoType ForAll{} = False
isMonoType (ParensInType _ t) = isMonoType t
isMonoType (KindedType _ t _) = isMonoType t
isMonoType _ = True

-- | Universally quantify a type
mkForAll :: ta -> [Text] -> Type ka ta -> Type ka ta
mkForAll ann args ty = foldl (\t arg -> ForAll ann arg t Nothing) ty args

-- | Replace a type variable, taking into account variable shadowing
replaceTypeVars :: Text -> Type ka ta -> Type ka ta -> Type ka ta
replaceTypeVars v r = replaceAllTypeVars [(v, r)]

-- | Replace named type variables with types
replaceAllTypeVars :: [(Text, Type ka ta)] -> Type ka ta -> Type ka ta
replaceAllTypeVars = go []
  where
    go :: [Text] -> [(Text, Type ka ta)] -> Type ka ta -> Type ka ta
    go _  m tv@(TypeVar _ v) = fromMaybe tv (v `lookup` m)
    go bs m (TypeApp ann t1 t2) = TypeApp ann (go bs m t1) (go bs m t2)
    go bs m f@(ForAll ann v t sco)
        | v `elem` keys = go bs (filter ((/= v) . fst) m) f
        | v `elem` usedVars =
            let v' = genName v (keys <> bs <> usedVars)
                t' = go bs [(v, TypeVar (typeAnn t) v')] t -- TODO-ann: correct annotation?
            in ForAll ann v' (go (v' : bs) m t') sco
        | otherwise = ForAll ann v (go (v : bs) m t) sco
      where
        keys = fmap fst m
        usedVars = usedTypeVariables . snd =<< m
    go bs m (ConstrainedType ann c t) = ConstrainedType ann (mapConstraintArgs (fmap (go bs m)) c) (go bs m t)
    go bs m (RCons ann name' t r) = RCons ann name' (go bs m t) (go bs m r)
    go bs m (KindedType ann t k) = KindedType ann (go bs m t) k
    go bs m (BinaryNoParensType ann t1 t2 t3) = BinaryNoParensType ann (go bs m t1) (go bs m t2) (go bs m t3)
    go bs m (ParensInType ann t) = ParensInType ann (go bs m t)
    go _  _ ty = ty

    genName :: Text -> [Text] -> Text
    genName orig inUse = try' 0
      where
        try' :: Integer -> Text
        try' n | (orig <> T.pack (show n)) `elem` inUse = try' (n + 1)
               | otherwise = orig <> T.pack (show n)

-- | Collect all type variables appearing in a type
usedTypeVariables :: Type ka ta -> [Text]
usedTypeVariables = ordNub . everythingOnTypes (++) go
  where
    go (TypeVar _ v) = [v]
    go _ = []

-- | Collect all free type variables appearing in a type
freeTypeVariables :: Type ka ta -> [Text]
freeTypeVariables = ordNub . go []
  where
    go :: [Text] -> Type ka ta -> [Text]
    go bound (TypeVar _ v) | v `notElem` bound = [v]
    go bound (TypeApp _ t1 t2) = go bound t1 <> go bound t2
    go bound (ForAll _ v t _) = go (v : bound) t
    go bound (ConstrainedType _ c t) = (go bound =<< constraintArgs c) <> go bound t
    go bound (RCons _ _ t r) = go bound t <> go bound r
    go bound (KindedType _ t _) = go bound t
    go bound (BinaryNoParensType _ t1 t2 t3) = go bound t1 <> go bound t2 <> go bound t3
    go bound (ParensInType _ t) = go bound t
    go _ _ = []

-- | Universally quantify over all type variables appearing free in a type
quantify :: Type ka ta -> Type ka ta
quantify ty = foldr (\arg t -> ForAll (typeAnn t) arg t Nothing) ty $ freeTypeVariables ty -- TODO-ann: correct annotation?

-- | Move all universal quantifiers to the front of a type
moveQuantifiersToFront :: Type ka ta -> Type ka ta
moveQuantifiersToFront = go [] []
  where
    go qs cs (ForAll ann q ty sco) = go ((ann, q, sco) : qs) cs ty
    go qs cs (ConstrainedType ann c ty) = go qs ((ann, c) : cs) ty
    go qs cs ty = foldl (\ty' (ann, q, sco) -> ForAll ann q ty' sco) (foldl (\ty' (ann, c) -> ConstrainedType ann c ty') ty cs) qs

-- | Check if a type contains wildcards
containsWildcards :: Type ka ta -> Bool
containsWildcards = everythingOnTypes (||) go
  where
    go :: Type ka ta -> Bool
    go TypeWildcard{} = True
    go _ = False

everywhereOnTypes :: (Type ka ta -> Type ka ta) -> Type ka ta -> Type ka ta
everywhereOnTypes f = go
  where
    go (TypeApp ann t1 t2) = f (TypeApp ann (go t1) (go t2))
    go (ForAll ann arg ty sco) = f (ForAll ann arg (go ty) sco)
    go (ConstrainedType ann c ty) = f (ConstrainedType ann (mapConstraintArgs (fmap go) c) (go ty))
    go (RCons ann name ty rest) = f (RCons ann name (go ty) (go rest))
    go (KindedType ann ty k) = f (KindedType ann (go ty) k)
    go (PrettyPrintFunction ann t1 t2) = f (PrettyPrintFunction ann (go t1) (go t2))
    go (PrettyPrintObject ann t) = f (PrettyPrintObject ann (go t))
    go (PrettyPrintForAll ann args t) = f (PrettyPrintForAll ann args (go t))
    go (BinaryNoParensType ann t1 t2 t3) = f (BinaryNoParensType ann (go t1) (go t2) (go t3))
    go (ParensInType ann t) = f (ParensInType ann (go t))
    go other = f other

everywhereOnTypesTopDown :: (Type ka ta -> Type ka ta) -> Type ka ta -> Type ka ta
everywhereOnTypesTopDown f = go . f
  where
    go (TypeApp ann t1 t2) = TypeApp ann (go (f t1)) (go (f t2))
    go (ForAll ann arg ty sco) = ForAll ann arg (go (f ty)) sco
    go (ConstrainedType ann c ty) = ConstrainedType ann (mapConstraintArgs (fmap (go . f)) c) (go (f ty))
    go (RCons ann name ty rest) = RCons ann name (go (f ty)) (go (f rest))
    go (KindedType ann ty k) = KindedType ann (go (f ty)) k
    go (PrettyPrintFunction ann t1 t2) = PrettyPrintFunction ann (go (f t1)) (go (f t2))
    go (PrettyPrintObject ann t) = PrettyPrintObject ann (go (f t))
    go (PrettyPrintForAll ann args t) = PrettyPrintForAll ann args (go (f t))
    go (BinaryNoParensType ann t1 t2 t3) = BinaryNoParensType ann (go (f t1)) (go (f t2)) (go (f t3))
    go (ParensInType ann t) = ParensInType ann (go (f t))
    go other = f other

everywhereOnTypesM :: Monad m => (Type ka ta -> m (Type ka ta)) -> Type ka ta -> m (Type ka ta)
everywhereOnTypesM f = go
  where
    go (TypeApp ann t1 t2) = (TypeApp ann <$> go t1 <*> go t2) >>= f
    go (ForAll ann arg ty sco) = (ForAll ann arg <$> go ty <*> pure sco) >>= f
    go (ConstrainedType ann c ty) = (ConstrainedType ann <$> overConstraintArgs (mapM go) c <*> go ty) >>= f
    go (RCons ann name ty rest) = (RCons ann name <$> go ty <*> go rest) >>= f
    go (KindedType ann ty k) = (KindedType ann <$> go ty <*> pure k) >>= f
    go (PrettyPrintFunction ann t1 t2) = (PrettyPrintFunction ann <$> go t1 <*> go t2) >>= f
    go (PrettyPrintObject ann t) = (PrettyPrintObject ann <$> go t) >>= f
    go (PrettyPrintForAll ann args t) = (PrettyPrintForAll ann args <$> go t) >>= f
    go (BinaryNoParensType ann t1 t2 t3) = (BinaryNoParensType ann <$> go t1 <*> go t2 <*> go t3) >>= f
    go (ParensInType ann t) = (ParensInType ann <$> go t) >>= f
    go other = f other

everywhereOnTypesTopDownM :: Monad m => (Type ka ta -> m (Type ka ta)) -> Type ka ta -> m (Type ka ta)
everywhereOnTypesTopDownM f = go <=< f
  where
    go (TypeApp ann t1 t2) = TypeApp ann <$> (f t1 >>= go) <*> (f t2 >>= go)
    go (ForAll ann arg ty sco) = ForAll ann arg <$> (f ty >>= go) <*> pure sco
    go (ConstrainedType ann c ty) = ConstrainedType ann <$> overConstraintArgs (mapM (go <=< f)) c <*> (f ty >>= go)
    go (RCons ann name ty rest) = RCons ann name <$> (f ty >>= go) <*> (f rest >>= go)
    go (KindedType ann ty k) = KindedType ann <$> (f ty >>= go) <*> pure k
    go (PrettyPrintFunction ann t1 t2) = PrettyPrintFunction ann <$> (f t1 >>= go) <*> (f t2 >>= go)
    go (PrettyPrintObject ann t) = PrettyPrintObject ann <$> (f t >>= go)
    go (PrettyPrintForAll ann args t) = PrettyPrintForAll ann args <$> (f t >>= go)
    go (BinaryNoParensType ann t1 t2 t3) = BinaryNoParensType ann <$> (f t1 >>= go) <*> (f t2 >>= go) <*> (f t3 >>= go)
    go (ParensInType ann t) = ParensInType ann <$> (f t >>= go)
    go other = f other

everythingOnTypes :: (r -> r -> r) -> (Type ka ta -> r) -> Type ka ta -> r
everythingOnTypes (<+>) f = go
  where
    go t@(TypeApp _ t1 t2) = f t <+> go t1 <+> go t2
    go t@(ForAll _ _ ty _) = f t <+> go ty
    go t@(ConstrainedType _ c ty) = foldl (<+>) (f t) (fmap go (constraintArgs c)) <+> go ty
    go t@(RCons _ _ ty rest) = f t <+> go ty <+> go rest
    go t@(KindedType _ ty _) = f t <+> go ty
    go t@(PrettyPrintFunction _ t1 t2) = f t <+> go t1 <+> go t2
    go t@(PrettyPrintObject _ t1) = f t <+> go t1
    go t@(PrettyPrintForAll _ _ t1) = f t <+> go t1
    go t@(BinaryNoParensType _ t1 t2 t3) = f t <+> go t1 <+> go t2 <+> go t3
    go t@(ParensInType _ t1) = f t <+> go t1
    go other = f other

everythingWithContextOnTypes :: s -> r -> (r -> r -> r) -> (s -> Type ka ta -> (s, r)) -> Type ka ta -> r
everythingWithContextOnTypes s0 r0 (<+>) f = go' s0
  where
    go' s t = let (s', r) = f s t in r <+> go s' t
    go s (TypeApp _ t1 t2) = go' s t1 <+> go' s t2
    go s (ForAll _ _ ty _) = go' s ty
    go s (ConstrainedType _ c ty) = foldl (<+>) r0 (fmap (go' s) (constraintArgs c)) <+> go' s ty
    go s (RCons _ _ ty rest) = go' s ty <+> go' s rest
    go s (KindedType _ ty _) = go' s ty
    go s (PrettyPrintFunction _ t1 t2) = go' s t1 <+> go' s t2
    go s (PrettyPrintObject _ t1) = go' s t1
    go s (PrettyPrintForAll _ _ t1) = go' s t1
    go s (BinaryNoParensType _ t1 t2 t3) = go' s t1 <+> go' s t2 <+> go' s t3
    go s (ParensInType _ t1) = go' s t1
    go _ _ = r0
