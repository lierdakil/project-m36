{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveLift,
             StandaloneDeriving, TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module TutorialD.THOrphans () where

import ProjectM36.Base

import Data.UUID (UUID)
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

instance Lift Day where
  lift = liftData

instance Lift UTCTime where
  lift = liftData

instance Lift UUID where
  lift = liftData

instance Lift Text.Text where
  lift = wrapUnwrap 'Text.pack Text.unpack

instance Lift ByteString.ByteString where
  lift = wrapUnwrap 'ByteString.pack ByteString.unpack

instance (Lift k, Lift v) => Lift (Map.Map k v) where
  lift = wrapUnwrap 'Map.fromList Map.toList

instance Lift a => Lift (Set.Set a) where
  lift = wrapUnwrap 'Set.fromList Set.toList

instance Lift a => Lift (Vector.Vector a) where
  lift = wrapUnwrap 'Vector.fromList Vector.toList

wrapUnwrap :: Lift t => Name -> (b -> t) -> b -> Q Exp
wrapUnwrap wn u = (AppE (VarE wn) <$>) . lift . u

instance Lift AtomExpr where
  lift (AttributeAtomExpr x) = AppE (ConE 'AttributeAtomExpr) <$> lift x
  lift (NakedAtomExpr x) = AppE (ConE 'NakedAtomExpr) <$> lift x
  lift (FunctionAtomExpr x y z) = [|FunctionAtomExpr $(lift x) $(lift y) $(lift z)|]
  lift (RelationAtomExpr x) = AppE (ConE 'RelationAtomExpr) <$> lift x
  lift (PlaceholderAtomExpr x) = [|NakedAtomExpr (toAtom $(varE . mkName . Text.unpack $ x))|]
  lift (ConstructedAtomExpr x y z) = [|ConstructedAtomExpr $(lift x) $(lift y) $(lift z)|]

deriving instance Lift Atom
deriving instance Lift AtomType
deriving instance Lift Attribute
deriving instance Lift AttributeExpr
deriving instance Lift TupleExpr
deriving instance Lift Relation
deriving instance Lift AttributeNames
deriving instance Lift RelationTuple
deriving instance Lift RelationTupleSet
deriving instance Lift TypeConstructor
deriving instance Lift ExtendTupleExpr
deriving instance Lift InclusionDependency
deriving instance Lift TypeConstructorDef
deriving instance Lift DataConstructorDef
deriving instance Lift DataConstructorDefArg
deriving instance Lift RestrictionPredicateExpr
deriving instance Lift RelationalExpr
deriving instance Lift DatabaseContextExpr
deriving instance Lift DatabaseContextIOExpr
