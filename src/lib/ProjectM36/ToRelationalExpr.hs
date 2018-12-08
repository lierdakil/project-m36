{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, MultiParamTypeClasses #-}
module ProjectM36.ToRelationalExpr where

import ProjectM36.Base
import ProjectM36.Tupleable
import ProjectM36.TupleSet
import Data.Proxy
import Data.Foldable

type family ToRelationalExprDispatcher a :: Bool where
  ToRelationalExprDispatcher RelationalExpr = 'True
  ToRelationalExprDispatcher Relation = 'True
  ToRelationalExprDispatcher a = 'False

class ToRelationalExpr a where
  toRelationalExpr :: a -> RelationalExpr

instance (ToRelationalExprDispatcher a ~ flag, ToRelationalExpr' flag a)
       => ToRelationalExpr a where
  toRelationalExpr = toRelationalExpr' (Proxy :: Proxy flag)

class ToRelationalExpr' (flag :: Bool) a where
  toRelationalExpr' :: Proxy flag -> a -> RelationalExpr

instance ToRelationalExpr' 'True RelationalExpr where
  toRelationalExpr' _ val = val

instance ToRelationalExpr' 'True Relation where
  toRelationalExpr' _ val = ExistingRelation val

instance {-# OVERLAPPABLE #-} (Tupleable a) => ToRelationalExpr' 'False a where
  toRelationalExpr' _ val = either (error . show) Prelude.id $ do
      let attrs = toAttributes (Proxy :: Proxy a)
      tuples <- mkTupleSet attrs [toTuple val]
      let rel = MakeStaticRelation attrs tuples
      pure rel

instance (Tupleable a, Traversable t) => ToRelationalExpr' 'False (t a) where
  toRelationalExpr' _ vals = either (error . show) Prelude.id $ do
      let attrs = toAttributes (Proxy :: Proxy a)
      tuples <- mkTupleSet attrs $ toList (fmap toTuple vals)
      let rel = MakeStaticRelation attrs tuples
      pure rel
