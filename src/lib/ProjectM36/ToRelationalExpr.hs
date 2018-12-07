{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ProjectM36.ToRelationalExpr where

import ProjectM36.Base
import ProjectM36.Tupleable
import ProjectM36.TupleSet
import Data.Proxy
import Data.Foldable

class ToRelationalExpr a where
  toRelationalExpr :: a -> RelationalExpr

instance ToRelationalExpr RelationalExpr where
  toRelationalExpr val = val

instance ToRelationalExpr Relation where
  toRelationalExpr val = ExistingRelation val

instance {-# OVERLAPPABLE #-} (Tupleable a) => ToRelationalExpr a where
  toRelationalExpr val = either (error . show) Prelude.id $ do
      let attrs = toAttributes (Proxy :: Proxy a)
      tuples <- mkTupleSet attrs [toTuple val]
      let rel = MakeStaticRelation attrs tuples
      pure rel

instance (Tupleable a, Traversable t) => ToRelationalExpr (t a) where
  toRelationalExpr vals = either (error . show) Prelude.id $ do
      let attrs = toAttributes (Proxy :: Proxy a)
      tuples <- mkTupleSet attrs $ toList (fmap toTuple vals)
      let rel = MakeStaticRelation attrs tuples
      pure rel
