{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LinearTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-overlapping-patterns #-}

module Control.Category.Relax where
import Data.Kind
import Prelude hiding ((.),id,curry)
import Control.Category.Constrained

newtype Relax (con :: * -> Constraint) k a b = Relax (k a b)

instance (Monoidal k, ProdObj con, Trivial ~ Obj k) => Monoidal (Relax con k) where
  Relax f × Relax g = Relax (f × g)
  swap = Relax swap
  assoc = Relax assoc
  assoc' = Relax assoc'
  unitor = Relax unitor
  unitor' = Relax unitor'
  
instance (Category k, Trivial ~ Obj k) => Category (Relax con k) where
  type Obj (Relax con k) = con
  id = Relax id
  Relax f ∘ Relax g = Relax (f ∘ g)
