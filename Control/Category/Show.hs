{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Category.Show where


import Prelude hiding ((.),id,curry,Num(..))
import Control.Category.Constrained

newtype Sho a b = Sho {fromSho :: Int -> ShowS}

instance Show (Sho a b) where
  showsPrec d (Sho f) = f d

shoCon :: String -> Sho a b
shoCon name = Sho $ \_ -> showString name

instance Category Sho where
  type Obj Sho = Trivial
  id = shoCon "id"
  Sho f ∘ Sho g = Sho $ \d -> showParen (d > 3) (f 3 . showString " ∘ " . g 3)

instance Monoidal Sho where
  swap = shoCon "swap"
  assoc = shoCon "assoc"
  assoc' = shoCon "assoc'"
  unitor = shoCon "unitor"
  unitor' = shoCon "unitor'"
  Sho f × Sho g = Sho $ \d -> showParen (d >= 2) (f 2 . showString " × " . g 2)

instance Cartesian Sho where
  dis = shoCon "dis"
  dup = shoCon "dup"
  exl = shoCon "exl"
  exr = shoCon "exr"
  Sho f ▵ Sho g = Sho $ \d -> showParen (d >= 2) (f 2 . showString " ▵ " . g 2)

instance CoCartesian Sho where
  new = shoCon "new"
  jam = shoCon "jam"
  inl = shoCon "inl"
  inr = shoCon "inr"
  Sho f ▿ Sho g = Sho $ \d -> showParen (d >= 2) (f 2 . showString " ▿ " . g 2)

-- instance Autonomous Sho where
--   turn' = shoCon "turn'"
--   turn = shoCon "turn"

class HasShow k where
  toShow :: k a b -> Sho a b

instance HasShow Sho where
  toShow = id

-- instance Additive (Sho a b) where
--   Sho f + Sho g = Sho $ \d -> showParen (d > 6) (f 6 . showString " + " . g 6)
--   zero = Sho $ \_ -> ("0"<>)

-- instance Group (Sho a b) where
--   Sho f - Sho g = Sho $ \d -> showParen (d > 6) (f 6 . showString " - " . g 7)
--   negate (Sho f) = Sho $ \d -> showParen (d > 6) (showString "- " . f 7)

-- instance Read2 f Sho a b where
--   read2 x _ _ = Sho $ \_ -> (x++) 

instance Read (Sho a b) where
  readsPrec _ x = [(Sho $ \_ -> (x++), "")] 

-- instance TensorCategory v Sho where
--   metric = (shoCon ("g"))
--   cometric =  shoCon ("g'")
--   christoffel =  shoCon ("Γ")

-- instance HasPartial v Sho where
--   partialDerivative  (Sho f) = Sho $ \d -> showParen (d>2) (showString "∂" . (f 2))

-- instance HasDerivative v Sho where
--   derivSemantics (Sho f) = Sho $ \d -> showParen (d>2) (showString "∇" . (f 2))
