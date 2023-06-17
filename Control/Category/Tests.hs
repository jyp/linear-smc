{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UnicodeSyntax #-}


module Control.Category.Tests (
  -- Interface
  type P, unit, split, merge,
  -- pattern (:::),
  encode, decode, (!:),
  -- Helpers for cartesian categories
  ignore, copy, discard
) where


import Prelude hiding ((.),id,curry,LT,GT,EQ)
import Control.Category.Constrained
import Control.Category.FreeSMC as SMC
import Control.Category.Linear
import Control.Category.FC9 (FreeCartesian, toTrie)

data A = A
data B = B
data C = C
data D = D
-- type Ca = Sho
type Ca = SMC.Cat Sho Trivial
type Q = P Ca
type Z = FreeCartesian Ca (Obj Ca)


(&) ::  a ⊸ (a ⊸ b) ⊸ b
x & f = f x


φ :: Q r d ⊸ Q r (a⊗(b⊗c))
φ = encode (SMC.Embed (shoCon "φ"))

ξ :: Q r a ⊸ Q r b ⊸ Q r c
ξ x y = encode (SMC.Embed (shoCon "ξ")) (merge (x,y))

ζ :: Q r a ⊸ Q r b ⊸ Q r c
ζ x y = encode (SMC.Embed (shoCon "ζ")) (merge (x,y))

ω :: Q r c ⊸ Q r d ⊸ Q r (a⊗b)
ω x y = encode (SMC.Embed (shoCon "ω")) (merge (x,y))

ex3 :: Q r (A ⊗ B) ⊸ Q r (C, D)
ex3 az = split az & \(a,z) ->
         split (φ a) & \(y,xw) ->
         split xw & \(x,w) ->
         split (ω x z) & \(c,d) ->
         ξ y c !: ζ w d

t3 ::  Z (A ⊗ B) (C, D)
t3 = extract ex3

s3 ::  Ca (A ⊗ B) (C, D)
s3 = decode ex3

-- >>> t3
-- ξ ∘ (π₁ ∘ φ ∘ π₁ ▵ π₁ ∘ ω ∘ (π₁ ∘ π₂ ∘ φ ∘ π₁ ▵ π₂)) ▵ ζ ∘ (π₂ ∘ π₂ ∘ φ ∘ π₁ ▵ π₂ ∘ ω ∘ (π₁ ∘ π₂ ∘ φ ∘ π₁ ▵ π₂))


-- >>> toTrie t3
-- ⟨(⟨1φ1!,(⟨1φ21!,2!⟩).ω1!⟩).ξ!,(⟨1φ22!,(⟨1φ21!,2!⟩).ω2!⟩).ζ!⟩

-- >>> s3
-- (ξ × ζ) ∘ assoc' ∘ (id × (assoc ∘ (swap × id) ∘ assoc' ∘ (id × ω) ∘ assoc ∘ (swap × id))) ∘ assoc ∘ (φ × id)
