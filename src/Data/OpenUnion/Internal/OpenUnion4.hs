{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

-- Only for MemberU below, when emulating Monad Transformers
{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}

-- Open unions (type-indexed co-products) for extensible effects
-- This implementation relies on _closed_ type families added
-- to GHC 7.8
-- It has NO overlapping instances and NO Typeable
-- Alas, the absence of Typeable means the projections and injections
-- generally take linear time.
-- The code illustrate how to use closed type families to
-- disambiguate otherwise overlapping instances.

-- The interface is the same as of other OpenUnion*.hs
module Data.OpenUnion.Internal.OpenUnion4
  (Union, inj, prj, decomp, Member, MemberU2, (:>), weaken) where

-- The data constructors of Union are not exported

-- A sum data type, for `composing' effects
-- In GHC 7.4, we should make it a list
-- (:>) :: (* -> *) -> (* -> List) -> List
infixr 1 :>
data ((a :: * -> *) :> b)

-- Essentially, the nested Either data type
data Union r v where                      -- r is of a kind [*->*]
  UNow  :: Functor t => t v -> Union (t :> r) v
  UNext :: Union r v -> Union (any :> r) v

instance Functor (Union r) where
    {-# INLINE fmap #-}
    fmap f (UNow x)  = UNow (fmap f x)
    fmap f (UNext x) = UNext (fmap f x)

data P (n::Nat) = P

-- Convenient synonym
-- (also provides compatibility with other OpenUnion*.hs)
type Member t r = Member' t r (FindElem t r)

class Functor t => Member' t r (n :: Nat) where
  inj' :: P n -> t v -> Union r v
  prj' :: P n -> Union r v -> Maybe (t v)

instance (Functor t, r ~ (t :> r')) => Member' t r Z where
  inj' _ = UNow
  prj' _ (UNow x) = Just x
  prj' _ _        = Nothing

instance (r ~ (t' :> r'), Member' t r' n) => Member' t r (S n) where
  inj' _ = UNext . inj' (P::P n)
  prj' _ (UNow _)  = Nothing
  prj' _ (UNext x) = prj' (P::P n) x


inj :: forall t r v. (Functor t, Member t r) => t v -> Union r v
inj = inj' (P::P (FindElem t r))

prj :: forall t r v. (Functor t, Member t r) => Union r v -> Maybe (t v)
prj = prj' (P::P (FindElem t r))

{-# INLINE decomp #-}
decomp :: Union (t :> r) v -> Either (Union r v) (t v)
decomp (UNow x)  = Right x
decomp (UNext v) = Left v

weaken :: Functor t => Union r w -> Union (t :> r) w
weaken = UNext

data Nat = Z | S Nat

-- Find an index of an element in a `list'
-- The element must exist
-- This closed type family disambiguates otherwise overlapping
-- instances
type family FindElem (t :: * -> *) r :: Nat where
  FindElem t (t :> r)  = Z
  FindElem t (any :> r)  = S (FindElem t r)


type family EQU (a :: k) (b :: k) :: Bool where
  EQU a a = True
  EQU a b = False

-- This class is used for emulating monad transformers
class Member t r => MemberU2 (tag :: k -> * -> *) (t :: * -> *) r | tag r -> t
instance (MemberU' (EQU t1 t2) tag t1 (t2 :> r))
         => MemberU2 tag t1 (t2 :> r)

-- |
class Member t r =>
      MemberU' (f::Bool)
               (tag :: k -> * -> *)
               (t :: * -> *) r
                | tag r -> t

instance Functor (tag e) => MemberU' True tag (tag e) (tag e :> r)

instance (Member t (t' :> r), MemberU2 tag t r) =>
           MemberU' False tag t (t' :> r)
