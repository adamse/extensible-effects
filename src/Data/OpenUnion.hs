{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}


-- | Original work at <http://okmij.org/ftp/Haskell/extensible/OpenUnion1.hs>
-- and <http://okmij.org/ftp/Haskell/extensible/OpenUnion2.hs>.
-- Open unions (type-indexed co-products) for extensible effects.
module Data.OpenUnion(
  module Data.OpenUnion.Internal.OpenUnion4
  , prjForce
  , SetMember
  ) where

import Data.OpenUnion.Internal.OpenUnion4

type SetMember tag t r = MemberU2 tag t r

prjForce :: forall t r v. (Functor t, Member t r) => Union r v -> t v
prjForce union = case prj union of
                   Just tv -> tv
                   Nothing -> error "prjForce with an invalid type"
