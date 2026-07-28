-- | Heyting algebras: bounded lattices with implication.
--
-- A Heyting algebra need not be Boolean: excluded middle may fail.
-- Classical @Bool@ is Heyting (via material implication); @H3@ and
-- @Goedel@ are the standard non-Boolean examples in this package.
module Circuit.Logics.Heyting
  ( Heyting (..),
  )
where

import Circuit.Logics.Lattice (BoundedLattice, MeetSemiLattice (..), UpperBounded (..), (/\))

-- | Bounded lattice with relative pseudo-complement (@==>@).
--
-- Characteristic properties (see "Circuit.Logics.Laws"):
--
-- > a ==> a           == top
-- > a /\ (a ==> b)    == a /\ b
-- > b /\ (a ==> b)    == b
-- > a ==> (b /\ c)    == (a ==> b) /\ (a ==> c)
class (BoundedLattice a) => Heyting a where
  -- | Implication (relative pseudo-complement).
  (==>) :: a -> a -> a

infixr 1 ==>

instance Heyting Bool where
  -- Material implication: classical.
  a ==> b = not a || b
