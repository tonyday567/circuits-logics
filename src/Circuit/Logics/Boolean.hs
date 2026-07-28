-- | Boolean algebras: Heyting + complements (classical logic structure).
module Circuit.Logics.Boolean
  ( Complemented (..),
    Boolean,
  )
where

import Circuit.Logics.Heyting (Heyting)
import Circuit.Logics.Lattice (BoundedLattice, LowerBounded (..), UpperBounded (..))

-- | Lattice complement (negation).
class (BoundedLattice a) => Complemented a where
  complement :: a -> a

instance Complemented Bool where
  complement = not

-- | Boolean algebra: Heyting with complements satisfying excluded middle
-- and non-contradiction (see "Circuit.Logics.Laws").
type Boolean a = (Heyting a, Complemented a)
