-- | H3 — smallest Heyting algebra that is not Boolean.
--
-- Values: true / false / unknown. Implication is defined so Heyting laws
-- hold; excluded middle fails at @HUnknown@.
--
-- Epistemic reading: \"do we know enough to assert?\" — a natural output
-- alphabet for @Process obs H3@.
module Circuit.Logics.H3
  ( H3 (..),
  )
where

import Circuit.Logics.Boolean (Complemented (..))
import Circuit.Logics.Heyting (Heyting (..))
import Circuit.Logics.Lattice
  ( JoinSemiLattice (..),
    LowerBounded (..),
    MeetSemiLattice (..),
    UpperBounded (..),
  )

-- | Three-valued Heyting algebra (intuitionistic toy model).
data H3
  = -- | Known false / bottom.
    HFalse
  | -- | Open / undecided.
    HUnknown
  | -- | Known true / top.
    HTrue
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

instance JoinSemiLattice H3 where
  HFalse \/ x = x
  x \/ HFalse = x
  HUnknown \/ HUnknown = HUnknown
  _ \/ _ = HTrue

instance MeetSemiLattice H3 where
  HTrue /\ x = x
  x /\ HTrue = x
  HUnknown /\ HUnknown = HUnknown
  _ /\ _ = HFalse

instance LowerBounded H3 where
  bottom = HFalse

instance UpperBounded H3 where
  top = HTrue

instance Heyting H3 where
  _ ==> HTrue = HTrue
  HFalse ==> _ = HTrue
  HTrue ==> HFalse = HFalse
  HUnknown ==> HUnknown = HTrue
  HUnknown ==> HFalse = HFalse
  _ ==> _ = HUnknown

-- | Pseudo-complement @a ==> HFalse@. Not a Boolean complement:
-- @HUnknown \\/ complement HUnknown /= HTrue@.
instance Complemented H3 where
  complement a = a ==> HFalse
