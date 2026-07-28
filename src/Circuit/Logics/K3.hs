-- | K3 — Kleene three-valued logic.
--
-- Same three labels as 'Circuit.Logics.H3.H3' (true / false / unknown) and
-- the same lattice tables, but __not__ Heyting: there is no coherent
-- implication instance here. Partial-computation / undefinedness reading.
module Circuit.Logics.K3
  ( K3 (..),
  )
where

import Circuit.Logics.Lattice
  ( JoinSemiLattice (..),
    LowerBounded (..),
    MeetSemiLattice (..),
    UpperBounded (..),
  )

-- | Kleene three-valued logic (lattice only).
data K3
  = KFalse
  | KUnknown
  | KTrue
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

instance JoinSemiLattice K3 where
  KFalse \/ x = x
  x \/ KFalse = x
  KUnknown \/ KUnknown = KUnknown
  _ \/ _ = KTrue

instance MeetSemiLattice K3 where
  KTrue /\ x = x
  x /\ KTrue = x
  KUnknown /\ KUnknown = KUnknown
  _ /\ _ = KFalse

instance LowerBounded K3 where
  bottom = KFalse

instance UpperBounded K3 where
  top = KTrue
