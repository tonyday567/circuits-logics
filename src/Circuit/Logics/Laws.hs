-- | Named law functions for lattice / Heyting / Boolean carriers.
--
-- These are the value-level oracles: run them on concrete models so claims
-- about \"Boolean\", \"Heyting\", or \"just a lattice\" can go red.
module Circuit.Logics.Laws
  ( -- * Lattice
    law_join_assoc,
    law_join_comm,
    law_join_idem,
    law_meet_assoc,
    law_meet_comm,
    law_meet_idem,
    law_absorption_join,
    law_absorption_meet,

    -- * Heyting
    law_heyting_refl,
    law_heyting_mp_left,
    law_heyting_mp_right,
    law_heyting_distr,

    -- * Boolean
    law_excluded_middle,
    law_noncontradiction,
    law_double_negation,

    -- * Suites
    latticeSuite,
    heytingSuite,
    booleanSuite,
  )
where

import Circuit.Logics.Boolean (Complemented (..))
import Circuit.Logics.Heyting (Heyting (..))
import Circuit.Logics.Lattice
  ( JoinSemiLattice (..),
    LowerBounded (..),
    MeetSemiLattice (..),
    UpperBounded (..),
    (/\),
    (\/),
  )

law_join_assoc :: (JoinSemiLattice a) => a -> a -> a -> Bool
law_join_assoc a b c = a \/ (b \/ c) == (a \/ b) \/ c

law_join_comm :: (JoinSemiLattice a) => a -> a -> Bool
law_join_comm a b = a \/ b == b \/ a

law_join_idem :: (JoinSemiLattice a) => a -> Bool
law_join_idem a = a \/ a == a

law_meet_assoc :: (MeetSemiLattice a) => a -> a -> a -> Bool
law_meet_assoc a b c = a /\ (b /\ c) == (a /\ b) /\ c

law_meet_comm :: (MeetSemiLattice a) => a -> a -> Bool
law_meet_comm a b = a /\ b == b /\ a

law_meet_idem :: (MeetSemiLattice a) => a -> Bool
law_meet_idem a = a /\ a == a

law_absorption_join :: (JoinSemiLattice a, MeetSemiLattice a) => a -> a -> Bool
law_absorption_join a b = a \/ (a /\ b) == a

law_absorption_meet :: (JoinSemiLattice a, MeetSemiLattice a) => a -> a -> Bool
law_absorption_meet a b = a /\ (a \/ b) == a

law_heyting_refl :: (Eq a, Heyting a) => a -> Bool
law_heyting_refl a = (a ==> a) == top

law_heyting_mp_left :: (Eq a, Heyting a) => a -> a -> Bool
law_heyting_mp_left a b = (a /\ (a ==> b)) == (a /\ b)

law_heyting_mp_right :: (Eq a, Heyting a) => a -> a -> Bool
law_heyting_mp_right a b = (b /\ (a ==> b)) == b

law_heyting_distr :: (Eq a, Heyting a) => a -> a -> a -> Bool
law_heyting_distr a b c = (a ==> (b /\ c)) == ((a ==> b) /\ (a ==> c))

law_excluded_middle :: (Eq a, JoinSemiLattice a, Complemented a, UpperBounded a) => a -> Bool
law_excluded_middle a = (a \/ complement a) == top

law_noncontradiction :: (Eq a, MeetSemiLattice a, Complemented a, LowerBounded a) => a -> Bool
law_noncontradiction a = (a /\ complement a) == bottom

law_double_negation :: (Eq a, Complemented a) => a -> Bool
law_double_negation a = complement (complement a) == a

-- | All lattice laws on a triple of samples.
latticeSuite :: (JoinSemiLattice a, MeetSemiLattice a) => a -> a -> a -> Bool
latticeSuite a b c =
  and
    [ law_join_assoc a b c,
      law_join_comm a b,
      law_join_idem a,
      law_meet_assoc a b c,
      law_meet_comm a b,
      law_meet_idem a,
      law_absorption_join a b,
      law_absorption_meet a b
    ]

heytingSuite :: (Heyting a) => a -> a -> a -> Bool
heytingSuite a b c =
  latticeSuite a b c
    && and
      [ law_heyting_refl a,
        law_heyting_mp_left a b,
        law_heyting_mp_right a b,
        law_heyting_distr a b c
      ]

booleanSuite :: (Heyting a, Complemented a) => a -> a -> a -> Bool
booleanSuite a b c =
  heytingSuite a b c
    && and
      [ law_excluded_middle a,
        law_noncontradiction a,
        law_double_negation a
      ]
