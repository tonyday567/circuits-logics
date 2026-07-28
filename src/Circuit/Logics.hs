-- | Multi-valued and Heyting logics as circuit compile targets.
--
-- == Role
--
-- This package is __not__ substrate core. It sits in the same column as
-- @FinRel@ and @Process@: an alternative (or complementary) meaning for
-- boxes and wires — truth values, judgments, graded belief — that circuit
-- syntax can compile into.
--
-- == Hierarchy
--
-- @
--   Lattice  →  Heyting  →  Boolean
--                  │
--                  ├── Bool          (classical propositional structure)
--                  ├── H3            (finite non-Boolean Heyting)
--                  ├── Goedel r      (fuzzy \/ graded Heyting)
--                  └── (K3 is lattice-only — not Heyting)
-- @
--
-- == numhask guess
--
-- numhask already owns order lattices (@JoinSemiLattice@ \/ @MeetSemiLattice@).
-- A plausible path is:
--
-- 1. Keep this package as the logic laboratory and circuit payload home.
-- 2. If Heyting earns its keep, lift a thin @Heyting@ class into numhask
--    (or a @numhask-logic@ satellite) with the same laws.
-- 3. Instances: @Bool@, maybe @Goedel Double@; leave H3\/K3 here as
--    specialty carriers and oracles.
-- 4. @Boolean2Ring@ documents Boolean↔Ring; only migrate if numhask wants
--    that bridge next to its ring tower.
--
-- Until then: compile logic here; keep numeric core clean.
module Circuit.Logics
  ( -- * Lattice / Heyting / Boolean
    module Circuit.Logics.Lattice,
    module Circuit.Logics.Heyting,
    module Circuit.Logics.Boolean,

    -- * Carriers
    module Circuit.Logics.H3,
    module Circuit.Logics.K3,
    module Circuit.Logics.Goedel,
    module Circuit.Logics.Boolean2Ring,
    module Circuit.Logics.Prop,

    -- * Merge protocols
    module Circuit.Logics.Combine,

    -- * Streaming judges
    module Circuit.Logics.Process,

    -- * Oracles
    module Circuit.Logics.Laws,
  )
where

import Circuit.Logics.Boolean
import Circuit.Logics.Boolean2Ring
import Circuit.Logics.Combine
import Circuit.Logics.Goedel
import Circuit.Logics.H3
import Circuit.Logics.Heyting
import Circuit.Logics.K3
import Circuit.Logics.Lattice
import Circuit.Logics.Laws
import Circuit.Logics.Process
import Circuit.Logics.Prop
