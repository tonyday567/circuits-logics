-- | Ways to merge parallel judgments (beyond raw lattice meet/join).
--
-- Lattice ops are available via "Circuit.Logics.Lattice". Protocol-style
-- combinators live here — especially useful for multi-judge @Process@ wiring.
module Circuit.Logics.Combine
  ( consensusH3,
    consensusK3,
    firstKnownH3,
    latchH3,
  )
where

import Circuit.Logics.H3 (H3 (..))
import Circuit.Logics.K3 (K3 (..))

-- | Agree only when both sides are the same decided value; else unknown.
--
-- Not lattice meet: @consensusH3 HTrue HFalse = HUnknown@, whereas
-- meet would be @HFalse@.
consensusH3 :: H3 -> H3 -> H3
consensusH3 HTrue HTrue = HTrue
consensusH3 HFalse HFalse = HFalse
consensusH3 _ _ = HUnknown

consensusK3 :: K3 -> K3 -> K3
consensusK3 KTrue KTrue = KTrue
consensusK3 KFalse KFalse = KFalse
consensusK3 _ _ = KUnknown

-- | Prefer the first decided value; unknown only if both open.
firstKnownH3 :: H3 -> H3 -> H3
firstKnownH3 HUnknown y = y
firstKnownH3 x _ = x

-- | Once decided, stay decided (left-biased latch over a stream step).
latchH3 :: H3 -> H3 -> H3
latchH3 HUnknown y = y
latchH3 x _ = x
