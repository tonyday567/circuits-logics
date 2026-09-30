-- | Judgment processes: streaming machines that emit multi-valued logic.
--
-- @Moore obs H3@ is the flagship shape — Moore machines whose timeline
-- is a sequence of epistemic verdicts. This module is a compile target for
-- agent/sensor/parser-style \"are we decided yet?\" stories on top of
-- @Circuit.GMachine@.
module Circuit.Logics.Process
  ( -- * Threshold judges
    voteH3,
    voteLatchH3,

    -- * Combinators on H3 streams
    consensusProc,
    latchProc,

    -- * Gödel degree process
    ewmaGoedel,
  )
where

import Circuit.GMachine (Cell (..), GMoore (..), GMooreT (..), Moore, moore)
import Circuit.Logics.Combine (consensusH3, latchH3)
import Circuit.Logics.Goedel (Goedel (..), mkGoedel)
import Circuit.Logics.H3 (H3 (..))

-- | Majority-style judge on @Bool@ votes.
--
-- Emits @HTrue@ / @HFalse@ once one side reaches threshold @k@ and leads;
-- otherwise @HUnknown@. Verdicts are revisable if the other side catches up
-- (no latch).
voteH3 :: Int -> Moore Bool H3
voteH3 k = moore inject step extract
  where
    inject b = step (0, 0) b
    step (y, n) True = (y + 1, n)
    step (y, n) False = (y, n + 1)
    extract (y, n)
      | y >= k && y > n = HTrue
      | n >= k && n > y = HFalse
      | otherwise = HUnknown

-- | Like 'voteH3' but freezes the first decided verdict.
voteLatchH3 :: Int -> Moore Bool H3
voteLatchH3 k = moore inject step extract
  where
    inject b =
      case extractOpen (stepOpen (0, 0) b) of
        HUnknown -> Left (stepOpen (0, 0) b)
        v -> Right v
    step (Left yn) b =
      case extractOpen (stepOpen yn b) of
        HUnknown -> Left (stepOpen yn b)
        v -> Right v
    step (Right v) _ = Right v
    extract (Left yn) = extractOpen yn
    extract (Right v) = v

    stepOpen (y, n) True = (y + 1, n)
    stepOpen (y, n) False = (y, n + 1)
    extractOpen (y, n)
      | y >= k && y > n = HTrue
      | n >= k && n > y = HFalse
      | otherwise = HUnknown

-- | Pointwise consensus of two H3 streams (same observation).
--
-- State is a pair of sub-states; extract is 'consensusH3'.
consensusProc :: Moore a H3 -> Moore a H3 -> Moore a H3
consensusProc (GMooreT (GMoore i1 (Cell k1 o1))) (GMooreT (GMoore i2 (Cell k2 o2))) =
  moore
    (\a -> (i1 a, i2 a))
    (\(s1, s2) a -> (k1 (s1, a), k2 (s2, a)))
    (\(s1, s2) -> consensusH3 (o1 s1) (o2 s2))

-- | Running latch over an H3-producing process.
latchProc :: Moore a H3 -> Moore a H3
latchProc (GMooreT (GMoore i (Cell k o))) =
  moore
    (\a -> let s0 = i a in (s0, o s0))
    ( \(s, v) a ->
        let s' = k (s, a)
            v' = latchH3 v (o s')
         in (s', v')
    )
    snd

-- | Exponentially weighted moving average of @[0,1]@ samples as Gödel degrees.
--
-- @ewmaGoedel alpha@ uses smoothing factor @alpha ∈ (0,1]@.
ewmaGoedel :: Double -> Moore Double (Goedel Double)
ewmaGoedel alpha = moore inject step extract
  where
    inject x = clamp x
    step s x = (1 - alpha) * s + alpha * clamp x
    extract = mkGoedel
    clamp x = max 0 (min 1 x)
