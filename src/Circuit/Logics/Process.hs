-- | Judgment processes: streaming machines that emit multi-valued logic.
--
-- @Moore obs H3@ is the flagship shape — Moore machines whose timeline
-- is a sequence of epistemic verdicts. This module is a compile target for
-- agent/sensor/parser-style \"are we decided yet?\" stories on top of
-- @Circuit.Machine@.
module Circuit.Logics.Process
  ( -- * Threshold judges
    voteH3,
    voteLatchH3,

    -- * Combinators on H3 streams
    latchProc,

    -- * Gödel degree process
    ewmaGoedel,
  )
where

import Circuit.Logics.Combine (latchH3)
import Circuit.Logics.Goedel (Goedel (..), mkGoedel)
import Circuit.Logics.H3 (H3 (..))
import Circuit.Machine (Body (..), MooreT (..), moore)
import Circuit.Tensor (unConst)

-- | Majority-style judge on @Bool@ votes.
--
-- Emits @HTrue@ / @HFalse@ once one side reaches threshold @k@ and leads;
-- otherwise @HUnknown@. Verdicts are revisable if the other side catches up
-- (no latch).
voteH3 :: Int -> MooreT (,) (->) Bool H3
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
voteLatchH3 :: Int -> MooreT (,) (->) Bool H3
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

-- | Running latch over an H3-producing process.
latchProc :: MooreT (,) (->) a H3 -> MooreT (,) (->) a H3
latchProc (MooreT (Body i k o)) =
  moore
    (\a -> let s0 = i a in (s0, unConst (o s0)))
    ( \(s, v) a ->
        let s' = k (s, a)
            v' = latchH3 v (unConst (o s'))
         in (s', v')
    )
    snd

-- | Exponentially weighted moving average of @[0,1]@ samples as Gödel degrees.
--
-- @ewmaGoedel alpha@ uses smoothing factor @alpha ∈ (0,1]@.
ewmaGoedel :: Double -> MooreT (,) (->) Double (Goedel Double)
ewmaGoedel alpha = moore inject step extract
  where
    inject x = clamp x
    step s x = (1 - alpha) * s + alpha * clamp x
    extract = mkGoedel
    clamp x = max 0 (min 1 x)
