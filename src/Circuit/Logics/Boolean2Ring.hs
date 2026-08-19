-- | Boolean algebra re-read as a ring of characteristic 2.
--
-- > (+)  = XOR   (symmetric difference)
-- > (*)  = AND
-- > zero = false
-- > one  = true
-- > negate = id
--
-- Keeps Boolean and ring APIs separate while documenting the bridge —
-- relevant if numhask ever wants an explicit Boolean↔Ring story.
module Circuit.Logics.Boolean2Ring
  ( Boolean2Ring (..),
    xorBool,
  )
where

import Circuit.Logics.Boolean (Complemented (..))
import Circuit.Logics.Lattice (LowerBounded (..), MeetSemiLattice (..), UpperBounded (..), (/\))

-- | Carrier wrapper: same Boolean values, ring operations.
newtype Boolean2Ring b = Boolean2Ring {getBoolean2Ring :: b}
  deriving (Eq, Ord, Show, Read)

-- | XOR on a complemented meet-semilattice with bounds.
xorBool :: (Complemented b) => b -> b -> b
xorBool a b =
  let either' = (a /\ complement b) `joinLike` (complement a /\ b)
   in either'
  where
    -- local join via De Morgan when we only have meet+complement+bounds
    joinLike x y = complement (complement x /\ complement y)

instance (MeetSemiLattice b, Complemented b, LowerBounded b, UpperBounded b) => Num (Boolean2Ring b) where
  Boolean2Ring a + Boolean2Ring b = Boolean2Ring (xorBool a b)
  Boolean2Ring a * Boolean2Ring b = Boolean2Ring (a /\ b)
  negate = id
  abs = id
  signum (Boolean2Ring b) = Boolean2Ring b
  fromInteger n
    | n == 0 = Boolean2Ring bottom
    | otherwise = Boolean2Ring top
