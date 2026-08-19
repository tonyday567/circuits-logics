-- | Gödel–Dummett fuzzy logic on an ordered unit interval.
--
-- Meet/join are min/max; implication is the standard Gödel rule.
-- Heyting but not Boolean: excluded middle fails for intermediate degrees.
module Circuit.Logics.Goedel
  ( Goedel (..),
    mkGoedel,
    unGoedel,
    godei,
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
import Data.Ratio (Rational)

-- | Gödel truth degree, intended in @[0,1]@.
--
-- The constructor is strict about bounds via 'mkGoedel'; the newtype is
-- still exposed for zero-cost unwrapping when the invariant is trusted.
newtype Goedel r = Goedel r
  deriving (Eq, Ord, Show, Read)

-- | Clamp to the unit interval and wrap.
mkGoedel :: (Ord r, Num r) => r -> Goedel r
mkGoedel r = Goedel (max 0 (min 1 r))

unGoedel :: Goedel r -> r
unGoedel (Goedel r) = r

-- | Alias matching common spelling in the literature.
godei :: Rational -> Goedel Rational
godei = mkGoedel

instance (Ord r) => JoinSemiLattice (Goedel r) where
  Goedel a \/ Goedel b = Goedel (max a b)

instance (Ord r) => MeetSemiLattice (Goedel r) where
  Goedel a /\ Goedel b = Goedel (min a b)

instance (Ord r, Num r) => LowerBounded (Goedel r) where
  bottom = Goedel 0

instance (Ord r, Num r) => UpperBounded (Goedel r) where
  top = Goedel 1

instance (Ord r, Num r) => Heyting (Goedel r) where
  Goedel a ==> Goedel b
    | a <= b = Goedel 1
    | otherwise = Goedel b

-- | Gödel negation as @a ==> bottom@ (sharp: only 0 maps to 1).
instance (Ord r, Num r) => Complemented (Goedel r) where
  complement a = a ==> bottom
