-- | Order lattices for truth-value carriers.
--
-- Naming follows numhask's lattice vocabulary (@\/@, @/\\@, bounds) so a
-- future alignment is a thin instance layer, not a redesign. This package
-- owns its hierarchy so it stays an optional compile target.
module Circuit.Logics.Lattice
  ( JoinSemiLattice (..),
    MeetSemiLattice (..),
    Lattice,
    LowerBounded (..),
    UpperBounded (..),
    BoundedLattice,
  )
where

-- | Join-semilattice (disjunction / least upper bound).
--
-- > Associativity: x \/ (y \/ z) == (x \/ y) \/ z
-- > Commutativity: x \/ y == y \/ x
-- > Idempotency:   x \/ x == x
class (Eq a) => JoinSemiLattice a where
  (\/) :: a -> a -> a

infixr 5 \/

-- | Meet-semilattice (conjunction / greatest lower bound).
--
-- > Associativity: x /\ (y /\ z) == (x /\ y) /\ z
-- > Commutativity: x /\ y == y /\ x
-- > Idempotency:   x /\ x == x
class (Eq a) => MeetSemiLattice a where
  (/\) :: a -> a -> a

infixr 6 /\

-- | Lattice when absorption holds:
--
-- > a \/ (a /\ b) == a
-- > a /\ (a \/ b) == a
type Lattice a = (JoinSemiLattice a, MeetSemiLattice a)

class (JoinSemiLattice a) => LowerBounded a where
  bottom :: a

class (MeetSemiLattice a) => UpperBounded a where
  top :: a

type BoundedLattice a =
  (JoinSemiLattice a, MeetSemiLattice a, LowerBounded a, UpperBounded a)

instance JoinSemiLattice Bool where
  (\/) = (||)

instance MeetSemiLattice Bool where
  (/\) = (&&)

instance LowerBounded Bool where
  bottom = False

instance UpperBounded Bool where
  top = True
