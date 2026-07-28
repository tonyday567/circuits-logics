-- | Classical propositional formulas and evaluation.
--
-- The formula AST is the \"syntax\"; @Bool@ (and other Boolean carriers)
-- are compile targets for @evalProp@.
module Circuit.Logics.Prop
  ( Prop (..),
    evalProp,
    evalPropH3,
    simplify,
  )
where

import Circuit.Logics.Boolean (Complemented (..))
import Circuit.Logics.H3 (H3 (..))
import Circuit.Logics.Heyting (Heyting (..))
import Circuit.Logics.Lattice
  ( JoinSemiLattice (..),
    LowerBounded (..),
    MeetSemiLattice (..),
    UpperBounded (..),
    (/\),
    (\/),
  )

-- | Propositional formula over variable names @v@.
data Prop v
  = -- | Atomic variable.
    Var v
  | -- | Falsehood.
    Bot
  | -- | Truth.
    Top
  | -- | Negation.
    Not (Prop v)
  | -- | Conjunction.
    And (Prop v) (Prop v)
  | -- | Disjunction.
    Or (Prop v) (Prop v)
  | -- | Implication.
    Imp (Prop v) (Prop v)
  deriving (Eq, Ord, Show, Read, Functor)

-- | Evaluate a formula given a valuation into a complemented Heyting algebra.
evalProp ::
  (Heyting b, Complemented b) =>
  (v -> b) ->
  Prop v ->
  b
evalProp env = go
  where
    go (Var v) = env v
    go Bot = bottom
    go Top = top
    go (Not p) = complement (go p)
    go (And p q) = go p /\ go q
    go (Or p q) = go p \/ go q
    go (Imp p q) = go p ==> go q

-- | Three-valued eval: atoms may be unknown; connectives use H3 tables.
--
-- @Not@ on H3 uses the Heyting-style pseudo-complement @a ==> HFalse@.
evalPropH3 :: (v -> H3) -> Prop v -> H3
evalPropH3 env = go
  where
    go (Var v) = env v
    go Bot = HFalse
    go Top = HTrue
    go (Not p) = go p ==> HFalse
    go (And p q) = go p /\ go q
    go (Or p q) = go p \/ go q
    go (Imp p q) = go p ==> go q

-- | Cheap structural simplify (constants only).
simplify :: Prop v -> Prop v
simplify = go
  where
    go (Not p) = case go p of
      Bot -> Top
      Top -> Bot
      Not q -> q
      p' -> Not p'
    go (And p q) = case (go p, go q) of
      (Bot, _) -> Bot
      (_, Bot) -> Bot
      (Top, q') -> q'
      (p', Top) -> p'
      (p', q') -> And p' q'
    go (Or p q) = case (go p, go q) of
      (Top, _) -> Top
      (_, Top) -> Top
      (Bot, q') -> q'
      (p', Bot) -> p'
      (p', q') -> Or p' q'
    go (Imp p q) = case (go p, go q) of
      (Bot, _) -> Top
      (_, Top) -> Top
      (Top, q') -> q'
      (p', Bot) -> Not p'
      (p', q') -> Imp p' q'
    go p = p
