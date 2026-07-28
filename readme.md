# circuits-logics

Multi-valued and Heyting logics as **circuit compile targets** — value-level
truth systems that sit in the same column as `FinRel` and `Process`, not in
the substrate core package list.

```bash
cd ~/haskell/circuits-logics
cabal run circuits-logics-verify
```

## What this is

| layer | contents |
|---|---|
| hierarchy | `Lattice` → `Heyting` → `Boolean` (+ `Complemented`) |
| classical | `Bool`, propositional AST `Prop v` with `evalProp` |
| multi-valued | `H3` (Heyting), `K3` (lattice only), `Goedel r` (fuzzy) |
| bridge | `Boolean2Ring` (Boolean as char-2 ring) |
| streaming | `Process obs H3` judges: `voteH3`, latch, consensus, `ewmaGoedel` |
| oracles | named laws in `Circuit.Logics.Laws` that can go red |

## Role relative to circuits

```text
  syntax (Free / Loop / Net / Algebra)
              │
              ▼ compile / interpret
     ┌────────┼────────┬─────────────┐
     ▼        ▼        ▼             ▼
   (->)   Process   FinRel    logics payloads
                              (H3, Goedel, …)
```

Logics are **payloads and judgment alphabets**. Wiring structure still comes
from circuits; whether a wire carries `Double` or `H3` is this package.

## numhask guess

numhask already has order lattices. A plausible future:

1. **Now** — experiment here; keep numeric core clean.
2. **If Heyting consolidates** — lift a thin class into numhask (or
   `numhask-logic`) with the same laws; instances for `Bool` and maybe
   unit-interval degrees.
3. **Leave specialty carriers** (`H3`, `K3`, process judges) in
   `circuits-logics` as the circuit-facing laboratory.
4. **`Boolean2Ring`** migrates only if the ring tower wants an explicit
   Boolean bridge.

Lattice operator names (`\/`, `/\`) match numhask on purpose.

## Origins

Carrier tables and Heyting/Boolean law shapes were mined from SubHask's logic
module; this package is a clean-room hierarchy for circuits use, not a SubHask
dependency.
