-- | Exact finite oracles for CNF semantics and canonical encoding.
module Axioma.Cnf (cnfChecks) where

import Circuit.Logics.Cnf
import Control.Monad (replicateM)
import Data.List (subsequences)
import Numeric.Natural (Natural)

cnfChecks :: [(String, Bool)]
cnfChecks =
  [ ("CNF population sizes", map (length . allCnfs) [0, 1, 2] == [2, 16, 65536]),
    ("CNF empty n=0 encoding and count", emptyZero),
    ("CNF empty clause encoding and count", emptyClause),
    ("CNF empty n=1 output", emptyOne),
    ("CNF unit encoding lengths", unitLengths),
    ("CNF unused variable", countModels (Cnf 2 [Clause [Literal 0 True]]) == Right 2),
    ("CNF tautological clause", countModels (Cnf 1 [Clause [Literal 0 True, Literal 0 False]]) == Right 2),
    ( "CNF contradictory units",
      countModels (Cnf 1 [Clause [Literal 0 True], Clause [Literal 0 False]]) == Right 0
    ),
    ("CNF sign flip changes count", signFlip),
    ("CNF exact count words", exactWords),
    ("CNF semantic equality does not change encoding", encodingNotSemantic),
    ("CNF finite-set canonicalisation", canonicalInvariance),
    ("CNF width is at most k", widthAtMost 1 (Cnf 1 [Clause [], Clause [Literal 0 True]])),
    ("CNF invalid negative variable count", validate (Cnf (-1) []) == Left (NegativeVariableCount (-1))),
    ( "CNF invalid variable index",
      validate (Cnf 1 [Clause [Literal 1 False]]) == Left (VariableOutOfBounds 1 1)
    ),
    ("CNF exhaustive n=0", all finiteCase (allCnfs 0)),
    ("CNF exhaustive n=1", all finiteCase (allCnfs 1)),
    ("CNF exhaustive n=2", all finiteCase (allCnfs 2))
  ]

emptyZero :: Bool
emptyZero =
  fmap bitString (encodeCnf f) == Right "00"
    && fmap length (encodeCnf f) == Right 2
    && countModels f == Right 1
    && fmap countWord (countModels f) == Right [True]
    && satisfiable f == Right True
  where
    f = Cnf 0 []

emptyClause :: Bool
emptyClause =
  fmap bitString (encodeCnf f) == Right "011000"
    && fmap length (encodeCnf f) == Right 6
    && countModels f == Right 0
    && fmap countWord (countModels f) == Right [False]
    && satisfiable f == Right False
  where
    f = Cnf 0 [Clause []]

emptyOne :: Bool
emptyOne =
  fmap bitString (encodeCnf f) == Right "100"
    && fmap length (encodeCnf f) == Right 3
    && countModels f == Right 2
    && fmap countWord (countModels f) == Right [False, True]
    && satisfiable f == Right True
  where
    f = Cnf 1 []

unitLengths :: Bool
unitLengths =
  fmap length (encodeCnf (Cnf 1 [Clause [Literal 0 True]])) == Right 19
    && fmap length (encodeCnf (Cnf 1 [Clause [Literal 0 False]])) == Right 15

signFlip :: Bool
signFlip =
  countModels (Cnf 2 [Clause [Literal 0 True], Clause [Literal 0 False, Literal 1 True]]) == Right 1
    && countModels (Cnf 2 [Clause [Literal 0 False], Clause [Literal 0 False, Literal 1 True]]) == Right 2

exactWords :: Bool
exactWords =
  fmap countWord (countModels (Cnf 2 [])) == Right [False, False, True]
    && fmap countWord (countModels (Cnf 3 [])) == Right [False, False, False, True]

encodingNotSemantic :: Bool
encodingNotSemantic =
  countModels f == Right 2
    && countModels g == Right 2
    && encodeCnf f /= encodeCnf g
    && fmap length (encodeCnf f) == Right 20
    && fmap length (encodeCnf g) == Right 56
  where
    f = Cnf 2 [Clause [Literal 0 True]]
    g = Cnf 2 [Clause [Literal 0 True], Clause [Literal 0 True, Literal 1 True]]

canonicalInvariance :: Bool
canonicalInvariance =
  encodeCnf f == encodeCnf g && countModels f == countModels g
  where
    f = Cnf 2 [Clause [Literal 1 True, Literal 0 False], Clause [Literal 0 True]]
    g =
      Cnf
        2
        [ Clause [Literal 0 True],
          Clause [Literal 0 False, Literal 1 True, Literal 1 True],
          Clause [Literal 0 True]
        ]

finiteCase :: Cnf -> Bool
finiteCase f@(Cnf n _) = case (countModels f, satisfiable f, encodeCnf f) of
  (Right count, Right sat, Right encoded) ->
    count == referenceCount f
      && sat == (referenceCount f > 0)
      && length encoded == referenceLength f
      && wordToNat (countWord count) == count
      && length (countWord count) <= n + 1
  _ -> False

allCnfs :: Int -> [Cnf]
allCnfs n = map (Cnf n) (subsequences allClauses)
  where
    allClauses = map Clause (subsequences [Literal i p | i <- [0 .. n - 1], p <- [False, True]])

referenceCount :: Cnf -> Natural
referenceCount (Cnf n cs) =
  fromIntegral (length [() | a <- replicateM n [False, True], all (any (refLiteral a) . (\(Clause ls) -> ls)) cs])

refLiteral :: [Bool] -> Literal -> Bool
refLiteral a (Literal i p) = (a !! i) == p

referenceLength :: Cnf -> Int
referenceLength (Cnf n cs) =
  n + 2 + sum [4 + 8 * length ls + 4 * sum (map refCode ls) | Clause ls <- cs]

refCode :: Literal -> Int
refCode (Literal i p) = 2 * i + if p then 1 else 0

bitString :: [Bool] -> String
bitString = map (\b -> if b then '1' else '0')
