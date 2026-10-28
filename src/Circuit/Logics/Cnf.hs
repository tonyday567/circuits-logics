-- | Bounded-width conjunctive normal form, canonical encoding, and exact
-- model counting.
--
-- The finite-set semantics and encoding follow the comparator boundary of
-- OpenAI's @SubexponentialSAT.lean@. 'countModels' is an executable reference
-- by assignment enumeration; it does not reproduce the comparator machine's
-- asymptotic time or space bounds.
module Circuit.Logics.Cnf
  ( Literal (..),
    Clause (..),
    Cnf (..),
    CnfError (..),
    normalize,
    validate,
    widthAtMost,
    countModels,
    satisfiable,
    encodeCnf,
    countWord,
    wordToNat,
  )
where

import Control.Monad (replicateM)
import Data.Functor (($>))
import Data.List (nub, sort)
import Numeric.Natural (Natural)

-- | A zero-based variable index and its required Boolean value.
data Literal = Literal Int Bool
  deriving (Eq, Ord, Show)

-- | A disjunction. The constructor admits list input; 'normalize' supplies
-- finite-set semantics.
newtype Clause = Clause [Literal]
  deriving (Eq, Ord, Show)

-- | A CNF together with its declared number of variables. Declared variables
-- need not occur in a clause.
data Cnf = Cnf
  { variableCount :: Int,
    clauses :: [Clause]
  }
  deriving (Eq, Ord, Show)

-- | A formula outside the finite comparator domain.
data CnfError
  = NegativeVariableCount Int
  | VariableOutOfBounds Int Int
  deriving (Eq, Show)

-- | Sort and deduplicate literals and clauses. Opposite signs of one variable
-- remain distinct.
normalize :: Cnf -> Cnf
normalize (Cnf n cs) = Cnf n (sort (nub (map normalizeClause cs)))
  where
    normalizeClause (Clause ls) = Clause (sort (nub ls))

-- | Check the declared variable count and every literal index.
validate :: Cnf -> Either CnfError ()
validate (Cnf n cs)
  | n < 0 = Left (NegativeVariableCount n)
  | otherwise = case badIndices of
      [] -> Right ()
      i : _ -> Left (VariableOutOfBounds n i)
  where
    badIndices =
      [ i
      | Clause ls <- cs,
        Literal i _ <- ls,
        i < 0 || i >= n
      ]

-- | Whether every clause has at most @k@ literals after duplicate removal.
--
-- >>> widthAtMost 1 (Cnf 1 [Clause []])
-- True
-- >>> widthAtMost 1 (Cnf 1 [Clause [Literal 0 True, Literal 0 False]])
-- False
widthAtMost :: Int -> Cnf -> Bool
widthAtMost k (Cnf _ cs) =
  k >= 0 && all (\(Clause ls) -> length (nub ls) <= k) cs

-- | Count every satisfying assignment of the declared variables.
--
-- >>> countModels (Cnf 0 [])
-- Right 1
-- >>> countModels (Cnf 0 [Clause []])
-- Right 0
-- >>> countModels (Cnf 2 [Clause [Literal 0 True]])
-- Right 2
-- >>> countModels (Cnf 1 [Clause [Literal 0 True, Literal 0 False]])
-- Right 2
-- >>> countModels (Cnf 1 [Clause [Literal 1 True]])
-- Left (VariableOutOfBounds 1 1)
countModels :: Cnf -> Either CnfError Natural
countModels f@(Cnf n _) = validate f $> countNormalized n (normalize f)

countNormalized :: Int -> Cnf -> Natural
countNormalized n f =
  fromIntegral (length [() | a <- replicateM n [False, True], evalCnf a f])

evalCnf :: [Bool] -> Cnf -> Bool
evalCnf a (Cnf _ cs) = all (evalClause a) cs

evalClause :: [Bool] -> Clause -> Bool
evalClause a (Clause ls) = any (evalLiteral a) ls

evalLiteral :: [Bool] -> Literal -> Bool
evalLiteral a (Literal i positive) = (a !! i) == positive

-- | Whether at least one satisfying assignment exists.
--
-- >>> satisfiable (Cnf 1 [])
-- Right True
-- >>> satisfiable (Cnf 1 [Clause [Literal 0 True], Clause [Literal 0 False]])
-- Right False
satisfiable :: Cnf -> Either CnfError Bool
satisfiable = fmap (> 0) . countModels

-- | Encode a valid CNF using unary naturals, framed lists, sorted literal
-- codes, and lexicographically sorted clause code lists.
--
-- >>> fmap (map (\b -> if b then '1' else '0')) (encodeCnf (Cnf 0 []))
-- Right "00"
-- >>> fmap (map (\b -> if b then '1' else '0')) (encodeCnf (Cnf 0 [Clause []]))
-- Right "011000"
-- >>> fmap (map (\b -> if b then '1' else '0')) (encodeCnf (Cnf 1 []))
-- Right "100"
encodeCnf :: Cnf -> Either CnfError [Bool]
encodeCnf f@(Cnf n _) =
  validate f $> encodeNat n ++ encodeList (encodeList encodeNat) (cnfNumbers (normalize f))

encodeNat :: Int -> [Bool]
encodeNat n = replicate n True ++ [False]

frame :: [Bool] -> [Bool]
frame = foldr (\b rest -> True : b : rest) [False]

encodeList :: (a -> [Bool]) -> [a] -> [Bool]
encodeList _ [] = [False]
encodeList enc (a : as) = True : (frame (enc a) ++ encodeList enc as)

literalCode :: Literal -> Int
literalCode (Literal i positive) = 2 * i + if positive then 1 else 0

clauseNumbers :: Clause -> [Int]
clauseNumbers (Clause ls) = sort (map literalCode ls)

cnfNumbers :: Cnf -> [[Int]]
cnfNumbers = sort . map clauseNumbers . clauses

-- | Encode a nonnegative count as zero or little-endian binary digits.
--
-- >>> countWord 0
-- [False]
-- >>> countWord 1
-- [True]
-- >>> countWord 2
-- [False,True]
-- >>> countWord 4
-- [False,False,True]
countWord :: Natural -> [Bool]
countWord 0 = [False]
countWord n =
  let (q, r) = n `divMod` 2
   in (r == 1) : if q == 0 then [] else countWord q

-- | Decode little-endian binary digits.
--
-- >>> wordToNat [False,True]
-- 2
-- >>> wordToNat (countWord 8)
-- 8
wordToNat :: [Bool] -> Natural
wordToNat = foldr (\b n -> (if b then 1 else 0) + 2 * n) 0
