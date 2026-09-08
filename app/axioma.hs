-- | Value-level oracles for manyvalued carriers and processes.
module Main where

import Circuit.Logics
import Circuit.Process (scan)
import Data.Ratio ((%))

check :: String -> Bool -> IO Bool
check name ok = do
  putStrLn $ (if ok then "PASS " else "FAIL ") ++ name
  pure ok

main :: IO ()
main = do
  putStrLn "circuits-logics-axioma"
  results <-
    sequence
      [ -- Bool is Boolean
        check "Bool booleanSuite" $
          all3 booleanSuite [False, True],
        -- H3 is Heyting, not Boolean
        check "H3 heytingSuite" $
          all3 heytingSuite universeH3,
        check "H3 fails excluded middle at Unknown" $
          not (law_excluded_middle HUnknown),
        -- K3 is lattice only
        check "K3 latticeSuite" $
          all3 latticeSuite universeK3,
        -- Goedel is Heyting, not Boolean at 1/2
        check "Goedel heytingSuite samples" $
          heytingSuite (godei 0) (godei (1 % 2)) (godei 1)
            && heytingSuite (godei (1 % 3)) (godei (2 % 3)) (godei 1),
        check "Goedel fails excluded middle at 1/2" $
          not (law_excluded_middle (godei (1 % 2) :: Goedel Rational)),
        -- Boolean2Ring
        check "Boolean2Ring XOR/AND" $
          let a = Boolean2Ring True
              b = Boolean2Ring False
           in a + b == Boolean2Ring True
                && a * b == Boolean2Ring False
                && a + a == Boolean2Ring False,
        -- Prop eval
        check "evalProp classical tautology" $
          let p = Imp (And (Var 'A') (Imp (Var 'A') (Var 'B'))) (Var 'B')
              envs = [const False, const True, \v -> v == 'A', \v -> v == 'B']
           in all (\e -> evalProp e p) envs,
        check "evalPropH3 unknown atom" $
          evalPropH3 (const HUnknown) (Var ()) == HUnknown,
        -- Combine
        check "consensusH3" $
          consensusH3 HTrue HTrue == HTrue
            && consensusH3 HTrue HFalse == HUnknown
            && consensusH3 HUnknown HTrue == HUnknown,
        -- Process obs H3
        check "voteH3 timeline" $
          scan (voteH3 3) [True, True, False, True, True]
            == [HUnknown, HUnknown, HUnknown, HTrue, HTrue],
        check "voteLatchH3 freezes" $
          scan (voteLatchH3 2) [True, True, False, False, False]
            == [HUnknown, HTrue, HTrue, HTrue, HTrue],
        check "latchProc" $
          scan (latchProc (voteH3 2)) [True, True, False]
            == [HUnknown, HTrue, HTrue],
        check "ewmaGoedel in unit interval" $
          let ys = scan (ewmaGoedel 0.5) [0, 1, 1, 0]
           in all (\(Goedel r) -> r >= 0 && r <= 1) ys
      ]
  if and results
    then putStrLn "\nAll tests passed."
    else error "Some tests failed."
  where
    universeH3 = [HFalse, HUnknown, HTrue]
    universeK3 = [KFalse, KUnknown, KTrue]
    all3 f xs = and [f a b c | a <- xs, b <- xs, c <- xs]
