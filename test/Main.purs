module Test.Main where

import Control.Monad.Eff          (Eff)
import Test.Spec.QuickCheck       (QCRunnerEffects)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner           (run)

import Test.Algebrae.Array        as Array
import Test.Algebrae.Map          as Map

import Prelude                    (Unit, discard)

main ∷ Eff (QCRunnerEffects ()) Unit
main = run [consoleReporter] do
  Array.main
  Map.main
