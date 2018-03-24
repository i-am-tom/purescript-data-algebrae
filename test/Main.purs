module Test.Main where

import Control.Monad.Eff          (Eff)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner           (RunnerEffects, run)

import Test.Algebrae.Array        as Array
import Test.Algebrae.Map          as Map

import Prelude                    (Unit, discard)

main ∷ Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  Array.main
  Map.main
