module Test.Main where

import Effect                     (Effect)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner           (run)

import Test.Algebrae.Array        as Array
import Test.Algebrae.Map          as Map

import Prelude                    (Unit, discard)

main ∷ Effect Unit
main = run [consoleReporter] do
  Array.main
  Map.main
