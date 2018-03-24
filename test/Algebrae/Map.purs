module Test.Algebrae.Map where

import Data.Algebra.Map           as Map
import Data.Map                   as M
import Data.Maybe                 (Maybe(..))
import Data.Tuple.Nested          ((/\))
import Test.Spec                  (Spec, describe, it)
import Test.Spec.Assertions       (shouldEqual)
import Test.Spec.Runner           (RunnerEffects)

import Prelude

main :: Spec (RunnerEffects ()) Unit
main = do
  let
    standard
      = M.fromFoldable
          [ "a" /\ 1
          , "b" /\ 2
          , "c" /\ 3
          ]

  describe "Map" do
    it "Empty" do
      Map.interpret standard [ Map.Empty ]
        `shouldEqual` Just M.empty

    it "DeleteAt" do
      Map.interpret M.empty [ Map.Delete "missing" ]
        `shouldEqual` Just M.empty ∷ Maybe (M.Map String Int)

      Map.interpret standard [ Map.Delete "a" ]
        `shouldEqual` Just (M.fromFoldable [ "b" /\ 2, "c" /\ 3 ])

    it "Rename" do
      Map.interpret standard [ Map.Rename "d" "a" ]
        `shouldEqual` Nothing

      Map.interpret standard [ Map.Rename "a" "d" ]
        `shouldEqual` Just (M.fromFoldable [ "b" /\ 2, "c" /\ 3, "d" /\ 1 ])

    it "Add" do
      Map.interpret standard [ Map.Add "a" 4 ]
        `shouldEqual` Just (M.fromFoldable [ "a" /\ 4, "b" /\ 2, "c" /\ 3 ])

      Map.interpret M.empty [ Map.Add "d" 4 ]
        `shouldEqual` Just (M.fromFoldable [ "d" /\ 4 ])

    it "Swap" do
      Map.interpret M.empty [ Map.Swap "a" "b" ]
        `shouldEqual` Nothing ∷ Maybe (M.Map String Int)

      Map.interpret (M.fromFoldable [ "a" /\ 1 ]) [ Map.Swap "a" "a" ]
        `shouldEqual` Just (M.fromFoldable [ "a" /\ 1 ])

      Map.interpret (M.fromFoldable [ "a" /\ 1, "b" /\ 2 ]) [ Map.Swap "a" "b" ]
        `shouldEqual` Just (M.fromFoldable [ "a" /\ 2, "b" /\ 1 ])

