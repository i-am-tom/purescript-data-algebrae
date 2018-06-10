module Test.Algebrae.Set where

import Data.Algebra.Set           as Set
import Data.Set                   as S
import Data.Maybe                 (Maybe(..))
import Test.Spec                  (Spec, describe, it)
import Test.Spec.Assertions       (shouldEqual)

import Prelude

main ∷ Spec Unit
main = do
  let
    standard
      = S.fromFoldable [ "a", "b", "c" ]

  describe "Set" do
    it "Empty" do
      Set.interpret standard [ Set.Empty ]
        `shouldEqual` Just S.empty

    it "Delete" do
      Set.interpret S.empty [ Set.Delete "missing" ]
        `shouldEqual` Just S.empty ∷ Maybe (S.Set String)

      Set.interpret standard [ Set.Delete "a" ]
        `shouldEqual` Just (S.fromFoldable [ "b", "c" ])

    it "Add" do
      Set.interpret standard [ Set.Add "a" ]
        `shouldEqual` Just (S.fromFoldable [ "a", "b", "c" ])

      Set.interpret S.empty [ Set.Add "d" ]
        `shouldEqual` Just (S.fromFoldable [ "d" ])

