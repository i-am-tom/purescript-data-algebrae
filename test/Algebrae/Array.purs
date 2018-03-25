module Test.Algebrae.Array where

import Data.Algebra.Array   as Array
import Data.Array           (sort)
import Data.Maybe           (Maybe(..))
import Test.QuickCheck      ((===))
import Test.Spec            (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck)

import Prelude

main ∷ Spec (QCRunnerEffects ()) Unit
main = do
  describe "Array" do
    it "Empty" do
      Array.interpret [ 1, 2, 3 ] [ Array.Empty ]
        `shouldEqual` Just []

    it "Push" do
      Array.interpret [] [ Array.Push 0 ]
        `shouldEqual` Just [ 0 ]

      Array.interpret [ 1, 2, 3 ] [ Array.Push 0 ]
        `shouldEqual` Just [ 1, 2, 3, 0 ]

    it "Unshift" do
      Array.interpret [] [ Array.Unshift 0 ]
        `shouldEqual` Just [ 0 ]

      Array.interpret [ 1, 2, 3 ] [ Array.Unshift 0 ]
        `shouldEqual` Just [ 0, 1, 2, 3 ]

    describe "Pop" do
      it "Empty" do
        Array.interpret [] [ Array.Pop ]
          `shouldEqual` Nothing ∷ Maybe (Array Int)

      it "Non-empty" do
        Array.interpret [ 1, 2, 3 ] [ Array.Pop ]
          `shouldEqual` Just [ 1, 2 ]

    describe "Shift" do
      it "Empty" do
        Array.interpret [] [ Array.Shift ]
          `shouldEqual` Nothing ∷ Maybe (Array Int)

      it "Non-empty" do
        Array.interpret [ 1, 2, 3 ] [ Array.Shift ]
          `shouldEqual` Just [ 2, 3 ]

    describe "DeleteAt" do
      it "Out-of-bounds" do
        Array.interpret [] [ Array.DeleteAt (-1) ]
          `shouldEqual` Nothing ∷ Maybe (Array Int)

      it "Within bounds" do
        Array.interpret [ 1 ] [ Array.DeleteAt 0 ]
          `shouldEqual` Just []

    describe "Move" do
      it "Out-of-bounds" do
        Array.interpret [] [ Array.Move 2 4 ]
          `shouldEqual` Nothing ∷ Maybe (Array Int)

        Array.interpret [ 1 ] [ Array.Move 0 3 ]
          `shouldEqual` Nothing ∷ Maybe (Array Int)

        Array.interpret [ 1 ] [ Array.Move 3 0 ]
          `shouldEqual` Nothing ∷ Maybe (Array Int)

      it "Same index" do
        Array.interpret [ 1 ] [ Array.Move 0 0 ]
          `shouldEqual` Just [ 1 ]

        Array.interpret [ 1, 2, 3 ] [ Array.Move 0 0 ]
          `shouldEqual` Just [ 1, 2, 3 ]

        Array.interpret [ 1, 2, 3 ] [ Array.Move 2 2 ]
          `shouldEqual` Just [ 1, 2, 3 ]

        Array.interpret [ 1, 2, 3 ] [ Array.Move 1 1 ]
          `shouldEqual` Just [ 1, 2, 3 ]

      it "Forward move" do
        Array.interpret [ 1, 2, 3 ] [ Array.Move 0 2 ]
          `shouldEqual` Just [ 2, 3, 1 ]

        Array.interpret [ 1, 2, 3 ] [ Array.Move 1 2 ]
          `shouldEqual` Just [ 1, 3, 2 ]

      it "Backward move" do
        Array.interpret [ 1, 2, 3 ] [ Array.Move 2 0 ]
          `shouldEqual` Just [ 3, 1, 2 ]

        Array.interpret [ 1, 2, 3 ] [ Array.Move 2 1 ]
          `shouldEqual` Just [ 1, 3, 2 ]

    describe "InsertAt" do
      it "Out of index" do
        Array.interpret [] [ Array.InsertAt (-1) 0 ]
          `shouldEqual` Nothing

        Array.interpret [] [ Array.InsertAt 1 0 ]
          `shouldEqual` Nothing

      it "Arbitrary position" do
        Array.interpret [ 1, 2, 3 ] [ Array.InsertAt 0 0 ]
          `shouldEqual` Just [ 0, 1, 2, 3 ]

        Array.interpret [ 1, 2, 3 ] [ Array.InsertAt 1 0 ]
          `shouldEqual` Just [ 1, 0, 2, 3 ]

        Array.interpret [ 1, 2, 3 ] [ Array.InsertAt 2 0 ]
          `shouldEqual` Just [ 1, 2, 0, 3 ]

        Array.interpret [ 1, 2, 3 ] [ Array.InsertAt 3 0 ]
          `shouldEqual` Just [ 1, 2, 3, 0 ]

    describe "Swap" do
      it "Out-of-bounds" do
        Array.interpret [] [ Array.Swap 2 4 ]
          `shouldEqual` Nothing ∷ Maybe (Array Int)

        Array.interpret [ 1 ] [ Array.Swap 0 3 ]
          `shouldEqual` Nothing ∷ Maybe (Array Int)

        Array.interpret [ 1 ] [ Array.Swap 3 0 ]
          `shouldEqual` Nothing ∷ Maybe (Array Int)

      it "Same index" do
        Array.interpret [ 1 ] [ Array.Swap 0 0 ]
          `shouldEqual` Just [ 1 ]

        Array.interpret [ 1, 2, 3 ] [ Array.Swap 0 0 ]
          `shouldEqual` Just [ 1, 2, 3 ]

        Array.interpret [ 1, 2, 3 ] [ Array.Swap 2 2 ]
          `shouldEqual` Just [ 1, 2, 3 ]

        Array.interpret [ 1, 2, 3 ] [ Array.Swap 1 1 ]
          `shouldEqual` Just [ 1, 2, 3 ]

      it "Forward move" do
        Array.interpret [ 1, 2, 3 ] [ Array.Swap 0 2 ]
          `shouldEqual` Just [ 3, 2, 1 ]

        Array.interpret [ 1, 2, 3 ] [ Array.Swap 1 2 ]
          `shouldEqual` Just [ 1, 3, 2 ]

      it "Backward move" do
        Array.interpret [ 1, 2, 3 ] [ Array.Swap 2 0 ]
          `shouldEqual` Just [ 3, 2, 1 ]

        Array.interpret [ 1, 2, 3 ] [ Array.Swap 2 1 ]
          `shouldEqual` Just [ 1, 3, 2 ]

  describe "Sort" do
    it "QuickCheck" do
      quickCheck \(xs ∷ Array Int) →
        let
          expected = Just (sort xs)
          actual   = Array.interpret xs (Array.sort id xs)

        in
          expected === actual
