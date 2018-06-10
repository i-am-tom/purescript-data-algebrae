module Test.Algebrae.Array where

import Data.Algebra.Array    as Array
import Data.Array            (length, sortWith)
import Data.Foldable         (sum)
import Data.Function         (on)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe            (Maybe(..))
import Data.Tuple            (Tuple(..), fst, snd)
import Data.Tuple.Nested     (type (/\))
import Test.QuickCheck       ((===))
import Test.Spec             (Spec, describe, it)
import Test.Spec.QuickCheck  (quickCheck)

import Prelude

-- | Given the size of the container, generate an invalid index.
generateOutOfBounds ∷ Array Int → Int → Int
generateOutOfBounds array i = if i < 0 then i else length array + i

main ∷ Spec Unit
main = do
  describe "Array" do
    describe "Empty" do
      it "empties all arrays" $ quickCheck \(xs ∷ Array Int) →
        Array.interpret xs [ Array.Empty ] === Just []

    describe "Push" do
      it "pushes to all lists" $ quickCheck \xs (x ∷ Int) →
        Array.interpret xs [ Array.Push x ] === Just (xs <> [x])

      it "produces non-empty lists" $ quickCheck \xs (x ∷ Int) →
        map ((_ > 0) <<< length) (Array.interpret xs [ Array.Push x ])
          === Just true

    describe "Pop" do
      it "fails on empty list" $ quickCheck \(xs ∷ Array Int) →
        Array.interpret xs [ Array.Empty, Array.Pop ] === Nothing

      it "pops from any non-empty list" $ quickCheck \xs (x ∷ Int) →
        Array.interpret (xs <> [x]) [ Array.Pop ] === Just xs

      it "undoes `Push`" $ quickCheck \xs (x ∷ Int) →
        Array.interpret xs [ Array.Push x, Array.Pop ] === Just xs

    describe "Unshift" do
      it "unshifts to all lists" $ quickCheck \xs (x ∷ Int) →
        Array.interpret xs [ Array.Unshift x ] === Just ([x] <> xs)

      it "produces non-empty lists" $ quickCheck \xs (x ∷ Int) →
        map ((_ > 0) <<< length) (Array.interpret xs [ Array.Unshift x ])
          === Just true

    describe "Shift" do
      it "fails on empty list" $ quickCheck \(xs ∷ Array Int) →
        Array.interpret xs [ Array.Empty, Array.Shift ] === Nothing

      it "shifts from any non-empty list" $ quickCheck \xs (x ∷ Int) →
        Array.interpret ([x] <> xs) [ Array.Shift ] === Just xs

      it "undoes `Unshift`" $ quickCheck \xs (x ∷ Int) →
        Array.interpret xs [ Array.Unshift x, Array.Shift ] === Just xs

    describe "DeleteAt" do
      it "fails on out-of-bounds requests" $ quickCheck \ps (x ∷ Int) →
        Array.interpret ps [ Array.DeleteAt (generateOutOfBounds ps x) ]
          === Nothing

      it "deletes at any valid index" $ quickCheck \ps (x ∷ Int) qs →
        Array.interpret (ps <> [x] <> qs) [ Array.DeleteAt (length ps) ]
          === Just (ps <> qs)

      it "`Shift` === `DeleteAt 0`" $ quickCheck \(ps ∷ Array Int) →
        Array.interpret ps [ Array.DeleteAt 0 ]
          === Array.interpret ps [ Array.Shift ]

      it "`Pop` === `DeleteAt len - 1`" $ quickCheck \(ps ∷ Array Int) →
        Array.interpret ps [ Array.DeleteAt (length ps - 1) ]
          === Array.interpret ps [ Array.Pop ]

    describe "Move" do
      it "fails on out-of-bounds indices" $ quickCheck \(ps ∷ Array Int) x →
        Array.interpret ps [ Array.Move (generateOutOfBounds ps x) x ]
          === Nothing

      it "moves with valid indices" $ quickCheck \ps (x ∷ Int) qs rs →
        let
          from = length ps
          to   = from + length qs
        in
          Array.interpret (ps <> [x] <> qs <> rs) [ Array.Move from to ]
            === Just (ps <> qs <> [x] <> rs)

      it "undoes with the inverse" $ quickCheck \ps (x ∷ Int) qs rs →
        let
          from = length ps
          to   = from + length qs
        in
          Array.interpret (ps <> [x] <> qs <> rs)
            [ Array.Move from to, Array.Move to from ]
                === Just (ps <> [x] <> qs <> rs)

    describe "InsertAt" do
      it "fails when out-of-bounds" $ quickCheck \ps (x ∷ Int) →
        Array.interpret ps [ Array.InsertAt (generateOutOfBounds ps x) 0 ]
          === Nothing

      it "inserts at a valid index" $ quickCheck \ps (x ∷ Int) qs →
        Array.interpret (ps <> qs) [ Array.InsertAt (length ps) x ]
          === Just (ps <> [x] <> qs)

    describe "Swap" do
      it "fails when out-of-bounds" $ quickCheck \ps (x ∷ Int) →
        Array.interpret ps [ Array.Swap 0 (generateOutOfBounds ps x) ]
          === Nothing

      it "swaps valid indices" $ quickCheck \ps (x ∷ Array Int) qs y rs →
        Array.interpret (ps <> [x] <> qs <> [y] <> rs)
          [ Array.Swap (length ps) (length ps + length qs + 1) ]
            === Just (ps <> [y] <> qs <> [x] <> rs)

      it "can be replaced with moves" $ quickCheck \ps (x :: Int) qs y rs ->
        let from = length ps
            to   = length ps + length qs + 1
            list = ps <> [x] <> qs <> [y] <> rs
        in
          on (===) (Array.interpret list)
            [ Array.Swap from to ]

            [ Array.Move from to
            , Array.Move (to - 1) from
            ]

      it "is reversible" $ quickCheck \ps (x ∷ Int) qs y rs →
        let from = length ps
            to   = length ps + length qs + 1
        in
          Array.interpret (ps <> [x] <> qs <> [y] <> rs)
            [ Array.Swap from to, Array.Swap from to ]
                === Just (ps <> [x] <> qs <> [y] <> rs)

    describe "Filter" do
      it "drops trues" $ quickCheck \(xs ∷ Array (Array Unit /\ Array Unit)) →
        let
          input = xs >>= \(Tuple ts fs) → join [ ts $> true, fs $> false ]
        in
          length <$> (Array.interpret input (Array.filter identity input))
            === Just (sum (map (length <<< fst) xs))

      it "drops falses" $ quickCheck \(xs ∷ Array (Array Unit /\ Array Unit)) →
        let
          input = xs >>= \(Tuple ts fs) → join [ ts $> true, fs $> false ]
        in
          length <$> (Array.interpret input (Array.filter not input))
            === Just (sum (map (length <<< snd) xs))

    describe "Sort" do
      it "sorts by a given index" $ quickCheck \(xs ∷ Array Int) →
        let
          withIndices = mapWithIndex Tuple xs
          shuffled    = sortWith snd withIndices
        in
          map (map snd) (Array.interpret shuffled (Array.sortWith fst shuffled))
            === Just xs

      it "has a stable sort" $ quickCheck \(xs ∷ Array Int) (p ∷ Int → Int) →
        (===) (Array.interpret xs (Array.sortWith p xs)) $
          xs `Array.interpret` Array.sortWith p xs
            >>= \ys → ys `Array.interpret` Array.sortWith p ys
