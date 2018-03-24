module Data.Algebra.Array where

import Data.Array            (cons, deleteAt, foldM, insertAt, snoc, uncons, unsnoc, updateAt, (!!))
import Data.Generic.Rep      (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe            (Maybe(..))
import Prelude

-- | An update algebra for incremental modification.
data Update value
  -- Remove everything from the array.
  = Empty

  -- Remove the last thing in the array if the array has any elements.
  | Pop

  -- Remove the first thing in the array if the array has any elements.
  | Shift

  -- Delete the element at the given index (unless out-of-bounds), decrementing
  -- the indices of all subsequent values down by 1.
  | DeleteAt Int

  -- Move the element at the first index to the second index. Note that the
  -- second index is the actual index of the element after the move - if the
  -- first index is lower than the second, the value will be placed _after_ the
  -- value originally at the second index. See the tests for examples.
  | Move Int Int

  -- Insert a value at the given index, such that it has that index after the
  -- operation.
  | InsertAt Int value

  -- Add a value to the end of the array.
  | Push value

  -- Swap the elements at the two given indices, such that they each have the
  -- other's index once the operation is complete.
  | Swap Int Int

  -- Add a value to the beginning of the array, incrementing all indices of
  -- values already in the array.
  | Unshift value

derive instance eqUpdate      ∷ Eq  value ⇒ Eq  (Update value)
derive instance genericUpdate ∷ Generic (Update value) _
derive instance ordUpdate     ∷ Ord value ⇒ Ord (Update value)
derive instance functorUpdate ∷ Functor Update

instance showUpdate ∷ Show value ⇒ Show (Update value) where
  show = genericShow

-- | Perform a set of incremental updates on an array, maybe returning a
-- | result.
interpret ∷ ∀ value. Array value → Array (Update value) → Maybe (Array value)
interpret
  = foldM \values →
      case _ of
        Empty                → Just []
        Pop                  → map _.init (unsnoc values)
        Shift                → map _.tail (uncons values)
        DeleteAt index       → deleteAt index values
        InsertAt index value → insertAt index value values
        Unshift value        → Just (cons value values)
        Push value           → Just (snoc values value)

        Move from to → do
          elem ← values !! from

          tmp ← deleteAt from values
          insertAt to elem tmp

        Swap this that → do
          this' ← values !! this
          that' ← values !! that

          tmp ← updateAt this that' values
          updateAt that this' tmp
