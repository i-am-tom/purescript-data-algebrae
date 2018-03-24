module Data.Algebra.Array where

import Data.Array            (cons, deleteAt, foldM, insertAt, snoc, uncons, unsnoc, updateAt, (!!))
import Data.Generic.Rep      (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe            (Maybe(..))
import Prelude

-- | update algebra for incremental modification.
data Update value
  = Empty
  | Pop
  | Shift
  | DeleteAt Int
  | Move     Int Int
  | InsertAt Int     value
  | Push             value
  | Swap     Int Int
  | Unshift          value

derive instance eqUpdate      ∷ Eq  value ⇒ Eq  (Update value)
derive instance genericUpdate ∷ Generic (Update value) _
derive instance ordUpdate     ∷ Ord value ⇒ Ord (Update value)
derive instance functorUpdate ∷ Functor Update

instance showUpdate ∷ Show value ⇒ Show (Update value) where
  show = genericShow

-- | Perform a set of incremental updates on an array, maybe returning a
-- | result.
interpret
  ∷ ∀ value
  . Array value
  → Array (Update value)
  → Maybe (Array value)
interpret
  = foldM \values →
      case _ of
        Empty →
          Just []

        Pop →
          map _.init (unsnoc values)

        Shift →
          map _.tail (uncons values)

        DeleteAt index →
          deleteAt index values

        Move from to → do
          elem ← values !! from

          tmp ← deleteAt from values
          insertAt to elem tmp

        InsertAt index value →
          insertAt index value values

        Push value →
          Just (snoc values value)

        Swap this that → do
          this' ← values !! this
          that' ← values !! that

          tmp ← updateAt this that' values
          updateAt that this' tmp

        Unshift value →
          Just (cons value values)
