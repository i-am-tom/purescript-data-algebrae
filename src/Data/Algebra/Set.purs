module Data.Algebra.Set where

import Data.Array            (foldRecM)
import Data.Generic.Rep      (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Set              (Set, delete, empty, insert)
import Data.Maybe            (Maybe(..))
import Prelude

-- | An update algebra for incremental modification.
data Update value

  -- Remove everything from the set.
  = Empty

  -- Delete a particular value from the set.
  | Delete value

  -- Add a value to the set.
  | Add value

derive instance eqUpdate      ∷ Eq value ⇒ Eq (Update value)
derive instance genericUpdate ∷ Generic (Update value) _
derive instance ordUpdate     ∷ Ord value ⇒ Ord (Update value)
derive instance functorUpdate ∷ Functor Update

instance showUpdate ∷ (Show value) ⇒ Show (Update value) where
  show = genericShow

-- | Perform a set of incremental updates on an array, maybe returning a
-- | result.
interpret
  ∷ ∀ value
  . Ord value
  ⇒ Set value
  → Array (Update value)
  → Maybe (Set value)
interpret
  = foldRecM \values →
      case _ of
        Empty →
          Just empty

        Delete value →
          Just (delete value values)

        Add value →
          Just (insert value values)
