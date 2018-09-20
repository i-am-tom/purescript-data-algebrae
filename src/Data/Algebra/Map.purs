module Data.Algebra.Map where

import Data.Array            (foldRecM)
import Data.Generic.Rep      (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map              (Map, delete, empty, insert, lookup)
import Data.Maybe            (Maybe(..))
import Prelude

-- | An update algebra for incremental modification.
data Update key value
  -- Remove everything from the map.
  = Empty

  -- Delete the element at the given key. If the key doesn't exist, this is a
  -- no-op.
  | Delete key

  -- Rename the first key to the second, overwriting anything that may already
  -- be there. This will fail if the first key doesn't exist.
  | Rename key key

  -- Add a value at the given index, overwriting anything that may already be
  -- there.
  | Add key value

  -- Swap the elements at the two given keys, such that they each have the
  -- other's key once the operation is complete.
  | Swap key key

derive instance eqUpdate      ∷ (Eq key, Eq value) ⇒ Eq (Update key value)
derive instance genericUpdate ∷ Generic (Update key value) _
derive instance ordUpdate     ∷ (Ord key, Ord value) ⇒ Ord (Update key value)
derive instance functorUpdate ∷ Functor (Update key)

instance showUpdate ∷ (Show key, Show value) ⇒ Show (Update key value) where
  show = genericShow

-- | Perform a set of incremental updates on an array, maybe returning a
-- | result.
interpret
  ∷ ∀ key value
  . Ord key
  ⇒ Map key value
  → Array (Update key value)
  → Maybe (Map key value)
interpret
  = foldRecM \values →
      case _ of
        Empty →
          Just empty

        Delete key →
          Just (delete key values)

        Rename from to →
          lookup from values <#> \item →
            insert to item (delete from values)
        Add key value →
          Just (insert key value values)

        Swap this that → do
          this' ← lookup this values
          that' ← lookup that values

          pure
            $ insert this that'
            $ insert that this'
            $ values
