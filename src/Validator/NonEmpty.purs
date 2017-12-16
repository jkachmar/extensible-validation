module Validator.NonEmpty where

import Prelude

import Data.List.NonEmpty (NonEmptyList, singleton)
import Data.Monoid (class Monoid, mempty)
import Data.Validation.Semigroup (V, invalid)
import Data.Variant (SProxy(..), Variant, inj)

--------------------------------------------------------------------------------
-- | The type of non-empty input validation errors.
type IsEmpty r = Variant (isEmpty :: Unit | r)

-- | Validate that some input is non-empty.
validateNonEmpty
  :: ∀ a r
   . Eq a
  => Monoid a
  => a
  -> V (NonEmptyList (IsEmpty r)) a
validateNonEmpty input
  | input == mempty =
    let err = inj (SProxy :: SProxy "isEmpty") unit
    in invalid $ singleton err
  | otherwise = pure input
