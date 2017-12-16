module Validator.NonEmpty where

import Prelude

import Data.Monoid (class Monoid, mempty)
import Data.Validation.Semigroup (V, invalid)
import Data.Variant (SProxy(..), Variant, inj)

--------------------------------------------------------------------------------
-- | The type of non-empty input validation errors.
type NonEmptyError r = Variant (nonEmpty :: Unit | r)

-- | Validate that some input is non-empty.
validateNonEmpty
  :: ∀ a r
   . Eq a
  => Monoid a
  => a
  -> V (Array (NonEmptyError r)) a
validateNonEmpty input
  | input == mempty =
    let err = inj (SProxy :: SProxy "nonEmpty") unit
    in invalid [err]
  | otherwise = pure input
