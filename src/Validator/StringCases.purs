module Validator.StringCases where

import Prelude

import Data.List.NonEmpty (NonEmptyList, singleton)
import Data.String (toLower)
import Data.Validation.Semigroup (V, invalid)
import Data.Variant (SProxy(..), Variant, inj)

--------------------------------------------------------------------------------
-- | The type of errors for a string that has lowercase characters when it
-- | should not.
type HasLowercase r = Variant (hasLowercase  :: String | r)

-- | Validate that an input string only contains uppercase characters.
validateAllUppercase
  :: ∀ r
   . String
  -> V (NonEmptyList (HasLowercase r)) String
validateAllUppercase input
  | toLower input == input =
    let err = inj (SProxy :: SProxy "hasLowercase") input
    in invalid $ singleton err
  | otherwise = pure input

--------------------------------------------------------------------------------
-- | The type of errors for a string that has uppercase characters when it
-- | should not.
type HasUppercase r = Variant (hasUppercase  :: String | r)

-- | Validate that an input string only contains lowercase characters.
validateAllLowercase
  :: ∀ r
   . String
  -> V (NonEmptyList (HasUppercase r)) String
validateAllLowercase input
  | toLower input == input =
    let err = inj (SProxy :: SProxy "hasUppercase") input
    in invalid $ singleton err
  | otherwise = pure input

--------------------------------------------------------------------------------
-- | The type of errors for a string that is not "mixed case", i.e. does not
-- | have both uppercase and lowercase characters in it.
-- |
-- | Note that this is just a union of the `hasUppercase` and `hasLowercase`
-- | tags from the previous two validators.
type NonMixedCase r
  = Variant
  ( hasUppercase :: String
  , hasLowercase :: String
  | r
  )

-- | Validate that an input string contains both uppercase and lowercase
-- | characters.
validateMixedCase
  :: ∀ r
   . String
  -> V (NonEmptyList (NonMixedCase r)) String
validateMixedCase input =
  validateAllLowercase input
  *> validateAllUppercase input
