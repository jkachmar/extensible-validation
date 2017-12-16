module Validator.Email where

import Prelude

import Data.Either (fromRight)
import Data.List.NonEmpty (NonEmptyList, singleton)
import Data.String.Regex (Regex, regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.Validation.Semigroup (V, invalid)
import Data.Variant (SProxy(..), Variant, inj)
import Partial.Unsafe (unsafePartial)

--------------------------------------------------------------------------------
-- | Utility function to unsafely construct a regular expression from a pattern
-- | string.
-- |
-- | This will fail at runtime with an error if the pattern string is invalid.
unsafeRegexFromString :: String -> Regex
unsafeRegexFromString str =
  unsafePartial $ fromRight $ regex str noFlags

-- | Regular expression for email address validation.
emailRegex :: Regex
emailRegex =
  unsafeRegexFromString "^\\w+([.-]?\\w+)*@\\w+([.-]?\\w+)*(\\.\\w{2,3})+$"

-- | The type of email address validation errors.
type InvalidEmail r = Variant (invalidEmail :: String | r)

-- | Validate that some input string is a valid email address.
validateEmailAddress
  :: ∀ r
   . String
  -> V (NonEmptyList (InvalidEmail r)) String
validateEmailAddress input
  | test emailRegex input = pure input
  | otherwise =
    let err = inj (SProxy :: SProxy "invalidEmail") input
    in invalid $ singleton err
