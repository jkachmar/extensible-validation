module Validator.Email where

import Prelude

import Data.Either (fromRight)
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
type EmailAddressError r = Variant (emailAddress :: String | r)

-- | Validate that some input string is a valid email address.
validateEmailAddress
  :: ∀ r
   . String
  -> V (Array (EmailAddressError r)) String
validateEmailAddress input
  | test emailRegex input = pure input
  | otherwise =
    let err = inj (SProxy :: SProxy "emailAddress") input
    in invalid [err]
