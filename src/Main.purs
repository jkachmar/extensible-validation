module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (fromFoldable)
import Data.List.NonEmpty (NonEmptyList)
import Data.String (fromCharArray, toCharArray)
import Data.Validation.Semigroup (V, unV)
import Data.Variant (Variant)
import Global.Unsafe (unsafeStringify)
import Validator.Email (validateEmailAddress)
import Validator.Length (TooLongOrShort, validateLength)
import Validator.NonEmpty (validateNonEmpty)
import Validator.StringCases (validateMixedCase)

--------------------------------------------------------------------------------
type EmailValidationError
  = Variant
  ( isEmpty      :: Unit
  , invalidEmail :: String
  )

-- Valid email addresses are those that are not empty and pass this email address
-- regex I found on stack overflow that's totally fine guys.
validateEmail :: String -> V (NonEmptyList EmailValidationError) String
validateEmail input =
  validateNonEmpty input
  *> validateEmailAddress input

-- An empty string is a pretty bad email.
badEmail :: String
badEmail = "steve"

-- This one looks a little better though!
goodEmail :: String
goodEmail = "steve@example.com"

--------------------------------------------------------------------------------
type PasswordValidationError
  = Variant
  ( isEmpty      :: Unit
  , tooShort     :: Int
  , tooLong      :: Int
  , hasLowercase :: Unit
  , hasUppercase :: Unit
  )

-- Valid passwords are those that are not empty, contain both uppercase and
-- lowercase characters, and is between 8 and 64 characters long.
validatePassword :: String -> V (NonEmptyList PasswordValidationError) String
validatePassword input =
  validateNonEmpty input
  *> validateMixedCase input
  *> validateStringLength {minLength: 8, maxLength: 64} input

-- An empty string is just as bad a password as it is an email.
badPassword :: String
badPassword = ""

-- This one's looking a little better, but it would be nice if our validator
-- also checked for non-alphabetical characters.
goodPassword :: String
goodPassword = "ThisIsASafePassword"

--------------------------------------------------------------------------------
main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  unV (log <<< showInvalid) log $ validateEmail badEmail
  unV (log <<< showInvalid) log $ validateEmail goodEmail

  unV (log <<< showInvalid) log $ validatePassword badPassword
  unV (log <<< showInvalid) log $ validatePassword goodPassword

  where
    showInvalid :: ∀ a. (NonEmptyList a) -> String
    showInvalid errs = unsafeStringify $ fromFoldable errs

--------------------------------------------------------------------------------
-- | NOTE - This is only necessary because `String` has kind `Type` rather than
-- | the `Type -> Type` one would expect for a `Foldable`.
-- |
-- | A better solution would be to pull in Liam Goodacre's `purescript-moldy`
-- | library, which provides the `Moldable` typeclass for working with
-- | monomorphic, foldable collections.
validateStringLength
  :: ∀ r
    . {minLength :: Int, maxLength :: Int}
  -> String
  -> V (NonEmptyList (TooLongOrShort r)) String
validateStringLength {minLength, maxLength} input =
  map fromCharArray
  $ validateLength {minLength, maxLength}
  $ toCharArray input
