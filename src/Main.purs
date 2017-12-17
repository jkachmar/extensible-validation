module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (fromFoldable)
import Data.Bifunctor (bimap)
import Data.List.NonEmpty (NonEmptyList, singleton)
import Data.Newtype (class Newtype)
import Data.String (fromCharArray, toCharArray)
import Data.Validation.Semigroup (V, unV)
import Data.Variant (SProxy(..), Variant, inj)
import Global.Unsafe (unsafeStringify)
import Validator.Email (validateEmailAddress)
import Validator.Length (TooLongOrShort, validateLength)
import Validator.NonEmpty (validateNonEmpty)
import Validator.StringCases (validateMixedCase)

--------------------------------------------------------------------------------
-- | Email validations can fail with one of the following error tags.
type EmailValidationError
  = Variant
  ( isEmpty      :: Unit
  , invalidEmail :: String
  )

-- | Valid email addresses are those that are not empty and pass this email
-- | address regex I found on stack overflow that's totally fine guys.
validateEmail :: String -> V (NonEmptyList EmailValidationError) String
validateEmail input =
  validateNonEmpty input
  *> validateEmailAddress input

-- | An empty string is a pretty bad email.
badEmail :: String
badEmail = "steve"

-- | This one looks a little better though!
goodEmail :: String
goodEmail = "steve@example.com"

--------------------------------------------------------------------------------
-- | Password validations can fail with one of the following error tags.
--
-- | Note how `isEmpty` gets used again here, as it was above.
type PasswordValidationError
  = Variant
  ( isEmpty      :: Unit
  , tooShort     :: Int
  , tooLong      :: Int
  , hasLowercase :: Unit
  , hasUppercase :: Unit
  )

-- | Valid passwords are those that are not empty, contain both uppercase and
-- | lowercase characters, and is between 8 and 64 characters long.
validatePassword :: String -> V (NonEmptyList PasswordValidationError) String
validatePassword input =
  validateNonEmpty input
  *> validateMixedCase input
  *> validateStringLength {minLength: 8, maxLength: 64} input

-- | An empty string is just as bad a password as it is an email.
badPassword :: String
badPassword = ""

-- | This one's looking a little better, but it would be nice if our validator
-- | also checked for non-alphabetical characters.
goodPassword :: String
goodPassword = "ThisIsASafePassword"

--------------------------------------------------------------------------------
-- | This is the type of unvalidated form input, it's just a record of an email
-- | and password
type UnvalidatedForm =
  { email    :: String
  , password :: String
  }

-- | This newtype wrapper distinguishes proper email addresses from simpler
-- | types.
newtype Email = Email String
derive instance ntEmail :: Newtype Email _

-- | This newtype wrapper distinguishes proper passwords from simpler types.
newtype Password = Password String
derive instance ntPassword :: Newtype Password _

-- | This is the type of validated form input, it's a record with the same shape
-- | as `UnvalidatedForm`, but with the newtype wrappers defined above to
-- | identify `Email`s and `Password`s explicitly.
type ValidatedForm =
  { email    :: Email
  , password :: Password
  }

-- | Form field validations can fail with a collection of email validation
-- | errors or a collection of password validation errors (as defined above).
type ValidationError
  = Variant
  ( invalidEmail    :: NonEmptyList EmailValidationError
  , invalidPassword :: NonEmptyList PasswordValidationError
  )

-- | A collection of individual form field validation errors.
type ValidationErrors = NonEmptyList ValidationError

-- | Valid forms are those that have a valid email address in the `email` field
-- | and a valid password in the `password` field.
validateForm :: UnvalidatedForm -> V ValidationErrors ValidatedForm
validateForm {email, password} = {email: _, password: _}
  <$> (tagEmail $ validateEmail email)
  <*> (tagPassword $ validatePassword password)
  where
    tagEmail
      :: V (NonEmptyList EmailValidationError) String
      -> V (NonEmptyList ValidationError) Email
    tagEmail =
      bimap
      (singleton <<< (inj $ SProxy :: SProxy "invalidEmail"))
      Email

    tagPassword
      :: V (NonEmptyList PasswordValidationError) String
      -> V (NonEmptyList ValidationError) Password
    tagPassword =
      bimap
      (singleton <<< (inj $ SProxy :: SProxy "invalidPassword"))
      Password

-- | A form with bad email address and password fields.
badForm :: UnvalidatedForm
badForm = {email: badEmail, password: badPassword}

-- | A form with good email address and password fields.
goodForm :: UnvalidatedForm
goodForm = {email: goodEmail, password: goodPassword}

--------------------------------------------------------------------------------
main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  unV (log <<< showInvalid) log $ validateEmail badEmail
  unV (log <<< showInvalid) log $ validateEmail goodEmail

  unV (log <<< showInvalid) log $ validatePassword badPassword
  unV (log <<< showInvalid) log $ validatePassword goodPassword

  unV (log <<< show <<< fromFoldable) (log <<< unsafeStringify) $ validateForm badForm
  unV (log <<< show <<< fromFoldable) (log <<< unsafeStringify) $ validateForm goodForm

  where
    showInvalid :: ∀ a. (NonEmptyList a) -> String
    showInvalid errs = unsafeStringify $ fromFoldable errs

--------------------------------------------------------------------------------
-- | NOTE - This is only necessary because `String` has kind `Type` rather than
-- | the `Type -> Type` one would expect for a `Foldable`.
-- |
-- | A better solution would be to pull in Liam Goodacre's `purescript-moldy`
-- | library, which provides the `Moldable` typeclass for monomorphic, foldable
-- | collections.
validateStringLength
  :: ∀ r
    . {minLength :: Int, maxLength :: Int}
  -> String
  -> V (NonEmptyList (TooLongOrShort r)) String
validateStringLength {minLength, maxLength} input =
  map fromCharArray
  $ validateLength {minLength, maxLength}
  $ toCharArray input
