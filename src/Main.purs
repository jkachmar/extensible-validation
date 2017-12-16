module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (fromFoldable)
import Data.List.NonEmpty (NonEmptyList)
import Data.Validation.Semigroup (V, unV)
import Data.Variant (Variant)
import Global.Unsafe (unsafeStringify)
import Validator.Email (validateEmailAddress)
import Validator.NonEmpty (validateNonEmpty)

--------------------------------------------------------------------------------
type EmailValidationError
  = Variant
  ( isEmpty      :: Unit
  , invalidEmail :: String
  )

--------------------------------------------------------------------------------
badEmailValidation :: V (NonEmptyList EmailValidationError) String
badEmailValidation =
  let input = ""
  in validateNonEmpty     input
     *> validateEmailAddress input

goodEmailValidation :: V (NonEmptyList EmailValidationError) String
goodEmailValidation =
  let input = "steve@example.com"
  in validateNonEmpty     input
     *> validateEmailAddress input

--------------------------------------------------------------------------------
main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  unV (log <<< showInvalid) log badEmailValidation
  unV (log <<< showInvalid) log goodEmailValidation

  where
    showInvalid :: ∀ a. (NonEmptyList a) -> String
    showInvalid errs = unsafeStringify $ fromFoldable errs
