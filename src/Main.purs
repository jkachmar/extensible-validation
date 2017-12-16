module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Validation.Semigroup (V, unV)
import Data.Variant (Variant)
import Global.Unsafe (unsafeStringify)
import Validator.Email (validateEmailAddress)
import Validator.NonEmpty (validateNonEmpty)

--------------------------------------------------------------------------------
type ValidationError
  = Variant
  ( nonEmpty     :: Unit
  , emailAddress :: String
  )

type ValidationErrors = Array ValidationError

--------------------------------------------------------------------------------
badEmailValidation :: V ValidationErrors String
badEmailValidation =
  let input = ""
  in    validateNonEmpty     input
     *> validateEmailAddress input

goodEmailValidation :: V ValidationErrors String
goodEmailValidation =
  let input = "steve@example.com"
  in    validateNonEmpty     input
     *> validateEmailAddress input

--------------------------------------------------------------------------------
main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  unV (log <<< unsafeStringify) log badEmailValidation
  unV (log <<< unsafeStringify) log goodEmailValidation

