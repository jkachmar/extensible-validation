module Validator.Length where

import Prelude

import Data.Foldable (class Foldable, length)
import Data.List.NonEmpty (NonEmptyList, singleton)
import Data.Validation.Semigroup (V, invalid)
import Data.Variant (SProxy(..), Variant, inj)

--------------------------------------------------------------------------------
-- | The type of errors for an input that is shorter than some given length.
-- |
-- | Contains the length (as an `Int`) of the input that failed validation.
type TooShort r = Variant (tooShort :: Int | r)

-- | Validate that some `Foldable` input is greater than or equal to some
-- | `minLength`.
validateMinimumLength
  :: ∀ r t a
   . Foldable t
  => Int
  -> t a
  -> V (NonEmptyList (TooShort r)) (t a)
validateMinimumLength minLength input
  | length input <= minLength =
    let err = inj (SProxy :: SProxy "tooShort") (length input)
    in invalid $ singleton err
  | otherwise = pure input

--------------------------------------------------------------------------------
-- | The type of errors for an input that is longer than some given length.
-- |
-- | Contains the length (as an `Int`) of the input that failed validation.
type TooLong r = Variant (tooLong :: Int | r)

-- | Validate that some `Foldable` input is less than to some `maxLength`.
validateMaximumLength
  :: ∀ r t a
   . Foldable t
  => Int
  -> t a
  -> V (NonEmptyList (TooLong r)) (t a)
validateMaximumLength maxLength input
  | length input > maxLength =
    let err = inj (SProxy :: SProxy "tooLong") (length input)
    in invalid $ singleton err
  | otherwise = pure input

--------------------------------------------------------------------------------
-- | The type of errors for an input that is longer or shorter than a given
-- | range of `minLength` and `maxLength`.
-- |
-- | Contains the length (as an `Int`) of the input that failed validation.
-- |
-- | Note that this is just a union of the `tooLong` and `tooShort` tags from
-- | the previous two validators.
type TooLongOrShort r
  = Variant
  ( tooLong  :: Int
  , tooShort :: Int
  | r
  )

-- | Validate that some `Foldable` input is within the range of some `minLength`
-- | and `maxLength`.
validateLength
  :: ∀ r t a
   . Foldable t
  => {minLength :: Int, maxLength :: Int}
  -> t a
  -> V (NonEmptyList (TooLongOrShort r)) (t a)
validateLength {minLength, maxLength} input =
  validateMinimumLength minLength input
  *> validateMaximumLength maxLength input
