module Test.MyParser.Char where

import MyParser
import MyParser.Char (anyChar)

import Data.Either (isLeft)
import Hedgehog (Property, forAll, property, (===))
import Test.HUnit (Assertion, assertBool, (@?=))

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

unit_anyChar_emptyStringGivesLeft :: Assertion
unit_anyChar_emptyStringGivesLeft =
    assertBool "Should be `Left on the empty input`"
      (isLeft $ apply anyChar mempty)


-- hprop_anyChar_nonEmptyStringGivesRight :: Property
-- hprop_anyChar_nonEmptyStringGivesRight = property $ do
--    input <- Gen.string (Range.constant 1 100) Gen.alphaNum
--    let parseResult = apply anyChar input
--    1 === 1


