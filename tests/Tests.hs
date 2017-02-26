module Main (
    main
) where

import Test.Tasty
--import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.Char

import Parsers

--------------------------------------------------------------------------------
----------------Test for single character parsers-------------------------------
--------------------------------------------------------------------------------

singlesTests = testGroup "Tests for single character parsers" $ 
  concat [testChar
         , testSat
         ]

-- |Tests for parser char (parses specified character)
testChar = 
  [ testCase "Consuming one specified char" $ 
      parse (char 'c') "c" @?= ("", Just 'c')
  , testCase "Invalid char yields an error" $ 
      (parse (char 'c') "b") @?= ("b", Nothing)
  ]

-- |Tests for parser sat (parses character only if is satisfies predicate)
testSat = 
  [ testCase "Consuming one digit" $ 
      parse (sat isDigit) "123" @?= ("23", Just '1')
  , testCase "Letter is not a digit" $ 
      parse (sat isDigit) "a23" @?= ("a23", Nothing)
  ]

--------------------------------------------------------------------------------
------------------Test for parsers for groups of chars--------------------------
--------------------------------------------------------------------------------

manysTests = testGroup "Tests for multy character parsers" $ 
  [ wordTestGroup
  ]

-- |Tests for parser word (parses non-empty string of letters)
wordTestGroup = testGroup "Test cases for parser word" $ testWord 

testWord = 
  [ testCase "Parse until meeting with a space" $ 
      parse word "ab ba" @?= (" ba", Just "ab")
  , testCase "Fails on empty input" $
      parse word "" @?= ("", Nothing)
  ]

--------------------------------------------------------------------------------
-------------------------Markdown Tests-----------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-------------------------Running tests------------------------------------------
--------------------------------------------------------------------------------

unitTests = testGroup "Unit Tests" [ singlesTests
                                   , manysTests
                                   ]

tests :: TestTree
tests = testGroup "Tests" [unitTests]
     
main = defaultMain tests
