module Test.Test.Unit

import System

import Test.Unit

public export
tests : List (Test es)
tests = [ MkTest "assert should throw on False" $ do
          assertThrows AssertionFailure $
            assert False "should throw"

        , MkTest "assert should not throw on True" $ do
          assert True "should not throw"

        , MkTest "assertThrows throws if inner app does not throw" $ do
          assertThrows AssertionFailure $
            assertThrows () $
              pass

        , MkTest ("assertThrows does not throw " ++ 
                  "if inner app throws expected error") $ do
          assertThrows Integer $
            throw 10

        , MkTest "assertThrows does not catch unrelated errors" $ do
          assertThrows Nat $
            assertThrows () $
              throw Z

        , MkTest "innermost appropriate assertThrows catches error" $ do
          assertThrows AssertionFailure $
            assertThrows Integer $
              assertThrows Integer $
                throw 24

        , MkTest "assertDoesNotThrow throws if inner app throws" $ do
          assertThrows AssertionFailure $
            assertDoesNotThrow Integer $
              throw 2

        , MkTest ("assertDoesNotThrow does not throw " ++
                  "if inner app does not throw") $ do
          assertDoesNotThrow String $
            pass

        , MkTest "assertDoesNotThrow only handles appropriate errors" $ do
          assertThrows () $
            assertDoesNotThrow Integer $
              throw ()
        ]
