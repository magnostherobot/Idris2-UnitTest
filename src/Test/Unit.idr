module Test.Unit

import Control.App
import Language.Reflection

data AssertionFailure : Type where
  Fail : String -> AssertionFailure

CanAssert = HasErr AssertionFailure

pass : App es ()
pass = pure ()

fail : CanAssert es => String -> App es t
fail = throw . Fail

TestFunc : Type
TestFunc = {es : _} -> CanAssert es => App es ()

record Test where
  constructor MkTest
  desc : String
  f : TestFunc

PrimIO es => HasIO (App es) where
  liftIO x = primIO x

testPassed : {es : _} -> PrimIO es => () -> App es ()
testPassed () = putStrLn "test passed"

testFailed : {es : _} -> PrimIO es => AssertionFailure -> App es ()
testFailed (Fail msg) = putStrLn ("test failed: " ++ msg)

runTest : {es : _} -> PrimIO es => Test -> App es ()
runTest t = do putStr (t.desc ++ ": ")
               handle t.f testPassed testFailed

forEach : Foldable t => Monad m => (a -> m ()) -> t a -> m ()
forEach f = foldlM (const f) ()

tests : {es : _} -> PrimIO es => List Test -> App es ()
tests = forEach runTest

assert : CanAssert es => Bool -> String -> App es ()
assert True  msg = pass
assert False msg = fail msg

assertEq : CanAssert es => Eq a => Show a => a -> a -> App es ()
assertEq x y = assert (x == y)
  ("expected " ++ show x ++ ", got " ++ show y)

ex : Test
ex = MkTest "example description" $ do let x = 1
                                       assertEq x 2

app : {es : _} -> PrimIO es => App es ()
app = tests [ex, ex]

main : IO ()
main = run app
