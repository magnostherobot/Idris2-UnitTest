module Test.Unit

import Control.App

public export
data AssertionFailure : Type where
  Fail : String -> AssertionFailure

CanAssert = HasErr AssertionFailure

public export
pass : App es ()
pass = pure ()

public export
fail : CanAssert es => String -> App es t
fail = throw . Fail

public export
TestFunc : Type
TestFunc = {es : _} -> CanAssert es => App es ()

public export
record Test where
  constructor MkTest
  desc : String
  f : TestFunc

public export
PrimIO es => HasIO (App es) where
  liftIO x = primIO x

testPassed : {es : _} -> PrimIO es => () -> App es ()
testPassed () = putStrLn "test passed"

testFailed : {es : _} -> PrimIO es => AssertionFailure -> App es ()
testFailed (Fail msg) = putStrLn ("test failed: " ++ msg)

runTest : {es : _} -> PrimIO es => List Test -> Test -> App es (List Test)
runTest xs t = do putStr (t.desc ++ ": ")
                  let passed = (\_  => testPassed () >> pure xs)
                  let failed = (\af => testFailed af >> pure (t :: xs))
                  handle t.f passed failed

forEach : Foldable t => Monad m => (a -> m ()) -> t a -> m ()
forEach f = foldlM (const f) ()

public export
assert : CanAssert es => Bool -> String -> App es ()
assert True  msg = pass
assert False msg = fail msg

public export
assertEq : CanAssert es => Eq a => Show a => a -> a -> App es ()
assertEq x y = assert (x == y)
  ("expected " ++ show x ++ ", got " ++ show y)

public export
assertThrows : CanAssert es => (e : Error) -> App (e :: es) () -> App es ()
assertThrows e a = handle a (const $ fail "no throw") (const pass)

public export
runTestsApp : {es : _} -> PrimIO es => Foldable t => t Test -> App es ()
runTestsApp ts = do fails <- foldlM runTest [] ts
                    putStrLn (show (length fails) ++ " test(s) failed")

public export
runTests : Foldable t => t Test -> IO ()
runTests = run . runTestsApp
