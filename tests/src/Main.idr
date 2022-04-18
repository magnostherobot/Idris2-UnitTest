module Main

import Test.Unit

import Test.Test.Unit
import Control.App

main : IO ()
main = runTests Test.Test.Unit.tests
