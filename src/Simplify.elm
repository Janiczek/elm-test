module Simplify exposing (simplify)

{-| TODO docs
-}

import Fuzz.Internal exposing (Fuzzer)
import PRNG exposing (PRNG)
import Test.Expectation exposing (Expectation(..))


simplify : (a -> Expectation) -> Fuzzer a -> PRNG -> ( a, PRNG )
simplify getExpectation fuzzer prng =
    Debug.todo "Simplify.simplify"
