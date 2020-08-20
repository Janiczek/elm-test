module Test.Fuzz exposing (fuzzTest)

import Fuzz.Internal exposing (Fuzzer)
import GenResult exposing (GenResult(..))
import PRNG exposing (PRNG)
import Random exposing (Generator)
import Simplify
import Test.Expectation exposing (Expectation(..))
import Test.Internal exposing (Test(..), blankDescriptionFailure)


{-| Reject always-failing tests because of bad names or invalid fuzzers.
-}
fuzzTest : Fuzzer a -> String -> (a -> Expectation) -> Test
fuzzTest fuzzer untrimmedDesc getExpectation =
    let
        desc =
            String.trim untrimmedDesc
    in
    if String.isEmpty desc then
        blankDescriptionFailure

    else
        Labeled desc <| validatedFuzzTest fuzzer getExpectation


{-| Knowing that the fuzz test isn't obviously invalid, run the test and package up the results.
-}
validatedFuzzTest : Fuzzer a -> (a -> Expectation) -> Test
validatedFuzzTest fuzzer getExpectation =
    FuzzTest
        (\seed runs ->
            case runUntilFailure fuzzer getExpectation seed runs of
                Nothing ->
                    Pass

                Just failure ->
                    formatExpectation failure
        )


type alias Failure =
    { given : String
    , expectation : Expectation
    }


{-| Runs the specified number of fuzz tests and returns a dictionary of simplified failures.
-}
runUntilFailure : Fuzzer a -> (a -> Expectation) -> Random.Seed -> Int -> Maybe Failure
runUntilFailure fuzzer getExpectation initialSeed totalRuns =
    runOneFuzzIteration fuzzer getExpectation
        |> foldUntil
            totalRuns
            (\( failure, _ ) -> failure /= Nothing)
            ( Nothing, initialSeed )
        -- throw away the random seed
        |> Tuple.first


{-| Generate a fuzzed value, test it, and record the simplified test failure if any.
-}
runOneFuzzIteration : Fuzzer a -> (a -> Expectation) -> ( Maybe Failure, Random.Seed ) -> ( Maybe Failure, Random.Seed )
runOneFuzzIteration fuzzer getExpectation ( _, currentSeed ) =
    let
        genResult : GenResult a
        genResult =
            Fuzz.Internal.generate
                (PRNG.random currentSeed)
                fuzzer

        maybeNextSeed : Maybe Random.Seed
        maybeNextSeed =
            genResult
                |> GenResult.getPrng
                |> PRNG.getSeed

        nextSeed : Random.Seed
        nextSeed =
            case maybeNextSeed of
                Just seed ->
                    seed

                Nothing ->
                    stepSeed currentSeed

        maybeFailure : Maybe Failure
        maybeFailure =
            case genResult of
                Rejected _ ->
                    Nothing

                Generated { prng, value } ->
                    testGeneratedValue value prng fuzzer getExpectation
    in
    ( maybeFailure, nextSeed )


{-| Random.next is private ¯\_(ツ)\_/¯
-}
stepSeed : Random.Seed -> Random.Seed
stepSeed seed =
    seed
        |> Random.step (Random.int 0 0)
        |> Tuple.second


{-| Run a function whose inputs are the same as its outputs a given number of times. Requires the initial state to pass
in and returns the final state. This generic combinator extracts the "run n times" logic from our test running code.
-}
foldUntil : Int -> (a -> Bool) -> a -> (a -> a) -> a
foldUntil remainingRuns endingCondition initialState f =
    if remainingRuns <= 1 || endingCondition initialState then
        initialState

    else
        foldUntil (remainingRuns - 1) endingCondition (f initialState) f


{-| Given a rosetree -- a root to test and branches of simplifications -- run the test and perform simplification if it fails.
-}
testGeneratedValue : a -> PRNG -> Fuzzer a -> (a -> Expectation) -> Maybe Failure
testGeneratedValue value prng fuzzer getExpectation =
    case getExpectation value of
        Pass ->
            Nothing

        _ ->
            Just <| findSimplestFailure prng fuzzer getExpectation


{-| Knowing that the rosetree's root already failed, finds the key and value of the simplest failure.
-}
findSimplestFailure : PRNG -> Fuzzer a -> (a -> Expectation) -> Failure
findSimplestFailure prng fuzzer getExpectation =
    let
        ( simplestValue, _ ) =
            Simplify.simplify
                getExpectation
                fuzzer
                prng
    in
    { given = Test.Internal.toString simplestValue
    , expectation = getExpectation simplestValue
    }


formatExpectation : Failure -> Expectation
formatExpectation { given, expectation } =
    Test.Expectation.withGiven given expectation
