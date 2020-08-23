module Helpers exposing (different, expectPass, expectSimplifiesTo, expectTestToFail, expectToFail, randomSeedFuzzer, same, succeeded, testFailing, testSimplifying, testStringLengthIsPreserved)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Random
import Test exposing (Test)
import Test.Runner exposing (Runner, SeededRunners)
import Test.Runner.Failure exposing (Reason(..))


{-| To test simplifying, we have to fail some tests so we can simplify their inputs.
The best place we found for storing the expected last state(s) of the simplifying procedure is the description field, which is why we have this function here.
Previously, we (ab)used Expect.true for this, but since that was removed, here we are.
-}
expectSimplifiesTo : String -> Bool -> Expectation
expectSimplifiesTo label a =
    Expect.equal True a |> Expect.onFail label


testSimplifying : Int -> Test -> Test
testSimplifying runs test =
    let
        handleFailure { given, description } =
            case given of
                Nothing ->
                    Err "Expected this test to have a given value!"

                Just g ->
                    if g == description then
                        Ok ()

                    else
                        Err <| "Got simplified value " ++ g ++ " but expected " ++ description

        seed =
            Random.initialSeed 99
    in
    test
        |> Test.Runner.fromTest runs seed
        |> getRunners
        |> List.head
        |> Maybe.map
            (\runner ->
                Test.test
                    (List.head runner.labels
                        |> Maybe.withDefault "Failed"
                    )
                <|
                    \() ->
                        runner.run
                            |> (\run -> run ())
                            |> passToFail handleFailure
            )
        |> Maybe.withDefault (Test.test "Failed" <| \() -> Expect.fail "Failed")


testFailing : Test -> Test
testFailing test =
    let
        handleFailure { given, description } =
            case given of
                Nothing ->
                    Err "Expected this test to have a given value!"

                Just g ->
                    Ok ()

        seed =
            Random.initialSeed 99
    in
    test
        |> Test.Runner.fromTest 1000 seed
        |> getRunners
        |> List.head
        |> Maybe.map
            (\runner ->
                Test.test
                    (List.head runner.labels
                        |> Maybe.withDefault "Failed"
                    )
                <|
                    \() ->
                        runner.run
                            |> (\run -> run ())
                            |> passToFail handleFailure
            )
        |> Maybe.withDefault (Test.test "Failed" <| \() -> Expect.fail "Failed")


expectPass : a -> Expectation
expectPass _ =
    Expect.pass


testStringLengthIsPreserved : List String -> Expectation
testStringLengthIsPreserved strings =
    strings
        |> List.map String.length
        |> List.sum
        |> Expect.equal (String.length (List.foldl (++) "" strings))


expectToFail : Expectation -> Expectation
expectToFail expectation =
    case Test.Runner.getFailureReason expectation of
        Nothing ->
            Expect.fail "Expected this test to fail, but it passed!"

        Just _ ->
            Expect.pass


expectTestToFail : Test -> Expectation
expectTestToFail test =
    let
        seed =
            Random.initialSeed 99
    in
    test
        |> Test.Runner.fromTest 100 seed
        |> getRunners
        |> List.map (.run >> (\run -> run ()))
        |> List.map expectToFail
        |> List.map always
        |> Expect.all
        |> (\all -> all ())


succeeded : Expectation -> Bool
succeeded expectation =
    case Test.Runner.getFailureReason expectation of
        Nothing ->
            True

        Just _ ->
            False


passToFail :
    ({ reason : Reason
     , description : String
     , given : Maybe String
     }
     -> Result String ()
    )
    -> Expectation
    -> Expectation
passToFail f expectation =
    let
        result =
            case Test.Runner.getFailureReason expectation of
                Nothing ->
                    Err "Expected this test to fail, but it passed!"

                Just record ->
                    f record
    in
    case result of
        Ok () ->
            Expect.pass

        Err message ->
            Expect.fail message


getRunners : SeededRunners -> List Runner
getRunners seededRunners =
    case seededRunners of
        Test.Runner.Plain runners ->
            runners

        Test.Runner.Only runners ->
            runners

        Test.Runner.Skipping runners ->
            runners

        Test.Runner.Invalid _ ->
            []


{-| get a good distribution of random seeds, and don't simplify our seeds!
-}
randomSeedFuzzer : Fuzzer Random.Seed
randomSeedFuzzer =
    Fuzz.intRange 0 0xFFFFFFFF
        |> Fuzz.map Random.initialSeed


same : Expectation -> Expectation -> Expectation
same a b =
    case ( Test.Runner.getFailureReason a, Test.Runner.getFailureReason b ) of
        ( Nothing, Nothing ) ->
            Expect.pass

        ( Just _, Just _ ) ->
            Expect.pass

        ( reasonA, reasonB ) ->
            Expect.equal reasonA reasonB
                |> Expect.onFail "expected both arguments to fail, or both to succeed"


different : Expectation -> Expectation -> Expectation
different a b =
    case ( Test.Runner.getFailureReason a, Test.Runner.getFailureReason b ) of
        ( Nothing, Just _ ) ->
            Expect.pass

        ( Just _, Nothing ) ->
            Expect.pass

        ( Nothing, Nothing ) ->
            Expect.fail "expected only one argument to fail, but both passed"

        ( Just reasonA, Just reasonB ) ->
            [ reasonA, reasonB ]
                |> Expect.equal []
                |> Expect.onFail "expected only one argument to fail"
