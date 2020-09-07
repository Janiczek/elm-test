module Main exposing (main)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Html exposing (Html)
import Random
import Test
import Test.Runner


main : Html msg
main =
    List.range 0 20
        |> List.reverse
        |> List.map (genAndShrink fuzzer)
        |> Debug.toString
        |> Html.text


fuzzer : Fuzzer ( Int, Int, Int )
fuzzer =
    Fuzz.triple
        ( Fuzz.intRange 0 10
        , Fuzz.intRange 0 10
        , Fuzz.intRange 0 10
        )


genAndShrink : Fuzzer a -> Int -> List (List Expectation)
genAndShrink fuzzer_ seed =
    let
        _ =
            Debug.log "running for seed" seed
    in
    let
        runners =
            Test.Runner.fromTest 100
                (Random.initialSeed seed)
                (Test.fuzz
                    fuzzer_
                    "elm-test"
                    (\x ->
                        let
                            _ =
                                Debug.log "generated/shrunk value" x
                        in
                        Expect.fail "shrink more!"
                    )
                )
    in
    case runners of
        Test.Runner.Plain xs ->
            List.map (\runner -> runner.run ()) xs

        Test.Runner.Only xs ->
            List.map (\runner -> runner.run ()) xs

        Test.Runner.Skipping xs ->
            List.map (\runner -> runner.run ()) xs

        Test.Runner.Invalid str ->
            [ [] ]
