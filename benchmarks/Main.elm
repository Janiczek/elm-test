module Main exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram)
import Expect
import Fuzz
import Random
import Test
import Test.Runner


main =
    Benchmark.Runner.program suite


suite : Benchmark
suite =
    Benchmark.describe "elm-test"
        [ benchmark_ "bool"
            Fuzz.bool
        , benchmark_ "int 0 10"
            (Fuzz.intRange 0 10)
        , benchmark_ "weightedBool 0.75"
            (Fuzz.percentage
                |> Fuzz.map (\p -> p <= 0.75)
            )
        , benchmark_ "unit"
            Fuzz.unit
        , benchmark_ "triple ints"
            (Fuzz.triple
                ( Fuzz.intRange 0 10
                , Fuzz.intRange 0 10
                , Fuzz.intRange 0 10
                )
            )
        , benchmark_ "list of ints"
            (Fuzz.list (Fuzz.intRange 0 10))
        ]


benchmark_ :
    String
    -> Fuzz.Fuzzer a
    -> Benchmark
benchmark_ label fuzzer =
    let
        genAndShrink seed () =
            let
                runners =
                    Test.Runner.fromTest 100
                        seed
                        (Test.fuzz
                            fuzzer
                            "elm-test"
                            (always (Expect.fail "shrink more!"))
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
                    []

        gen seed () =
            Random.step
                (Test.Runner.fuzz fuzzer)
                seed
    in
    describe label
        [ benchmark "(0) gen" (gen (Random.initialSeed 0))
        , benchmark "(1) gen" (gen (Random.initialSeed 1))
        , benchmark "(2) gen" (gen (Random.initialSeed 2))
        , benchmark "(3) gen" (gen (Random.initialSeed 3))
        , benchmark "(4) gen" (gen (Random.initialSeed 4))
        , benchmark "(0) gen + shrink" (genAndShrink (Random.initialSeed 0))
        , benchmark "(1) gen + shrink" (genAndShrink (Random.initialSeed 1))
        , benchmark "(2) gen + shrink" (genAndShrink (Random.initialSeed 2))
        , benchmark "(3) gen + shrink" (genAndShrink (Random.initialSeed 3))
        , benchmark "(4) gen + shrink" (genAndShrink (Random.initialSeed 4))
        ]
