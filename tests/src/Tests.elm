module Tests exposing (all)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import FloatWithinTests exposing (floatWithinTests)
import Fuzz exposing (..)
import FuzzerTests exposing (fuzzerTests)
import Helpers exposing (..)
import Random
import RunnerTests
import Test exposing (..)
import Test.Html.EventTests
import Test.Html.ExampleAppTests
import Test.Html.Query.CustomNodeTests
import Test.Html.Query.MarkdownTests
import Test.Html.QueryTests
import Test.Html.SelectorTests
import Test.Runner


all : Test
all =
    {-
       Test.concat
           [ readmeExample
           , regressions
           , testTests
           , expectationTests
           , fuzzerTests
           , floatWithinTests
           , RunnerTests.all
           , elmHtmlTests
           ]
    -}
    janiczekTests



-- TODO float, floatRange, string, list, array
-- TODO oneOf, oneOfValues, map2, map3, map4, map5, andMap,
-- TODO frequency, frequencyValues, andThen, lazy, filter
-- TODO pair, triple
-- TODO char, order, invalid, weightedBool


janiczekTests : Test
janiczekTests =
    Test.describe "TODO categorize these tests"
        [ describe "bool"
            [ canGenerate False Fuzz.bool
            , canGenerate True Fuzz.bool
            , simplifiesTowards "simplest" False simplest Fuzz.bool
            , simplifiesTowards "non-False" True (\v -> v == False) Fuzz.bool
            ]
        , describe "unit"
            [ canGenerate () Fuzz.unit
            , simplifiesTowards "()" () simplest Fuzz.unit
            ]
        , describe "constant"
            [ passes "Returns what you give it - Int"
                (Fuzz.constant 42)
                (\v -> v == 42)
            , passes "Returns what you give it - different Int"
                (Fuzz.constant 999)
                (\v -> v == 999)
            , passes "Returns what you give it - Bool"
                (Fuzz.constant True)
                (\v -> v == True)
            , simplifiesTowards "42" 42 simplest (Fuzz.constant 42)
            ]
        , describe "maybe"
            [ canGenerateSatisfying "Just" (Fuzz.maybe Fuzz.unit) ((/=) Nothing)
            , canGenerateSatisfying "Nothing" (Fuzz.maybe Fuzz.unit) ((==) Nothing)
            ]
        , describe "result"
            [ canGenerateSatisfying "Ok"
                (Fuzz.result Fuzz.unit Fuzz.unit)
                (Result.toMaybe >> (/=) Nothing)
            , canGenerateSatisfying "Err"
                (Fuzz.result Fuzz.unit Fuzz.unit)
                (Result.toMaybe >> (==) Nothing)
            ]
        , describe "map"
            [ passes "Any number * 2 = even number"
                (Fuzz.intRange 0 5
                    |> Fuzz.map (\n -> n * 2)
                )
                (\n -> modBy 2 n == 0)
            ]
        , describe "intRange"
            [ passes "Full range"
                (Fuzz.intRange (negate 0xFFFFFFFF) 0xFFFFFFFF)
                (\n -> n >= negate 0xFFFFFFFF && n <= 0xFFFFFFFF)
            , passes "Smaller range"
                (Fuzz.intRange -5 5)
                (\n -> n >= -5 && n <= 5)
            , canGenerate (negate 0xFFFFFFFF)
                (Fuzz.intRange (negate 0xFFFFFFFF) 0xFFFFFFFF)
            , canGenerate 0xFFFFFFFF
                (Fuzz.intRange (negate 0xFFFFFFFF) 0xFFFFFFFF)

            -- TODO: rejects "Limits out of order (hi < lo)"
            ]
        , describe "int"
            [ passes "Full range"
                Fuzz.int
                (\n -> n >= negate 0xFFFFFFFF && n <= 0xFFFFFFFF)
            , canGenerate (negate 0xFFFFFFFF) Fuzz.int
            , -- TODO ~janiczek: the probabilities are too low for my liking -
              -- this test fails way too often
              canGenerate 0xFFFFFFFF Fuzz.int
            , cannotGenerateSatisfying "any Infinity"
                Fuzz.int
                (isInfinite << toFloat)
            , cannotGenerateSatisfying "NaN"
                Fuzz.int
                (isNaN << toFloat)
            ]
        , describe "percentage"
            [ passes "Range 0..1"
                Fuzz.percentage
                (\p -> p >= 0 && p <= 1)
            , cannotGenerateSatisfying "any Infinity" Fuzz.percentage isInfinite
            , cannotGenerateSatisfying "NaN" Fuzz.percentage isNaN
            , simplifiesTowards "simplest" 0 simplest Fuzz.percentage
            , simplifiesTowards "non-zero" 1 (\v -> v == 0) Fuzz.percentage
            , simplifiesTowards "non-zero non-one"
                0.25
                (\v -> v == 1 || v < 0.25)
                Fuzz.percentage
            ]
        ]


{-| An user test function that makes the simplifier simplify the value fully.
-}
simplest : a -> Bool
simplest _ =
    False


passes : String -> Fuzzer a -> (a -> Bool) -> Test
passes label fuzzer fn =
    fuzz fuzzer label (fn >> Expect.equal True)


canGenerateSatisfying : String -> Fuzzer a -> (a -> Bool) -> Test
canGenerateSatisfying label fuzzer fn =
    testFailing <|
        fuzz fuzzer
            ("Can generate satisfying: " ++ label)
            (\fuzzedValue ->
                (not <| fn fuzzedValue)
                    |> Expect.equal True
            )


cannotGenerateSatisfying : String -> Fuzzer a -> (a -> Bool) -> Test
cannotGenerateSatisfying label fuzzer fn =
    passes ("Cannot generate satisfying: " ++ label)
        fuzzer
        (not << fn)


canGenerate : a -> Fuzzer a -> Test
canGenerate value fuzzer =
    let
        valueString =
            Debug.toString value
    in
    testSimplifying_ <|
        fuzz fuzzer
            ("Can generate " ++ valueString)
            (\fuzzedValue ->
                (fuzzedValue /= value)
                    |> expectSimplifiesTo valueString
            )


simplifiesTowards : String -> a -> (a -> Bool) -> Fuzzer a -> Test
simplifiesTowards label value fn fuzzer =
    let
        valueString =
            Debug.toString value
    in
    testSimplifying_ <|
        fuzz fuzzer
            ("[" ++ label ++ "] Simplifies towards " ++ valueString)
            (\fuzzedValue ->
                fn fuzzedValue
                    |> expectSimplifiesTo valueString
            )


elmHtmlTests : Test
elmHtmlTests =
    describe "elm-html-test"
        [ Test.Html.QueryTests.all
        , Test.Html.Query.MarkdownTests.all
        , Test.Html.Query.CustomNodeTests.all
        , Test.Html.SelectorTests.all
        , Test.Html.EventTests.all
        , Test.Html.ExampleAppTests.all
        ]


readmeExample : Test
readmeExample =
    describe "The String module"
        [ describe "String.reverse"
            [ test "has no effect on a palindrome" <|
                \_ ->
                    let
                        palindrome =
                            "hannah"
                    in
                    Expect.equal palindrome (String.reverse palindrome)
            , test "reverses a known string" <|
                \_ ->
                    "ABCDEFG"
                        |> String.reverse
                        |> Expect.equal "GFEDCBA"
            , fuzz string "restores the original string if you run it again" <|
                \randomlyGeneratedString ->
                    randomlyGeneratedString
                        |> String.reverse
                        |> String.reverse
                        |> Expect.equal randomlyGeneratedString
            ]
        ]


expectationTests : Test
expectationTests =
    describe "Expectations"
        [ describe "Expect.err"
            [ test "passes on Err _" <|
                \_ ->
                    Err 12 |> Expect.err
            , test "passes on Ok _" <|
                \_ ->
                    Ok 12
                        |> Expect.err
                        |> expectToFail
            ]
        , describe "Expect.all"
            [ test "fails with empty list" <|
                \_ ->
                    "dummy subject"
                        |> Expect.all []
                        |> expectToFail
            ]
        , describe "Expect.equal"
            [ test "fails when equating two floats (see #230)" <|
                \_ ->
                    1.41
                        |> Expect.equal 1.41
                        |> expectToFail
            , test "succeeds when equating two ints" <|
                \_ -> 141 |> Expect.equal 141
            ]

        -- , describe "Expect.equal on unicode strings should show pretty output"
        --     [ test "ascii" <|
        --         \_ -> "😻🙀👻" |> Expect.equal "🙀👻😻🙈"
        --     , test "ascii space vs nbsp" <|
        --         \_ -> "asd qwe" |> Expect.equal "asd\u{00a0}qwe"
        --     , test "ascii only" <|
        --         \_ -> "asd qwe" |> Expect.equal "asd dwe"
        --     , test "newline diff" <|
        --         \_ -> "first\u{000a}second" |> Expect.equal "first\r\nsecond"
        --     , test "long lines" <|
        --         \_ -> "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque in scelerisque arcu. Curabitur cursus efficitur turpis sed porttitor. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Nunc eu cursus ex. Proin accumsan quam quis dui semper finibus. Nunc vel nibh at tellus finibus rhoncus eu eget dolor. Sed eget neque ut lorem imperdiet fermentum. 😻 Morbi iaculis ante euismod, vulputate velit ut, varius velit. Nulla tempus dapibus mattis. In tempus, nisi a porta lobortis, nulla lacus iaculis quam, vel euismod magna risus at tortor. Integer porta urna odio. Nulla pellentesque dictum maximus. Donec auctor urna nec tortor imperdiet varius." |> Expect.equal "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque in scelerisque arcu. Curabitur cursus efficitur turpis sed porttitor. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Nunc eu cursus ex. Proin accumsan quam quis dui semper finibus. Nunc vel nibh at tellus finibus rhoncus eu eget dolor. Sed eget neque ut lorem imperdiet fermentum. Morbi iaculis ante euismod, vulputate velit ut, varius velit. Nulla tempus dapibus mattis. In tempus, nisi a porta lobortis, nulla lacus iaculis quam, vel euismod magna risus at tortor. Integer porta urna odio. Nulla pellentesque dictum maximus. Donec auctor urna nec tortor imperdiet varius."
        --     ]
        ]


regressions : Test
regressions =
    describe "regression tests"
        [ fuzz (intRange 1 32) "for elm-community/elm-test #39" <|
            \positiveInt ->
                positiveInt
                    |> Expect.greaterThan 0
        , test "for elm-community/elm-test #127" <|
            {- If fuzz tests actually run 100 times, then asserting that no number
               in 1..8 equals 5 fails with 0.999998 probability. If they only run
               once, or stop after a duplicate due to #127, then it's much more
               likely (but not guaranteed) that the 5 won't turn up. See #128.
               (Issue numbers refer to elm-community/elm-test.)
            -}
            \() ->
                fuzz (intRange 1 8)
                    "fuzz tests run 100 times"
                    (Expect.notEqual 5)
                    |> expectTestToFail
        ]


testTests : Test
testTests =
    describe "functions that create tests"
        [ describe "describe"
            [ test "fails with empty list" <|
                \() ->
                    describe "x" []
                        |> expectTestToFail
            , test "fails with empty description" <|
                \() ->
                    describe "" [ test "x" expectPass ]
                        |> expectTestToFail
            ]
        , describe "test"
            [ test "fails with empty name" <|
                \() ->
                    test "" expectPass
                        |> expectTestToFail
            ]
        , describe "fuzz"
            [ test "fails with empty name" <|
                \() ->
                    fuzz Fuzz.bool "" expectPass
                        |> expectTestToFail
            ]
        , describe "fuzzWith"
            [ test "fails with fewer than 1 run" <|
                \() ->
                    fuzzWith { runs = 0 } Fuzz.bool "nonpositive" expectPass
                        |> expectTestToFail
            , test "fails with empty name" <|
                \() ->
                    fuzzWith { runs = 1 } Fuzz.bool "" expectPass
                        |> expectTestToFail
            ]
        , describe "Test.todo"
            [ test "causes test failure" <|
                \() ->
                    todo "a TODO test fails"
                        |> expectTestToFail
            , test "Passes are not TODO"
                (\_ -> Expect.pass |> Test.Runner.isTodo |> Expect.equal False)
            , test "Simple failures are not TODO" <|
                \_ ->
                    Expect.fail "reason" |> Test.Runner.isTodo |> Expect.equal False
            ]
        , identicalNamesAreRejectedTests
        ]


identicalNamesAreRejectedTests : Test
identicalNamesAreRejectedTests =
    describe "Identically-named sibling and parent/child tests fail"
        [ test "a describe with two identically named children" <|
            \() ->
                describe "x"
                    [ test "foo" expectPass
                    , test "foo" expectPass
                    ]
                    |> expectTestToFail
        , test "a describe with the same name as a child test" <|
            \() ->
                describe "A"
                    [ test "A" expectPass ]
                    |> expectTestToFail
        , test "a describe with the same name as a child describe fails" <|
            \() ->
                describe "A"
                    [ describe "A"
                        [ test "x" expectPass ]
                    ]
                    |> expectTestToFail
        , test "a describe with the same name as a sibling describe fails" <|
            \() ->
                Test.concat
                    [ describe "A" [ test "x" expectPass ]
                    , describe "A" [ test "y" expectPass ]
                    ]
                    |> expectTestToFail
        , test "a describe with the same name as a de facto sibling describe fails" <|
            \() ->
                Test.concat
                    [ Test.concat
                        [ describe "A" [ test "x" expectPass ]
                        ]
                    , describe "A" [ test "y" expectPass ]
                    ]
                    |> expectTestToFail
        , test "a describe with the same name as a de facto sibling describe fails (2)" <|
            \() ->
                Test.concat
                    [ Test.concat
                        [ describe "A" [ test "x" expectPass ]
                        ]
                    , Test.concat
                        [ describe "A" [ test "y" expectPass ]
                        ]
                    ]
                    |> expectTestToFail
        ]
