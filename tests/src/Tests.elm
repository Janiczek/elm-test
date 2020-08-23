module Tests exposing (all)

import Array
import Expect exposing (Expectation, FloatingPointTolerance(..))
import FloatWithinTests exposing (floatWithinTests)
import Fuzz exposing (..)
import FuzzerTests exposing (fuzzerTests)
import Helpers exposing (..)
import Random
import RunnerTests
import Set
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


janiczekTests : Test
janiczekTests =
    Test.describe "TODO categorize these tests"
        [ describe "Tough examples"
            [ simplifiesTowards "redistributed additive pair"
                ( 1, 1000 )
                (\( m, n ) -> m + n <= 1000)
                (Fuzz.pair
                    ( Fuzz.intRange 0 1000
                    , Fuzz.intRange 0 1000
                    )
                )
            , simplifiesTowards "list written in flip-a-coin way"
                [ 1001 ]
                (\list -> List.sum list <= 1000)
                (Fuzz.list (Fuzz.intRange 0 10000))
            , simplifiesTowards "list written in length-first way"
                -- TODO this has problems with run [2,0,1001]
                --                        == value [0,1001]
                [ 1001 ]
                (\list -> List.sum list <= 1000)
                (Fuzz.intRange 0 10
                    |> Fuzz.andThen
                        (\length ->
                            let
                                go : Int -> List Int -> Fuzzer (List Int)
                                go todo acc =
                                    if todo <= 0 then
                                        Fuzz.constant (List.reverse acc)

                                    else
                                        Fuzz.intRange 0 10000
                                            |> Fuzz.andThen (\item -> go (todo - 1) (item :: acc))
                            in
                            go length []
                        )
                )

            -- challenges: https://github.com/jlink/shrinking-challenge
            , simplifiesTowards "challenge: reverse"
                [ 0, 1 ]
                (\list -> list == List.reverse list)
                (Fuzz.list Fuzz.int)
            , -- TODO too big right now, doesn't seem to shrink when it could
              skip <|
                simplifiesTowards "challenge: large union list"
                    [ [ 0, 1, 2, 3, 4 ] ]
                    (\lists -> Set.size (Set.fromList (List.concat lists)) <= 4)
                    (Fuzz.list (Fuzz.list Fuzz.int))
            ]
        , describe "Fuzzers"
            [ describe "bool"
                [ canGenerate False Fuzz.bool
                , canGenerate True Fuzz.bool
                , simplifiesTowards "simplest" False simplest Fuzz.bool
                , simplifiesTowards "next simplest" True (\v -> v == False) Fuzz.bool
                ]
            , describe "order"
                [ canGenerate LT Fuzz.order
                , canGenerate EQ Fuzz.order
                , canGenerate GT Fuzz.order
                , simplifiesTowards "simplest" LT simplest Fuzz.order
                , simplifiesTowards "next simplest" EQ (\x -> x == LT) Fuzz.order
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
                , simplifiesTowards "simplest" Nothing simplest (Fuzz.maybe Fuzz.int)
                , simplifiesTowards "non-Nothing" (Just 0) (\x -> x == Nothing) (Fuzz.maybe Fuzz.int)
                ]
            , describe "result"
                [ canGenerateSatisfying "Ok"
                    (Fuzz.result Fuzz.unit Fuzz.unit)
                    (Result.toMaybe >> (/=) Nothing)
                , canGenerateSatisfying "Err"
                    (Fuzz.result Fuzz.unit Fuzz.unit)
                    (Result.toMaybe >> (==) Nothing)
                , simplifiesTowards "simplest"
                    (Err 0)
                    simplest
                    (Fuzz.result Fuzz.int Fuzz.int)
                , simplifiesTowards "non-Err"
                    (Ok 0)
                    (\x -> Result.toMaybe x == Nothing)
                    (Fuzz.result Fuzz.int Fuzz.int)
                ]
            , describe "map"
                (let
                    fuzzer : Fuzzer Int
                    fuzzer =
                        Fuzz.int
                            |> Fuzz.map (\n -> n * 2)
                 in
                 [ passes "Any number * 2 = even number" fuzzer (\n -> modBy 2 n == 0)
                 , simplifiesTowards "simplest" 0 simplest fuzzer
                 , simplifiesTowards "non-zero" 2 (\n -> n == 0) fuzzer
                 ]
                )
            , describe "intRange"
                [ passes "Smaller range"
                    (Fuzz.intRange -5 5)
                    (\n -> n >= -5 && n <= 5)
                , cannotGenerateSatisfying "Smaller range"
                    (Fuzz.intRange -5 5)
                    (\n -> n < -5 && n > 5)
                , simplifiesTowards "(-,+) simplest" 0 simplest (Fuzz.intRange -5 5)
                , simplifiesTowards "(-,+) non-zero" 1 (\n -> n == 0) (Fuzz.intRange -5 5)
                , simplifiesTowards "(0,+) simplest" 0 simplest (Fuzz.intRange 0 5)
                , simplifiesTowards "(0,+) non-zero" 1 (\n -> n == 0) (Fuzz.intRange 0 5)
                , simplifiesTowards "(+,+) simplest" 1 simplest (Fuzz.intRange 1 5)
                , simplifiesTowards "(+,+) non-low" 2 (\n -> n == 1) (Fuzz.intRange 1 5)
                , simplifiesTowards "(-,0) simplest" 0 simplest (Fuzz.intRange -5 0)
                , simplifiesTowards "(-,0) non-zero" -1 (\n -> n == 0) (Fuzz.intRange -5 0)
                , simplifiesTowards "(-,-) simplest" -1 simplest (Fuzz.intRange -5 -1)
                , simplifiesTowards "(-,-) non-high" -2 (\n -> n == -1) (Fuzz.intRange -5 -1)

                -- TODO: rejects "Limits out of order (hi < lo)"
                ]
            , describe "int"
                [ cannotGenerateSatisfying "any Infinity"
                    Fuzz.int
                    (isInfinite << toFloat)
                , cannotGenerateSatisfying "NaN"
                    Fuzz.int
                    (isNaN << toFloat)
                , simplifiesTowards "simplest" 0 simplest Fuzz.int
                , simplifiesTowards "non-zero" 1 (\n -> n == 0) Fuzz.int
                , simplifiesTowards "negative" -1 (\n -> n >= 0) Fuzz.int
                ]
            , describe "percentage"
                [ passes "Range 0..1"
                    Fuzz.percentage
                    (\p -> p >= 0 && p <= 1)
                , cannotGenerateSatisfying "any Infinity" Fuzz.percentage isInfinite
                , cannotGenerateSatisfying "NaN" Fuzz.percentage isNaN
                , simplifiesTowards "simplest" 0 simplest Fuzz.percentage
                , simplifiesTowards "non-zero" 1.1368683793337427e-13 (\v -> v == 0) Fuzz.percentage
                , simplifiesTowards "non-zero non-one, specific threshold #1"
                    0.25
                    (\v -> v == 1 || v < 0.25)
                    Fuzz.percentage
                , simplifiesTowards "non-zero non-one, specific threshold #2"
                    0.2500000004656613
                    (\v -> v == 1 || v <= 0.25)
                    Fuzz.percentage
                ]
            , describe "char"
                [ passes "Range 32..126"
                    Fuzz.char
                    (\char ->
                        let
                            code =
                                Char.toCode char
                        in
                        code >= 32 && code <= 126
                    )
                , simplifiesTowards "simplest" ' ' simplest Fuzz.char
                , simplifiesTowards "next simplest" '!' (\c -> c == ' ') Fuzz.char
                , simplifiesTowards "above A" 'B' (\c -> Char.toCode c <= Char.toCode 'A') Fuzz.char
                ]
            , describe "string"
                [ canGenerate "" Fuzz.string
                , canGenerateSatisfying "non-empty string"
                    -- TODO flaky test: the probability isn't great without preferring empty string
                    Fuzz.string
                    (not << String.isEmpty)
                , simplifiesTowards "simplest" "" simplest Fuzz.string
                , simplifiesTowards "next simplest" " " (\x -> x == "") Fuzz.string
                , simplifiesTowards "alpha" "A" (\x -> x == "" || not (String.all Char.isAlpha x)) Fuzz.string
                ]
            , describe "oneOfValues"
                [ canGenerate 1 (Fuzz.oneOfValues [ 1, 42 ])
                , canGenerate 42 (Fuzz.oneOfValues [ 1, 42 ])
                , cannotGenerateSatisfying "not in list"
                    (Fuzz.oneOfValues [ 1, 42 ])
                    (\n -> not <| List.member n [ 1, 42 ])
                , passes "One value -> picks it"
                    (Fuzz.oneOfValues [ 42 ])
                    (\n -> n == 42)
                , simplifiesTowards "simplest" 42 simplest (Fuzz.oneOfValues [ 42, 1, 999 ])
                , simplifiesTowards "next simplest" 1 (\x -> x == 42) (Fuzz.oneOfValues [ 42, 1, 999 ])

                -- TODO rejects: empty list
                ]
            , describe "oneOf"
                (let
                    fuzzer : Fuzzer Int
                    fuzzer =
                        Fuzz.oneOf
                            [ Fuzz.intRange -2 0
                            , Fuzz.constant 2
                            ]

                    constFuzzer : Fuzzer Int
                    constFuzzer =
                        Fuzz.oneOf
                            [ Fuzz.constant 42
                            , Fuzz.constant 1
                            , Fuzz.constant 999
                            ]
                 in
                 [ canGenerate -2 fuzzer
                 , canGenerate -1 fuzzer
                 , canGenerate 0 fuzzer
                 , canGenerate 2 fuzzer
                 , cannotGenerateSatisfying "not in the range"
                    fuzzer
                    (\n -> not <| List.member n [ -2, -1, 0, 2 ])
                 , passes "One fuzzer -> picks it"
                    (Fuzz.oneOf [ Fuzz.constant 42 ])
                    (\n -> n == 42)
                 , simplifiesTowards "simplest" 42 simplest constFuzzer
                 , simplifiesTowards "next simplest" 1 (\x -> x == 42) constFuzzer

                 -- TODO rejects: empty list
                 ]
                )
            , describe "frequencyValues"
                (let
                    fuzzer : Fuzzer Int
                    fuzzer =
                        Fuzz.frequencyValues
                            [ ( 0.3, 1 )
                            , ( 0.7, 42 )
                            ]

                    simplifyFuzzer : Fuzzer Int
                    simplifyFuzzer =
                        Fuzz.frequencyValues
                            [ ( 1, 42 )
                            , ( 2, 1 )
                            , ( 3, 999 )
                            ]
                 in
                 [ canGenerate 1 fuzzer
                 , canGenerate 42 fuzzer
                 , cannotGenerateSatisfying "not in the range"
                    fuzzer
                    (\n -> not <| List.member n [ 1, 42 ])
                 , passes "One value -> picks it"
                    (Fuzz.frequencyValues [ ( 0.7, 42 ) ])
                    (\n -> n == 42)
                 , simplifiesTowards "simplest" 42 simplest simplifyFuzzer
                 , simplifiesTowards "next simplest" 1 (\x -> x == 42) simplifyFuzzer

                 -- TODO rejects: empty list
                 -- TODO rejects: zero or negative weight
                 ]
                )
            , describe "frequency"
                (let
                    fuzzer : Fuzzer Int
                    fuzzer =
                        Fuzz.frequency
                            [ ( 0.3, Fuzz.intRange -2 0 )
                            , ( 0.7, Fuzz.constant 2 )
                            ]

                    simplifyFuzzer : Fuzzer Int
                    simplifyFuzzer =
                        Fuzz.frequency
                            [ ( 1, Fuzz.constant 42 )
                            , ( 2, Fuzz.constant 1 )
                            , ( 3, Fuzz.constant 999 )
                            ]
                 in
                 [ canGenerate -2 fuzzer
                 , canGenerate -1 fuzzer
                 , canGenerate 0 fuzzer
                 , canGenerate 2 fuzzer
                 , cannotGenerateSatisfying "not in the range"
                    fuzzer
                    (\n -> not <| List.member n [ -2, -1, 0, 2 ])
                 , passes "One fuzzer -> picks it"
                    (Fuzz.frequency [ ( 0.7, Fuzz.constant 42 ) ])
                    (\n -> n == 42)
                 , simplifiesTowards "simplest" 42 simplest simplifyFuzzer
                 , simplifiesTowards "next simplest" 1 (\x -> x == 42) simplifyFuzzer

                 -- TODO rejects: empty list
                 -- TODO rejects: zero or negative weight
                 ]
                )
            , describe "list"
                [ canGenerate [] (Fuzz.list Fuzz.unit)
                , canGenerateSatisfying "non-empty list"
                    (Fuzz.list Fuzz.unit)
                    (not << List.isEmpty)
                , simplifiesTowards "simplest" [] simplest (Fuzz.list Fuzz.int)
                , simplifiesTowards "next simplest" [ 0 ] (\x -> x == []) (Fuzz.list Fuzz.int)
                ]
            , describe "listOfLength"
                [ passes "always length 3"
                    (Fuzz.listOfLength 3 Fuzz.unit)
                    (\list -> List.length list == 3)
                , passes "negative length -> empty list"
                    (Fuzz.listOfLength -3 Fuzz.unit)
                    List.isEmpty
                , simplifiesTowards "simplest" [ 0, 0, 0 ] simplest (Fuzz.listOfLength 3 Fuzz.int)
                , simplifiesTowards "next simplest" [ 0, 0, 1 ] (\x -> x == [ 0, 0, 0 ]) (Fuzz.listOfLength 3 Fuzz.int)
                ]
            , describe "listOfLengthBetween"
                [ passes "always in range"
                    (Fuzz.listOfLengthBetween 2 5 Fuzz.unit)
                    (\list ->
                        let
                            length =
                                List.length list
                        in
                        length >= 2 && length <= 5
                    )
                , simplifiesTowards "simplest" [ 0, 0 ] simplest (Fuzz.listOfLengthBetween 2 5 Fuzz.int)
                , simplifiesTowards "next simplest" [ 0, 1 ] (\x -> x == [ 0, 0 ]) (Fuzz.listOfLengthBetween 2 5 Fuzz.int)
                ]
            , describe "array"
                [ canGenerate Array.empty (Fuzz.array Fuzz.unit)
                , canGenerateSatisfying "non-empty array"
                    (Fuzz.array Fuzz.unit)
                    (not << Array.isEmpty)
                , simplifiesTowards "simplest" Array.empty simplest (Fuzz.array Fuzz.int)
                , simplifiesTowards "next simplest" (Array.fromList [ 0 ]) (\x -> Array.isEmpty x) (Fuzz.array Fuzz.int)
                ]
            , describe "andThen"
                [ passes "integer defined by another integer"
                    (Fuzz.intRange 0 5
                        |> Fuzz.andThen
                            (\m ->
                                Fuzz.pair
                                    ( Fuzz.constant m
                                    , Fuzz.intRange m (m + 10)
                                    )
                            )
                    )
                    (\( m, n ) -> m <= n && n <= m + 10)
                ]
            , describe "weightedBool"
                [ canGenerate False (Fuzz.weightedBool 0.5)
                , canGenerate True (Fuzz.weightedBool 0.5)
                , passes "0 = always False"
                    (Fuzz.weightedBool 0)
                    (\bool -> bool == False)
                , passes "1 = always True"
                    (Fuzz.weightedBool 1)
                    (\bool -> bool == True)
                , passes "<0 clamps to 0"
                    (Fuzz.weightedBool -0.5)
                    (\bool -> bool == False)
                , passes ">1 clamps to 1"
                    (Fuzz.weightedBool 1.5)
                    (\bool -> bool == True)
                , simplifiesTowards "simplest" False simplest (Fuzz.weightedBool 0.5)
                , simplifiesTowards "non-False" True (\x -> x == False) (Fuzz.weightedBool 0.5)
                ]
            , describe "float"
                [ cannotGenerateSatisfying "NaN" Fuzz.float isNaN
                , cannotGenerateSatisfying "Infinity" Fuzz.float isInfinite
                , canGenerateSatisfying "negative" Fuzz.float (\f -> f < 0)
                , canGenerateSatisfying "positive" Fuzz.float (\f -> f > 0)
                , simplifiesTowards "simplest" 0 simplest Fuzz.float
                , simplifiesTowards "next simplest" 1 (\x -> x == 0) Fuzz.float
                , simplifiesTowards "simplest non-int"
                    -- TODO ~janiczek: hmmm... should prefer simple fractions first...
                    0.5
                    (\x -> x - toFloat (truncate x) == 0)
                    Fuzz.float
                , simplifiesTowards "simplest negative" -1 (\x -> x >= 0) Fuzz.float
                ]
            , describe "floatRange"
                [ passes "Smaller range"
                    (Fuzz.floatRange -5 5)
                    (\n -> n >= -5 && n <= 5)
                , cannotGenerateSatisfying "Smaller range"
                    (Fuzz.floatRange -5 5)
                    (\n -> n < -5 && n > 5)
                , simplifiesTowards "(-,+) simplest" 0 simplest (Fuzz.floatRange -5 5)
                , simplifiesTowards "(-,+) non-zero" 5.684341896668713e-13 (\n -> n == 0) (Fuzz.floatRange -5 5)
                , simplifiesTowards "(0,+) simplest" 0 simplest (Fuzz.floatRange 0 5)
                , simplifiesTowards "(0,+) non-zero" 5.684341896668713e-13 (\n -> n == 0) (Fuzz.floatRange 0 5)
                , simplifiesTowards "(+,+) simplest" 1 simplest (Fuzz.floatRange 1 5)
                , simplifiesTowards "(+,+) non-low" 1.0000000000004547 (\n -> n == 1) (Fuzz.floatRange 1 5)
                , simplifiesTowards "(-,0) simplest" 0 simplest (Fuzz.floatRange -5 0)
                , simplifiesTowards "(-,0) non-zero" -5.684341896668713e-13 (\n -> n == 0) (Fuzz.floatRange -5 0)
                , simplifiesTowards "(-,-) simplest" -1 simplest (Fuzz.floatRange -5 -1)
                , simplifiesTowards "(-,-) non-high" -1.0000000000004547 (\n -> n == -1) (Fuzz.floatRange -5 -1)

                -- TODO: rejects "Limits out of order (hi < lo)"
                ]
            , todo "filter"
            , todo "invalid"
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
