module Test.Runner
    exposing
        ( Runnable
        , Runner(..)
        , SeededRunners(..)
        , run
        , fromTest
        , getFailure
        , isTodo
        , formatLabels
        , Shrinkable
        , fuzz
        , shrink
        )

{-| This is an "experts only" module that exposes functions needed to run and
display tests. A typical user will use an existing runner library for Node or
the browser, which is implemented using this interface. A list of these runners
can be found in the `README`.

## Runner

@docs Runner, SeededRunners, fromTest

## Runnable

@docs Runnable, run

## Expectations

@docs getFailure, isTodo

## Formatting

@docs formatLabels

## Fuzzers
These functions give you the ability to run fuzzers separate of running fuzz tests.

@docs Shrinkable, fuzz, shrink
-}

import Test exposing (Test)
import Test.Internal as Internal
import Test.Expectation
import Test.Message exposing (failureMessage)
import RoseTree exposing (RoseTree(Rose))
import Lazy.List as LazyList exposing (LazyList)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Fuzz.Internal
import Random.Pcg
import String


{-| An unevaluated test. Run it with [`run`](#run) to evaluate it into a
list of `Expectation`s.
-}
type Runnable
    = Thunk (() -> List Expectation)


{-| A structured test runner, incorporating:

* The expectations to run
* The hierarchy of description strings that describe the results
-}
type Runner
    = Runnable Runnable
    | Labeled String Runner
    | Batch (List Runner)


{-| Evaluate a [`Runnable`](#Runnable) to get a list of `Expectation`s.
-}
run : Runnable -> List Expectation
run (Thunk fn) =
    fn ()


{-| Convert a `Test` into a `Runner`.

In order to run any fuzz tests that the `Test` may have, it requires a default run count as well
as an initial `Random.Pcg.Seed`. `100` is a good run count. To obtain a good random seed, pass a
random 32-bit integer to `Random.Pcg.initialSeed`. You can obtain such an integer by running
`Math.floor(Math.random()*0xFFFFFFFF)` in Node. It's typically fine to hard-code this value into
your Elm code; it's easy and makes your tests reproducible.
-}
fromTest : Int -> Random.Pcg.Seed -> Test -> SeededRunners
fromTest runs seed test =
    if runs < 1 then
        Invalid ("Test runner run count must be at least 1, not " ++ toString runs)
    else
        let
            distribution =
                distributeSeeds runs seed test
        in
            if List.isEmpty distribution.only then
                case countAllRunnables distribution.skipped of
                    0 ->
                        Plain distribution.all

                    skipped ->
                        Skipping skipped distribution.all
            else
                Only distribution.only


countAllRunnables : List Runner -> Int
countAllRunnables =
    List.foldl (countRunnables >> (+)) 0


countRunnables : Runner -> Int
countRunnables runnable =
    case runnable of
        Runnable runnable ->
            1

        Labeled _ runner ->
            countRunnables runner

        Batch runners ->
            countAllRunnables runners


type alias Distribution =
    { seed : Random.Pcg.Seed
    , only : List Runner
    , all : List Runner
    , skipped : List Runner
    }


{-| -}
type SeededRunners
    = Plain (List Runner)
    | Only (List Runner)
    | Skipping Int (List Runner)
    | Invalid String


emptyDistribution : Random.Pcg.Seed -> Distribution
emptyDistribution seed =
    { seed = seed
    , all = []
    , only = []
    , skipped = []
    }


{-| This breaks down a test into individual Runners, while assigning different
random number seeds to them. Along the way it also does a few other things:

1. Collect any tests created with `Test.only` so later we can run only those.
2. Collect any tests created with `Test.todo` so later we can fail the run.
3. Validate that the run count is at least 1.

Some design notes:

1. `only` tests and `skip` tests do not affect seed distribution. This is
important for the case where a user runs tests, sees one failure, and decides
to isolate it by using both `only` and providing the same seed as before. If
`only` changes seed distribution, then that test result might not reproduce!
This would be very frustrating, as it would mean you could reproduce the
failure when not using `only`, but it magically disappeared as soon as you
tried to isolate it. The same logic applies to `skip`.

2. Theoretically this could become tail-recursive. However, the Labeled and Batch
cases would presumably become very gnarly, and it's unclear whether there would
be a performance benefit or penalty in the end. If some brave soul wants to
attempt it for kicks, beware that this is not a performance optimization for
the faint of heart. Practically speaking, it seems unlikely to be worthwhile
unless somehow people start seeing stack overflows during seed distribution -
which would presumably require some absurdly deeply nested `describe` calls.
-}
distributeSeeds : Int -> Random.Pcg.Seed -> Test -> Distribution
distributeSeeds runs seed test =
    case test of
        Internal.Test run ->
            let
                ( firstSeed, nextSeed ) =
                    Random.Pcg.step Random.Pcg.independentSeed seed
            in
                { seed = nextSeed
                , all = [ Runnable (Thunk (\() -> run firstSeed runs)) ]
                , only = []
                , skipped = []
                }

        Internal.Labeled description subTest ->
            let
                next =
                    distributeSeeds runs seed subTest
            in
                { seed = next.seed
                , all = List.map (Labeled description) next.all
                , only = List.map (Labeled description) next.only
                , skipped = List.map (Labeled description) next.skipped
                }

        Internal.Skipped subTest ->
            let
                -- Go through the motions in order to obtain the seed, but then
                -- move everything to skipped.
                next =
                    distributeSeeds runs seed subTest
            in
                { seed = next.seed
                , all = []
                , only = []
                , skipped = next.all
                }

        Internal.Only subTest ->
            let
                next =
                    distributeSeeds runs seed subTest
            in
                -- `only` all the things!
                { next | only = next.all }

        Internal.Batch tests ->
            List.foldl (batchDistribute runs) (emptyDistribution seed) tests


batchDistribute : Int -> Test -> Distribution -> Distribution
batchDistribute runs test prev =
    let
        next =
            distributeSeeds runs prev.seed test
    in
        { seed = next.seed
        , all = prev.all ++ next.all
        , only = prev.only ++ next.only
        , skipped = prev.skipped ++ next.skipped
        }


{-| Return `Nothing` if the given [`Expectation`](#Expectation) is a [`pass`](#pass).

If it is a [`fail`](#fail), return a record containing the failure message,
along with the given inputs if it was a fuzz test. (If no inputs were involved,
the record's `given` field will be `Nothing`).

For example, if a fuzz test generates random integers, this might return
`{ message = "it was supposed to be positive", given = "-1" }`

    getFailure (Expect.fail "this failed")
    -- Just { message = "this failed", given = "" }

    getFailure (Expect.pass)
    -- Nothing
-}
getFailure : Expectation -> Maybe { given : Maybe String, message : String }
getFailure expectation =
    case expectation of
        Test.Expectation.Pass ->
            Nothing

        Test.Expectation.Fail record ->
            Just
                { given = record.given
                , message = failureMessage record
                }


{-| Determine if an expectation was created by a call to `Test.todo`. Runners
may treat these tests differently in their output.
-}
isTodo : Expectation -> Bool
isTodo expectation =
    case expectation of
        Test.Expectation.Pass ->
            False

        Test.Expectation.Fail { reason } ->
            reason == Test.Expectation.TODO


{-| A standard way to format descriptions and test labels, to keep things
consistent across test runner implementations.

The HTML, Node, String, and Log runners all use this.

What it does:

* drop any labels that are empty strings
* format the first label differently from the others
* reverse the resulting list

    [ "the actual test that failed"
    , "nested description failure"
    , "top-level description failure"
    ]
        |> formatLabels ((++) "↓ ") ((++) "✗ ")

    {-
        [ "↓ top-level description failure"
        , "↓ nested description failure"
        , "✗ the actual test that failed"
        ]
    -}

-}
formatLabels :
    (String -> format)
    -> (String -> format)
    -> List String
    -> List format
formatLabels formatDescription formatTest labels =
    case List.filter (not << String.isEmpty) labels of
        [] ->
            []

        test :: descriptions ->
            descriptions
                |> List.map formatDescription
                |> (::) (formatTest test)
                |> List.reverse


type alias Shrunken a =
    { down : LazyList (RoseTree a)
    , over : LazyList (RoseTree a)
    }


{-| A `Shrinkable a` is an opaque type that allows you to obtain a value of type
`a` that is smaller than the one you've previously obtained.
-}
type Shrinkable a
    = Shrinkable (Shrunken a)


{-| Given a fuzzer, return a random generator to produce a value and a
Shrinkable. The value is what a fuzz test would have received as input.
-}
fuzz : Fuzzer a -> Random.Pcg.Generator ( a, Shrinkable a )
fuzz fuzzer =
    Fuzz.Internal.unpackGenTree fuzzer
        |> Random.Pcg.map
            (\(Rose root children) ->
                ( root, Shrinkable { down = children, over = LazyList.empty } )
            )


{-| Given a Shrinkable, attempt to shrink the value further. Pass `False` to
indicate that the last value you've seen (from either `fuzz` or this function)
caused the test to **fail**. This will attempt to find a smaller value. Pass
`True` if the test passed. If you have already seen a failure, this will attempt
to shrink that failure in another way. In both cases, it may be impossible to
shrink the value, represented by `Nothing`.
-}
shrink : Bool -> Shrinkable a -> Maybe ( a, Shrinkable a )
shrink causedPass (Shrinkable { down, over }) =
    let
        tryNext =
            if causedPass then
                over
            else
                down
    in
        case LazyList.headAndTail tryNext of
            Just ( Rose root children, tl ) ->
                Just ( root, Shrinkable { down = children, over = tl } )

            Nothing ->
                Nothing
