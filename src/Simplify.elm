module Simplify exposing (State, simplify)

{-| TODO docs
-}

import Fuzz.Internal exposing (Fuzzer)
import GenResult exposing (GenResult(..))
import PRNG
import RandomRun exposing (Chunk, RandomRun)
import Simplify.Cmd exposing (SimplifyCmd(..))
import Test.Expectation exposing (Expectation(..))


type alias State a =
    { getExpectation : a -> Expectation
    , fuzzer : Fuzzer a
    , value : a
    , randomRun : RandomRun
    }


{-| TODO ~janiczek: perhaps we can just return `a` here
-}
simplify : State a -> ( a, RandomRun )
simplify state =
    if RandomRun.isEmpty state.randomRun then
        -- We can't do any better
        ( state.value, state.randomRun )

    else
        simplifyWhileProgress state


simplifyWhileProgress : State a -> ( a, RandomRun )
simplifyWhileProgress state =
    let
        _ =
            Debug.log
                "[SIMPLIFY] simplifyWhileProgress"
                ( RandomRun.toList state.randomRun, state.value )
    in
    let
        nextState =
            simplifyOnce state
    in
    if nextState.randomRun == state.randomRun then
        ( nextState.value, nextState.randomRun )

    else
        simplifyWhileProgress nextState


simplifyOnce : State a -> State a
simplifyOnce state =
    runCmds
        (Simplify.Cmd.cmdsForRun state.randomRun)
        state


runCmds : List SimplifyCmd -> State a -> State a
runCmds cmds state =
    List.foldl runCmd state cmds


runCmd : SimplifyCmd -> State a -> State a
runCmd cmd state =
    case cmd of
        DeleteChunkAndMaybeDecrementPrevious chunk ->
            deleteChunk chunk state

        ReplaceChunkWithZero chunk ->
            replaceChunkWithZero chunk state

        SortChunk chunk ->
            sortChunk chunk state

        MinimizeChoice options ->
            minimizeChoice options state

        RedistributeChoices options ->
            redistribute options state


{-| Tries the new RandomRun with the given fuzzer and test fn, and if the run
generates a value which fails the test, save it as the currently best
counterexample.
-}
keepIfStillFails :
    RandomRun
    -> State a
    ->
        { stillFails : Bool
        , newState : State a
        }
keepIfStillFails newRandomRun state =
    let
        nope () =
            { stillFails = False
            , newState = state
            }
    in
    if state.randomRun == newRandomRun then
        nope ()

    else
        case Fuzz.Internal.generate (PRNG.hardcoded newRandomRun) state.fuzzer of
            Generated { value } ->
                case state.getExpectation value of
                    Pass ->
                        nope ()

                    Fail _ ->
                        if RandomRun.compare state.randomRun newRandomRun == GT then
                            { stillFails = True
                            , newState =
                                { state
                                    | value = value
                                    , randomRun = newRandomRun
                                }
                            }

                        else
                            nope ()

            Rejected _ ->
                nope ()



-- SIMPLIFY CMD IMPLEMENTATIONS


deleteChunk : Chunk -> State a -> State a
deleteChunk chunk state =
    if RandomRun.isInBounds chunk state.randomRun then
        let
            simplifiedRun : RandomRun
            simplifiedRun =
                RandomRun.deleteChunk chunk state.randomRun

            { newState } =
                keepIfStillFails simplifiedRun state
        in
        newState

    else
        state


replaceChunkWithZero : Chunk -> State a -> State a
replaceChunkWithZero chunk state =
    if RandomRun.isInBounds chunk state.randomRun then
        let
            simplifiedRun : RandomRun
            simplifiedRun =
                RandomRun.replaceChunkWithZero chunk state.randomRun

            { newState } =
                keepIfStillFails simplifiedRun state
        in
        newState

    else
        state


sortChunk : Chunk -> State a -> State a
sortChunk chunk state =
    if RandomRun.isInBounds chunk state.randomRun then
        let
            simplifiedRun : RandomRun
            simplifiedRun =
                RandomRun.sortChunk chunk state.randomRun

            { newState } =
                keepIfStillFails simplifiedRun state
        in
        newState

    else
        state


minimizeChoice : { index : Int } -> State a -> State a
minimizeChoice { index } state =
    if
        RandomRun.isInBounds
            { startIndex = index
            , size = 1
            }
            state.randomRun
    then
        case RandomRun.get index state.randomRun of
            Nothing ->
                state

            Just value ->
                binarySearchShrink
                    { low = 0
                    , high = value
                    , state = state
                    , updateRun =
                        \value_ accRun ->
                            RandomRun.set index value_ accRun
                    }

    else
        state


redistribute : { leftIndex : Int, rightIndex : Int } -> State a -> State a
redistribute options state =
    if
        RandomRun.isInBounds
            { startIndex = options.leftIndex
            , size = options.rightIndex - options.leftIndex + 1
            }
            state.randomRun
    then
        {- First we try swapping them if left > right.

           Then we try to (binary-search) minimize the left while keeping the
           sum constant (so what we subtract from left we add to right).
        -}
        case RandomRun.swapIfOutOfOrder options state.randomRun of
            Nothing ->
                state

            Just { newRun, newLeftValue, newRightValue } ->
                let
                    { newState } =
                        keepIfStillFails newRun state
                in
                binarySearchShrink
                    { low = 0
                    , high = newLeftValue
                    , state = newState
                    , updateRun =
                        \value accRun ->
                            RandomRun.replace
                                [ ( options.leftIndex, value )
                                , ( options.rightIndex, newRightValue + newLeftValue - value )
                                ]
                                accRun
                    }

    else
        state



-- BINARY SEARCH SHRINKING


type alias BinarySearchOptions a =
    { low : Int
    , high : Int
    , state : State a
    , updateRun : Int -> RandomRun.RandomRun -> RandomRun.RandomRun
    }


binarySearchShrink : BinarySearchOptions a -> State a
binarySearchShrink ({ updateRun, low, state } as options) =
    let
        -- Let's try the best case first
        runWithLow =
            updateRun low options.state.randomRun

        { stillFails, newState } =
            keepIfStillFails runWithLow state
    in
    if stillFails then
        -- We can't do any better
        newState

    else
        binarySearchLoop options


binarySearchLoop : BinarySearchOptions a -> State a
binarySearchLoop ({ low, high, state, updateRun } as options) =
    if low + 1 < high then
        let
            mid =
                -- `(low + high) // 2` would cause integer overflow
                low + (high - low) // 2

            newRun =
                updateRun mid options.state.randomRun

            { stillFails, newState } =
                keepIfStillFails newRun state

            optionsWithNewRange =
                if stillFails then
                    { options | high = mid }

                else
                    { options | low = mid }

            newOptions =
                { optionsWithNewRange | state = newState }
        in
        binarySearchLoop newOptions

    else
        options.state
