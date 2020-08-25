module RandomRun exposing
    ( Chunk
    , RandomRun
    , append
    , compare
    , deleteChunk
    , empty
    , equal
    , get
    , isEmpty
    , isFull
    , isInBounds
    , length
    , nextChoice
    , replace
    , replaceChunkWithZero
    , set
    , sortChunk
    , swapIfOutOfOrder
    , toList
    , update
    )

import Deque exposing (Deque)


type alias RandomRun =
    { data : Deque Int

    -- derived precomputed data:
    , length : Int
    }


{-| A cap for the maximum amount of entropy a fuzzer can use.
This stops infinite recursion (in cases where each step of the recursion makes
a PRNG choice), like in:

    infiniteList : Fuzzer a -> Fuzzer (List a)
    infiniteList itemFuzzer =
        let
            go accList =
                itemFuzzer
                    |> Fuzz.andThen (\item -> go (item :: accList))
        in
        go []

-}
maxLength : Int
maxLength =
    8196


type alias Chunk =
    { size : Int
    , startIndex : Int
    }


empty : RandomRun
empty =
    { data = Deque.empty
    , length = 0
    }


isEmpty : RandomRun -> Bool
isEmpty run =
    run.length == 0


isFull : RandomRun -> Bool
isFull run =
    run.length == maxLength


nextChoice : RandomRun -> Maybe ( Int, RandomRun )
nextChoice run =
    case Deque.popFront run.data of
        ( Nothing, _ ) ->
            Nothing

        ( Just first, rest ) ->
            Just
                ( first
                , { run
                    | length = run.length - 1
                    , data = rest
                  }
                )


append : Int -> RandomRun -> RandomRun
append n run =
    { run
        | length = run.length + 1
        , data = Deque.pushBack n run.data
    }


isInBounds : Chunk -> RandomRun -> Bool
isInBounds { startIndex, size } run =
    startIndex + size <= run.length


length : RandomRun -> Int
length run =
    run.length


getChunk : Chunk -> RandomRun -> List Int
getChunk chunk run =
    run.data
        |> Deque.dropLeft chunk.startIndex
        |> Deque.left chunk.size
        |> Deque.toList


deleteChunk : Chunk -> RandomRun -> RandomRun
deleteChunk chunk run =
    { run
        | length = run.length - chunk.size
        , data =
            Deque.append
                (Deque.left chunk.startIndex run.data)
                (Deque.dropLeft (chunk.startIndex + chunk.size) run.data)
    }


replaceChunkWithZero : Chunk -> RandomRun -> RandomRun
replaceChunkWithZero chunk run =
    -- TODO maybe `replace [...] run` would be faster?
    { run
        | data =
            Deque.append
                (Deque.left chunk.startIndex run.data)
                (Deque.append
                    (Deque.repeat chunk.size 0)
                    (Deque.dropLeft (chunk.startIndex + chunk.size) run.data)
                )
    }


sortChunk : Chunk -> RandomRun -> RandomRun
sortChunk chunk run =
    let
        sortedIndexed : List ( Int, Int )
        sortedIndexed =
            run
                |> getChunk chunk
                |> List.sort
                |> List.indexedMap
                    (\i value -> ( chunk.startIndex + i, value ))
    in
    replace sortedIndexed run


replace : List ( Int, Int ) -> RandomRun -> RandomRun
replace values run =
    List.foldl
        (\( index, newValue ) accRun ->
            set index newValue accRun
        )
        run
        values


swapIfOutOfOrder :
    { leftIndex : Int, rightIndex : Int }
    -> RandomRun
    ->
        Maybe
            { newRun : RandomRun
            , newLeftValue : Int
            , newRightValue : Int
            }
swapIfOutOfOrder { leftIndex, rightIndex } run =
    Maybe.map2
        (\left right ->
            if left > right then
                { newRun =
                    replace
                        [ ( leftIndex, right )
                        , ( rightIndex, left )
                        ]
                        run
                , newLeftValue = right
                , newRightValue = left
                }

            else
                { newRun = run
                , newLeftValue = left
                , newRightValue = right
                }
        )
        (get leftIndex run)
        (get rightIndex run)


get : Int -> RandomRun -> Maybe Int
get index run =
    run.data
        |> Deque.dropLeft index
        |> Deque.first


set : Int -> Int -> RandomRun -> RandomRun
set index value run =
    { run
        | data =
            Deque.append
                (Deque.left index run.data
                    |> Deque.pushBack value
                )
                (Deque.dropLeft (index + 1) run.data)
    }


sortKey : RandomRun -> ( Int, List Int )
sortKey run =
    ( run.length
    , toList run
    )


compare : RandomRun -> RandomRun -> Order
compare a b =
    Basics.compare (sortKey a) (sortKey b)


toList : RandomRun -> List Int
toList run =
    Deque.toList run.data


update : Int -> (Int -> Int) -> RandomRun -> RandomRun
update index fn run =
    case get index run of
        Nothing ->
            run

        Just value ->
            let
                newValue =
                    fn value
            in
            if newValue < 0 then
                run

            else
                replace [ ( index, fn value ) ] run


equal : RandomRun -> RandomRun -> Bool
equal run1 run2 =
    toList run1 == toList run2
