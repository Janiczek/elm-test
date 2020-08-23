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

import MicroListExtra as List
import Queue exposing (Queue)


type alias RandomRun =
    { data : Queue Int

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
    { data = Queue.empty
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
    case Queue.dequeue run.data of
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
        , data = Queue.enqueue n run.data
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
        |> Queue.toList
        |> List.drop chunk.startIndex
        |> List.take chunk.size


deleteChunk : Chunk -> RandomRun -> RandomRun
deleteChunk chunk run =
    let
        list =
            Queue.toList run.data
    in
    { run
        | length = run.length - chunk.size
        , data =
            (List.take chunk.startIndex list
                ++ List.drop (chunk.startIndex + chunk.size) list
            )
                |> Queue.fromList
    }


replaceChunkWithZero : Chunk -> RandomRun -> RandomRun
replaceChunkWithZero chunk run =
    -- TODO maybe `replace [...] run` would be faster?
    let
        list =
            Queue.toList run.data
    in
    { run
        | data =
            List.fastConcat
                [ List.take chunk.startIndex list
                , List.repeat chunk.size 0
                , List.drop (chunk.startIndex + chunk.size) list
                ]
                |> Queue.fromList
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
    replaceInList values run.length (Queue.toList run.data)


{-| An optimization to not do Queue.toList redundantly.

Expects `list == Queue.toList run.data`

-}
replaceInList : List ( Int, Int ) -> Int -> List Int -> RandomRun
replaceInList values len list =
    { length = len
    , data =
        List.foldl
            (\( index, newValue ) accList ->
                List.setAt index newValue accList
            )
            list
            values
            |> Queue.fromList
    }


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
    let
        list =
            Queue.toList run.data
    in
    Maybe.map2
        (\left right ->
            if left > right then
                { newRun =
                    replaceInList
                        [ ( leftIndex, right )
                        , ( rightIndex, left )
                        ]
                        run.length
                        list
                , newLeftValue = right
                , newRightValue = left
                }

            else
                { newRun = run
                , newLeftValue = left
                , newRightValue = right
                }
        )
        (List.getAt leftIndex list)
        (List.getAt rightIndex list)


get : Int -> RandomRun -> Maybe Int
get index run =
    run.data
        |> Queue.toList
        |> List.getAt index


set : Int -> Int -> RandomRun -> RandomRun
set index value run =
    { run
        | data =
            run.data
                |> Queue.toList
                |> List.setAt index value
                |> Queue.fromList
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
    Queue.toList run.data


update : Int -> (Int -> Int) -> RandomRun -> RandomRun
update index fn run =
    case get index run of
        Nothing ->
            run

        Just value ->
            replace [ ( index, fn value ) ] run


equal : RandomRun -> RandomRun -> Bool
equal run1 run2 =
    toList run1 == toList run2
