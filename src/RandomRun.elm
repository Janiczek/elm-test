module RandomRun exposing
    ( Chunk
    , RandomRun
    , append
    , compare
    , deleteChunk
    , empty
    , get
    , isEmpty
    , isInBounds
    , isOverCapacity
    , length
    , nextChoice
    , replace
    , replaceChunkWithZero
    , set
    , sortChunk
    , swapIfOutOfOrder
    , toList
    )

import MicroListExtra as List
import Queue exposing (Queue)


type alias RandomRun =
    Queue Int


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


isOverCapacity : RandomRun -> Bool
isOverCapacity run =
    length run >= maxLength


type alias Chunk =
    { size : Int
    , startIndex : Int
    }


empty : RandomRun
empty =
    Queue.empty


isEmpty : RandomRun -> Bool
isEmpty run =
    Queue.isEmpty run


nextChoice : RandomRun -> Maybe ( Int, RandomRun )
nextChoice run =
    case Queue.dequeue run of
        ( Nothing, _ ) ->
            Nothing

        ( Just first, rest ) ->
            Just ( first, rest )


append : Int -> RandomRun -> RandomRun
append n run =
    Queue.enqueue n run


isInBounds : Chunk -> RandomRun -> Bool
isInBounds { startIndex, size } run =
    startIndex + size <= length run


length : RandomRun -> Int
length run =
    Queue.size run


getChunk : Chunk -> RandomRun -> List Int
getChunk chunk run =
    run
        |> Queue.toList
        |> List.drop chunk.startIndex
        |> List.take chunk.size


deleteChunk : Chunk -> RandomRun -> RandomRun
deleteChunk chunk run =
    let
        list =
            Queue.toList run
    in
    (List.take chunk.startIndex list
        ++ List.drop (chunk.startIndex + chunk.size) list
    )
        |> Queue.fromList


replaceChunkWithZero : Chunk -> RandomRun -> RandomRun
replaceChunkWithZero chunk run =
    -- TODO maybe `replace [...] run` would be faster?
    let
        list =
            Queue.toList run
    in
    List.fastConcat
        [ List.take chunk.startIndex list
        , List.repeat chunk.size 0
        , List.drop (chunk.startIndex + chunk.size) list
        ]
        |> Queue.fromList


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
    replaceInList values (Queue.toList run)


replaceInList : List ( Int, Int ) -> List Int -> RandomRun
replaceInList values list =
    List.foldl
        (\( index, newValue ) accList ->
            List.setAt index newValue accList
        )
        list
        values
        |> Queue.fromList


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
            Queue.toList run
    in
    Maybe.map2
        (\left right ->
            if left > right then
                { newRun =
                    replaceInList
                        [ ( leftIndex, right )
                        , ( rightIndex, left )
                        ]
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
    run
        |> Queue.toList
        |> List.getAt index


set : Int -> Int -> RandomRun -> RandomRun
set index value run =
    run
        |> Queue.toList
        |> List.setAt index value
        |> Queue.fromList


sortKey : RandomRun -> ( Int, List Int )
sortKey run =
    ( length run
    , toList run
    )


compare : RandomRun -> RandomRun -> Order
compare a b =
    Basics.compare (sortKey a) (sortKey b)


toList : RandomRun -> List Int
toList run =
    Queue.toList run
