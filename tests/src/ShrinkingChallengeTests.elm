module ShrinkingChallengeTests exposing (shrinkingChallenges)

import Expect exposing (Expectation)
import Fuzz exposing (..)
import Helpers exposing (..)
import Set
import Test exposing (..)


{-| <https://github.com/jlink/shrinking-challenge>
-}
shrinkingChallenges : Test
shrinkingChallenges =
    describe "Shrinking challenges"
        [ reverse
        , largeUnionList
        , bound5
        , calculator
        , lengthList
        , difference1
        , difference2
        , difference3
        , binHeap
        , coupling
        , deletion
        , distinct
        , nestedLists
        ]


{-| <https://github.com/jlink/shrinking-challenge/blob/836bafa664659a435ae186eed5b87e941228ae3d/challenges/reverse.md>
-}
reverse : Test
reverse =
    simplifiesTowardsWith { runs = 1000 }
        "reverse"
        [ 0, 1 ]
        (Fuzz.list Fuzz.int)
        (\list -> list == List.reverse list)


{-| <https://github.com/jlink/shrinking-challenge/blob/836bafa664659a435ae186eed5b87e941228ae3d/challenges/large_union_list.md>
-}
largeUnionList : Test
largeUnionList =
    simplifiesTowards
        "large union list"
        [ [ 0, 1, 2, 3, 4 ] ]
        (Fuzz.list (Fuzz.list Fuzz.int))
        (\lists -> Set.size (Set.fromList (List.concat lists)) <= 4)


{-| <https://github.com/jlink/shrinking-challenge/blob/836bafa664659a435ae186eed5b87e941228ae3d/challenges/bound5.md>
-}
bound5 : Test
bound5 =
    -- Given: 5-tuple of 16bit ints
    -- Property: if each list sums to < 256, sum of all values < 5*256
    -- Shrinks towards: ([-32768],[-1],[],[],[])
    todo "bound5"


{-| <https://github.com/jlink/shrinking-challenge/blob/836bafa664659a435ae186eed5b87e941228ae3d/challenges/calculator.md>
-}
calculator : Test
calculator =
    let
        exprFuzzer : Int -> Fuzzer CalcExpr
        exprFuzzer maxDepth =
            if maxDepth <= 0 then
                Fuzz.map Int Fuzz.int

            else
                Fuzz.lazy
                    (\() ->
                        let
                            subExprFuzzer =
                                exprFuzzer (maxDepth - 1)
                        in
                        Fuzz.oneOf
                            [ Fuzz.map Int Fuzz.int
                            , Fuzz.map2 Add subExprFuzzer subExprFuzzer
                            , Fuzz.map2 Div subExprFuzzer subExprFuzzer
                            ]
                    )

        noDivisionByLiteralZero : CalcExpr -> Bool
        noDivisionByLiteralZero expr =
            case expr of
                Div _ (Int 0) ->
                    False

                Int _ ->
                    True

                Add a b ->
                    noDivisionByLiteralZero a
                        && noDivisionByLiteralZero b

                Div a b ->
                    noDivisionByLiteralZero a
                        && noDivisionByLiteralZero b

        eval : CalcExpr -> Maybe Int
        eval expr =
            case expr of
                Int i ->
                    Just i

                Add a b ->
                    Maybe.map2 (+)
                        (eval a)
                        (eval b)

                Div a b ->
                    Maybe.map2
                        (\a_ b_ ->
                            if b_ == 0 then
                                Nothing

                            else
                                Just <| a_ // b_
                        )
                        (eval a)
                        (eval b)
                        |> Maybe.andThen identity
    in
    simplifiesTowards
        "calculator"
        (Div (Int 0) (Add (Int 0) (Int 0)))
        (exprFuzzer 5 |> Fuzz.filter noDivisionByLiteralZero)
        (\expr -> eval expr /= Nothing)


type CalcExpr
    = Int Int
    | Add CalcExpr CalcExpr
    | Div CalcExpr CalcExpr


{-| <https://github.com/jlink/shrinking-challenge/blob/836bafa664659a435ae186eed5b87e941228ae3d/challenges/lengthlist.md>
-}
lengthList : Test
lengthList =
    simplifiesTowards
        "lengthList"
        [ 900 ]
        (Fuzz.intRange 1 100
            |> Fuzz.andThen
                (\len -> Fuzz.listOfLength len (Fuzz.intRange 0 1000))
        )
        (\list ->
            case List.maximum list of
                Nothing ->
                    Debug.todo "shouldn't have generated an empty list"

                Just max ->
                    max < 900
        )


{-| <https://github.com/jlink/shrinking-challenge/blob/836bafa664659a435ae186eed5b87e941228ae3d/challenges/difference.md>
-}
difference1 : Test
difference1 =
    -- Given: two positive integers (x and y)
    -- Property: x < 10 || x - y == 0
    -- Shrinks towards: [10, 10]
    todo "difference1"


{-| <https://github.com/jlink/shrinking-challenge/blob/836bafa664659a435ae186eed5b87e941228ae3d/challenges/difference.md>
-}
difference2 : Test
difference2 =
    -- Given: two positive integers (x and y)
    -- Property: x < 10 || (x - y) is between 1 and 4
    -- Shrinks towards: [10, 6]
    todo "difference2"


{-| <https://github.com/jlink/shrinking-challenge/blob/836bafa664659a435ae186eed5b87e941228ae3d/challenges/difference.md>
-}
difference3 : Test
difference3 =
    -- Given: two positive integers (x and y)
    -- Property: x < 10 || x - y == 1
    -- Shrinks towards: [10, 9]
    todo "difference3"


{-| <https://github.com/jlink/shrinking-challenge/blob/836bafa664659a435ae186eed5b87e941228ae3d/challenges/binheap.md>
-}
binHeap : Test
binHeap =
    -- Given: Heap = Heap Int (Maybe Heap) (Maybe Heap)
    -- Property: \heap ->
    --   let
    --     l1 = toList heap
    --     l2 = wrongToSortedList heap
    --   in
    --   (l2 == List.sort l2) && (List.sort l1 == l2)
    -- Shrinks towards:
    --   Heap 0
    --     Nothing
    --     (Heap 0
    --        (Heap 0 Nothing Nothing)
    --        (Heap 1 Nothing Nothing)
    --     )
    todo "binHeap"


{-| <https://github.com/jlink/shrinking-challenge/blob/836bafa664659a435ae186eed5b87e941228ae3d/challenges/coupling.md>
-}
coupling : Test
coupling =
    -- Given: list of ints 0..10 |> filter (\l -> List.all (\i -> i < List.length l) l)
    -- Property: for all indices i, if i != l[i] then i != l[l[i]]
    -- Shrinks towards: [1, 0]
    todo "coupling"


{-| <https://github.com/jlink/shrinking-challenge/blob/836bafa664659a435ae186eed5b87e941228ae3d/challenges/deletion.md>
-}
deletion : Test
deletion =
    -- Given: list of integers
    -- Property: not (List.member x (List.removeFirst x list))
    -- Shrinks towards: ([0, 0], 0)
    todo "deletion"


{-| <https://github.com/jlink/shrinking-challenge/blob/836bafa664659a435ae186eed5b87e941228ae3d/challenges/distinct.md>
-}
distinct : Test
distinct =
    -- Given: list of ints
    -- Property: doesn't contain 3 or more distinct elements
    -- Shrinks towards: [0, 1, 2]
    todo "distinct"


{-| <https://github.com/jlink/shrinking-challenge/blob/836bafa664659a435ae186eed5b87e941228ae3d/challenges/nestedlists.md>
-}
nestedLists : Test
nestedLists =
    -- Given: list of lists of ints
    -- Property: sum of lengths <= 10
    -- Shrinks towards: [[0,0,0,0,0,0,0,0,0,0,0]]
    todo "nestedLists"
