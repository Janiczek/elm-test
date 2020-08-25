module ShrinkingChallengeTests exposing (shrinkingChallenges)

import Expect exposing (Expectation)
import Fuzz exposing (..)
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
    -- Given: AST = Int Int | Add AST AST | Div AST AST
    -- Property: if no subterms are `Div _ 0`, evaluating the expression will not trigger zero division error
    -- Shrinks towards: Div (Int 0) (Add 0 0)
    todo "calculator"


{-| <https://github.com/jlink/shrinking-challenge/blob/836bafa664659a435ae186eed5b87e941228ae3d/challenges/lengthlist.md>
-}
lengthList : Test
lengthList =
    -- Given: int 1..100 |> andThen (\len -> listOfLength len (int 0..1000))
    -- Property: max value >= 900
    -- Shrinks towards: [900]
    todo "lengthList"


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
