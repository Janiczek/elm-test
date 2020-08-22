module MicroListExtra exposing (fastConcat, fastConcatMap, getAt, setAt)


getAt : Int -> List a -> Maybe a
getAt index list =
    list
        |> List.drop index
        |> List.head


setAt : Int -> a -> List a -> List a
setAt index value list =
    List.take index list
        ++ value
        :: List.drop (index + 1) list


fastConcat : List (List a) -> List a
fastConcat =
    List.foldr (++) []


fastConcatMap : (a -> List b) -> List a -> List b
fastConcatMap f =
    List.foldr (f >> (++)) []
