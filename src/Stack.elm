module Stack exposing
    ( Stack
    , empty
    , isEmpty
    , peek
    , pop
    , popN
    , push
    , size
    , toString
    )


type Stack a
    = Stack (List a)


toString : (a -> String) -> Stack a -> String
toString aToString (Stack entries) =
    case entries of
        [] ->
            "<empty>"

        _ ->
            String.join " " (List.reverse (List.map aToString entries))


empty : Stack a
empty =
    Stack []


isEmpty : Stack a -> Bool
isEmpty (Stack list) =
    List.isEmpty list


size : Stack a -> Int
size (Stack list) =
    List.length list


popN : Int -> Stack a -> Stack a
popN n (Stack list) =
    Stack (List.drop n list)


pop : Stack a -> Maybe ( a, Stack a )
pop (Stack list) =
    case list of
        [] ->
            Nothing

        str :: rest ->
            Just ( str, Stack rest )


push : a -> Stack a -> Stack a
push entry (Stack list) =
    Stack (entry :: list)


peek : Stack a -> Maybe a
peek (Stack list) =
    case list of
        [] ->
            Nothing

        head :: _ ->
            Just head
