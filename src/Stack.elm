module Stack exposing (Stack, empty, isEmpty, pop, popN, push, size, toString)


type Stack
    = Stack (List String)


toString : Stack -> String
toString (Stack entries) =
    case entries of
        [] ->
            "<empty>"

        _ ->
            String.join " " (List.reverse entries)


empty : Stack
empty =
    Stack []


isEmpty : Stack -> Bool
isEmpty (Stack list) =
    List.isEmpty list


size : Stack -> Int
size (Stack list) =
    List.length list


popN : Int -> Stack -> Stack
popN n (Stack list) =
    Stack (List.drop n list)


pop : Stack -> Maybe ( String, Stack )
pop (Stack list) =
    case list of
        [] ->
            Nothing

        str :: rest ->
            Just ( str, Stack rest )


push : String -> Stack -> Stack
push entry (Stack list) =
    Stack (entry :: list)
