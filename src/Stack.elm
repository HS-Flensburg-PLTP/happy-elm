module Stack exposing (Stack, empty, popN, push, size, toString)


type Stack
    = Stack (List String)


toString : Stack -> String
toString (Stack entries) =
    case entries of
        [] ->
            "<empty>"

        _ ->
            String.join " " entries


empty : Stack
empty =
    Stack []


size : Stack -> Int
size (Stack list) =
    List.length list


popN : Int -> Stack -> Stack
popN n (Stack list) =
    Stack (List.drop n list)


push : String -> Stack -> Stack
push entry (Stack list) =
    Stack (entry :: list)
