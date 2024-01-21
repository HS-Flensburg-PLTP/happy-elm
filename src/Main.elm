module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, table, td, text, textarea, th, tr)
import Html.Attributes exposing (cols, list, placeholder, rows)
import Html.Events exposing (onClick, onInput)
import Parser exposing ((|.), (|=), Parser, Step(..), Trailing(..))
import Set
import Stack exposing (Stack)


type Msg
    = Step
    | ChangeGrammar String
    | Parse


type StateId
    = StateId Int


stateToString : StateId -> String
stateToString (StateId no) =
    String.fromInt no


type RuleId
    = RuleId Int


ruleToString : RuleId -> String
ruleToString (RuleId no) =
    String.fromInt no


type ShiftReduceAction
    = ShiftAndEnter StateId
    | ReduceUsingRule RuleId
    | Accept


type Action
    = ShiftReduce ShiftReduceAction
    | GoTo StateId


actionToString : Action -> String
actionToString action =
    case action of
        ShiftReduce (ShiftAndEnter state) ->
            "shift and enter state " ++ stateToString state

        ShiftReduce (ReduceUsingRule rule) ->
            "reduce using rule " ++ ruleToString rule

        ShiftReduce Accept ->
            "accept"

        GoTo state ->
            "go to state " ++ stateToString state


type alias Model =
    { grammar : String
    , maybeError : Maybe String
    , currentRow : Row
    , table : List Row
    , actions : List Action
    , automaton : Automaton
    }


type alias Automaton =
    { grammar : Dict Int Production
    , states : Dict Int State
    }


type alias Row =
    { state : StateId
    , stack : Stack
    , lookAhead : String
    , unscanned : List String
    }


type alias State =
    { stateId : StateId
    , lookaheads : Dict String ( ShiftReduceAction, List ShiftReduceAction )
    , gotos : Dict String StateId
    }


type Production
    = Production String (List String)



-- Parser


parserDict : Parser ( comparable, value ) -> Parser (Dict comparable value)
parserDict parserKeyValue =
    let
        parserHelp : Dict comparable value -> Parser (Step (Dict comparable value) (Dict comparable value))
        parserHelp dict =
            Parser.oneOf
                [ Parser.succeed (\( no, rule ) -> Loop (Dict.insert no rule dict))
                    |= parserKeyValue
                    |. Parser.symbol "\n"
                , Parser.succeed (Done dict)
                    |. Parser.symbol "\n"
                ]
    in
    Parser.loop Dict.empty parserHelp


parserHeader : String -> Parser ()
parserHeader name =
    Parser.succeed ()
        |. Parser.chompWhile (\c -> c == '-')
        |. Parser.symbol "\n"
        |. Parser.symbol name
        |. Parser.symbol "\n"
        |. Parser.chompWhile (\c -> c == '-')
        |. Parser.symbol "\n"


parserNonTerminal : Parser String
parserNonTerminal =
    Parser.variable
        { start = \c -> Char.isUpper c || c == '%'
        , inner = \c -> Char.isLower c || Char.isUpper c || c == '_'
        , reserved = Set.empty
        }


parserTerminal : Parser String
parserTerminal =
    Parser.oneOf
        [ Parser.succeed (\str -> String.dropRight 1 (String.dropLeft 1 str))
            |= Parser.variable
                { start = \c -> c == '\''
                , inner = \c -> c /= ' '
                , reserved = Set.empty
                }
        , Parser.variable
            { start = \c -> c == 'L'
            , inner = \c -> Char.isLower c || Char.isUpper c || c == '_'
            , reserved = Set.empty
            }
        , Parser.succeed (\_ -> "%eof")
            |= Parser.keyword "%eof"
        , Parser.succeed (\_ -> "'")
            |= Parser.keyword "'\\''"
        ]


type ProductionType
    = Rule
    | Grammar


parserProduction : ProductionType -> Parser ( Int, Production )
parserProduction production =
    let
        parserRHS : List String -> Parser (Step (List String) (List String))
        parserRHS rules =
            Parser.oneOf
                [ Parser.succeed (Loop rules)
                    |. Parser.symbol "."
                    |. Parser.symbol " "
                , Parser.succeed (\rule -> Loop (rule :: rules))
                    |= Parser.oneOf [ parserTerminal, parserNonTerminal ]
                    |. Parser.symbol " "
                , Parser.succeed (Done (List.reverse rules))
                ]
    in
    Parser.succeed (\pre posts ruleId -> ( ruleId, Production pre posts ))
        |. Parser.symbol "\t"
        |= parserNonTerminal
        |. Parser.spaces
        |. Parser.symbol "->"
        |. Parser.symbol " "
        |= Parser.loop [] parserRHS
        |. Parser.spaces
        |= (case production of
                Rule ->
                    Parser.succeed identity
                        |. Parser.symbol "("
                        |. Parser.keyword "rule"
                        |. Parser.spaces
                        |= Parser.int
                        |. Parser.symbol ")"

                Grammar ->
                    Parser.succeed identity
                        |. Parser.symbol "("
                        |= Parser.int
                        |. Parser.symbol ")"
           )


parserInfo : Parser Automaton
parserInfo =
    Parser.succeed Automaton
        |= parserGrammar
        |. parserTerminals
        |. parserNonTerminals
        |= parserStates


parserGrammar : Parser (Dict Int Production)
parserGrammar =
    Parser.succeed identity
        |. parserHeader "Grammar"
        |= parserDict (parserProduction Grammar)


parserTerminals : Parser ()
parserTerminals =
    let
        parserHelp : () -> Parser (Step () ())
        parserHelp _ =
            Parser.oneOf
                [ Parser.succeed (Loop ())
                    |. Parser.symbol "\t"
                    |. parserTerminal
                    |. Parser.spaces
                    |. Parser.symbol "{"
                    |. Parser.chompUntil "}"
                    |. Parser.symbol "}"
                    |. Parser.symbol "\n"
                , Parser.succeed (Done ())
                ]
    in
    Parser.succeed ()
        |. parserHeader "Terminals"
        |. Parser.loop () parserHelp
        |. Parser.symbol "\n"


parserNonTerminals : Parser ()
parserNonTerminals =
    let
        parserHelp : () -> Parser (Step () ())
        parserHelp _ =
            Parser.oneOf
                [ Parser.succeed (Loop ())
                    |. Parser.symbol "\t"
                    |. parserNonTerminal
                    |. Parser.spaces
                    |. Parser.oneOf
                        [ Parser.succeed ()
                            |. Parser.keyword "rule"
                            |. Parser.spaces
                            |. Parser.int
                            |. Parser.symbol "\n"
                        , Parser.succeed ()
                            |. Parser.keyword "rules"
                            |. Parser.sequence
                                { start = ""
                                , separator = ","
                                , end = "\n"
                                , spaces = Parser.succeed ()
                                , item =
                                    Parser.succeed ()
                                        |. Parser.symbol " "
                                        |. Parser.int
                                , trailing = Forbidden
                                }
                        ]
                , Parser.succeed (Done ())
                ]
    in
    Parser.succeed ()
        |. parserHeader "Non-terminals"
        |. Parser.loop () parserHelp
        |. Parser.symbol "\n"


parserStates : Parser (Dict Int State)
parserStates =
    Parser.succeed identity
        |. parserHeader "States"
        |= Parser.map Dict.fromList
            (Parser.sequence
                { start = ""
                , separator = ""
                , end = ""
                , spaces = Parser.spaces
                , item = parserState
                , trailing = Forbidden
                }
            )


parserState : Parser ( Int, State )
parserState =
    Parser.succeed (\stateId lookaheads gotos -> ( stateId, State (StateId stateId) lookaheads gotos ))
        |. Parser.keyword "State"
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces
        |. parserDict (parserProduction Rule)
        |= parserDict parserLookahead
        |= parserDict parserGoto


parserLookahead : Parser ( String, ( ShiftReduceAction, List ShiftReduceAction ) )
parserLookahead =
    Parser.succeed (\( name, action ) conflicts -> ( name, ( action, conflicts ) ))
        |. Parser.symbol "\t"
        |= (parserTerminal
                |> Parser.andThen
                    (\name ->
                        if String.endsWith "shift" name then
                            Parser.succeed (\stateId -> ( String.dropRight 5 name, ShiftAndEnter (StateId stateId) ))
                                |. Parser.symbol ","
                                |. Parser.spaces
                                |. Parser.keyword "and"
                                |. Parser.spaces
                                |. Parser.keyword "enter"
                                |. Parser.spaces
                                |. Parser.keyword "state"
                                |. Parser.spaces
                                |= Parser.int

                        else if String.endsWith "reduce" name then
                            Parser.succeed (\ruleId -> ( String.dropRight 6 name, ReduceUsingRule (RuleId ruleId) ))
                                |. Parser.spaces
                                |. Parser.keyword "using"
                                |. Parser.spaces
                                |. Parser.keyword "rule"
                                |. Parser.spaces
                                |= Parser.int

                        else
                            Parser.succeed (\action -> ( name, action ))
                                |. Parser.spaces
                                |= parserShiftReduceAction
                    )
           )
        |= parserConflicts


parserConflicts : Parser (List ShiftReduceAction)
parserConflicts =
    let
        parserHelp : List ShiftReduceAction -> Parser (Step (List ShiftReduceAction) (List ShiftReduceAction))
        parserHelp actions =
            Parser.oneOf
                [ Parser.succeed (\action -> Loop (action :: actions))
                    |= parserConflict
                , Parser.succeed (Done (List.reverse actions))
                ]
    in
    Parser.loop [] parserHelp
        |> Parser.andThen
            (\list ->
                if List.isEmpty list then
                    Parser.succeed list

                else
                    Parser.succeed list
                        |. Parser.symbol "\n"
            )


parserConflict : Parser ShiftReduceAction
parserConflict =
    Parser.succeed identity
        |. Parser.symbol "\n\t\t\t"
        |. Parser.symbol "("
        |= parserShiftReduceAction
        |. Parser.symbol ")"


parserShiftReduceAction : Parser ShiftReduceAction
parserShiftReduceAction =
    Parser.oneOf
        [ Parser.succeed (\ruleId -> ReduceUsingRule (RuleId ruleId))
            |. Parser.keyword "reduce"
            |. Parser.spaces
            |. Parser.keyword "using"
            |. Parser.spaces
            |. Parser.keyword "rule"
            |. Parser.spaces
            |= Parser.int
        , Parser.succeed (\stateId -> ShiftAndEnter (StateId stateId))
            |. Parser.keyword "shift"
            |. Parser.symbol ","
            |. Parser.spaces
            |. Parser.keyword "and"
            |. Parser.spaces
            |. Parser.keyword "enter"
            |. Parser.spaces
            |. Parser.keyword "state"
            |. Parser.spaces
            |= Parser.int
        , Parser.succeed Accept
            |. Parser.keyword "accept"
        ]


parserGoto : Parser ( String, StateId )
parserGoto =
    Parser.succeed (\name id -> ( name, StateId id ))
        |. Parser.symbol "\t"
        |= (parserNonTerminal
                |> Parser.andThen
                    (\name ->
                        if String.endsWith "goto" name then
                            Parser.succeed (String.dropRight 4 name)

                        else
                            Parser.succeed name
                                |. Parser.spaces
                                |. Parser.keyword "goto"
                    )
           )
        |. Parser.spaces
        |. Parser.keyword "state"
        |. Parser.spaces
        |= Parser.int



-- Application


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeGrammar newInput ->
            { model | grammar = newInput, maybeError = Nothing }

        Parse ->
            case Parser.run parserInfo model.grammar of
                Err deadEnds ->
                    { model | maybeError = Just (Debug.toString deadEnds) }

                Ok automaton ->
                    { model | automaton = automaton, maybeError = Nothing }

        Step ->
            case model.maybeError of
                Just _ ->
                    model

                Nothing ->
                    case model.currentRow.unscanned of
                        [] ->
                            Debug.todo "Implement error handling"

                        token :: _ ->
                            case step token model.currentRow model.automaton of
                                Ok ( newRow, action ) ->
                                    { model
                                        | currentRow = newRow
                                        , table = model.currentRow :: model.table
                                        , actions = action :: model.actions
                                    }

                                Err message ->
                                    { model | maybeError = Just message }


step : String -> Row -> Automaton -> Result String ( Row, Action )
step token row automaton =
    case row.state of
        StateId stateId ->
            case Dict.get stateId automaton.states of
                Nothing ->
                    Err ("State " ++ String.fromInt stateId ++ " not found")

                Just state ->
                    case Dict.get token state.lookaheads of
                        Nothing ->
                            Debug.todo "Implement this case"

                        Just ( (ShiftAndEnter nextStateId) as action, _ ) ->
                            Ok
                                ( { state = nextStateId
                                  , stack = Stack.push token row.stack
                                  , lookAhead = ""
                                  , unscanned = List.drop 1 row.unscanned
                                  }
                                , ShiftReduce action
                                )

                        Just ( (ReduceUsingRule (RuleId ruleId)) as action, _ ) ->
                            case Dict.get ruleId automaton.grammar of
                                Nothing ->
                                    Err ("Rule " ++ String.fromInt ruleId ++ " not found")

                                Just rule ->
                                    Ok
                                        ( { state = row.state
                                          , stack = applyProduction rule row.stack
                                          , lookAhead = ""
                                          , unscanned = row.unscanned
                                          }
                                        , ShiftReduce action
                                        )

                        Just ( Accept, _ ) ->
                            Ok ( row, ShiftReduce Accept )


applyProduction : Production -> Stack -> Stack
applyProduction (Production pre posts) stack =
    let
        postsSize =
            List.length posts
    in
    if Stack.size stack >= postsSize then
        Stack.push pre (Stack.popN postsSize stack)

    else
        Debug.todo "Handle this error"


view : Model -> Html Msg
view model =
    div []
        [ textarea
            [ cols 40
            , rows 10
            , placeholder "..."
            , onInput ChangeGrammar
            ]
            []
        , button [ onClick Parse ] [ text "Parse grammar" ]
        , button [ onClick Step ] [ text "Perform step" ]
        , case model.maybeError of
            Nothing ->
                text ""

            Just message ->
                text message
        , viewTable (model.currentRow :: model.table) (Nothing :: List.map Just model.actions)
        , text (Debug.toString model.automaton)
        ]


viewTable : List Row -> List (Maybe Action) -> Html Msg
viewTable rows actions =
    table [] (tableHeader :: List.map2 viewRow (List.reverse rows) (List.reverse actions))


tableHeader : Html Msg
tableHeader =
    tr []
        [ th [] [ text "State" ]
        , th [] [ text "Parse Stack" ]
        , th [] [ text "Look Ahead" ]
        , th [] [ text "Unscanned" ]
        , th [] [ text "Parser Action" ]
        ]


viewRow : Row -> Maybe Action -> Html Msg
viewRow row maybeAction =
    tr []
        [ td [] [ text (stateToString row.state) ]
        , td [] [ text (Stack.toString row.stack) ]
        , td [] [ text row.lookAhead ]
        , td [] [ text (String.join " " row.unscanned) ]
        , td [] [ text (Maybe.withDefault "" (Maybe.map actionToString maybeAction)) ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( { maybeError = Nothing
                  , currentRow =
                        { state = StateId 0
                        , stack = Stack.empty
                        , lookAhead = ""
                        , unscanned =
                            [ "let"
                            , "L_LIDENT"
                            , "="
                            , "L_UIDENT"
                            , "."
                            , "("
                            , "L_LIDENT"
                            , "L_LIDENT"
                            , ")"
                            ]
                        }
                  , table = []
                  , actions = []
                  , automaton =
                        { grammar = Dict.empty
                        , states = Dict.empty
                        }
                  , grammar = ""
                  }
                , Cmd.none
                )
        , subscriptions = \_ -> Sub.none
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        }
