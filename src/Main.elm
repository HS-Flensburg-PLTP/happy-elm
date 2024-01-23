module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, table, td, text, textarea, th, tr)
import Html.Attributes exposing (cols, disabled, list, placeholder, rows)
import Html.Events exposing (onClick, onInput)
import List exposing (head)
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


ruleIdToInt : RuleId -> Int
ruleIdToInt (RuleId no) =
    no


ruleIdToString : RuleId -> String
ruleIdToString (RuleId no) =
    String.fromInt no


type ShiftReduceAction
    = ShiftAndEnter StateId
    | ReduceUsingRule RuleId
    | Accept


type Action
    = Shift StateId
    | Reduce RuleId
    | ReduceAndGoTo RuleId StateId
    | GoTo StateId
    | FinalAccept


actionToString : Action -> String
actionToString action =
    case action of
        Shift state ->
            "shift and enter state " ++ stateToString state

        Reduce rule ->
            "reduce using rule " ++ ruleIdToString rule

        ReduceAndGoTo rule state ->
            "reduce using rule " ++ ruleIdToString rule ++ " and go to state " ++ stateToString state

        GoTo state ->
            "go to state " ++ stateToString state

        FinalAccept ->
            "accept"


type alias Model =
    { grammar : String
    , parseEnabled : Bool
    , maybeError : Maybe String
    , currentRow : Row
    , table : List Row
    , actions : List Action
    , automaton : Maybe Automaton
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
        |. Parser.keyword name
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
        |. parserMainHeader
        |. parserConflicts
        |. parserUnusedRules
        |= parserGrammar
        |. parserTerminals
        |. parserNonTerminals
        |= parserStates
        |. parserGrammarTotals


parserMainHeader : Parser ()
parserMainHeader =
    Parser.succeed ()
        |. Parser.chompWhile (\c -> c == '-')
        |. Parser.symbol "\n"
        |. Parser.chompUntil "\n"
        |. Parser.symbol "\n"
        |. Parser.chompWhile (\c -> c == '-')
        |. Parser.symbol "\n"
        |. Parser.symbol "\n"


parserConflicts : Parser ()
parserConflicts =
    let
        parserConflictType =
            Parser.oneOf
                [ Parser.keyword "shift/reduce"
                , Parser.keyword "reduce/reduce"
                ]
    in
    Parser.loop ()
        (\_ ->
            Parser.oneOf
                [ Parser.succeed (Loop ())
                    |. Parser.keyword "state"
                    |. Parser.spaces
                    |. Parser.int
                    |. Parser.spaces
                    |. Parser.keyword "contains"
                    |. Parser.spaces
                    |. Parser.sequence
                        { start = ""
                        , separator = " and "
                        , end = "."
                        , spaces = Parser.succeed ()
                        , item =
                            Parser.succeed ()
                                |. Parser.int
                                |. Parser.spaces
                                |. parserConflictType
                                |. Parser.spaces
                                |. Parser.keyword "conflicts"
                        , trailing = Forbidden
                        }
                    |. Parser.symbol "\n"
                , Parser.succeed (Done ())
                    |. Parser.symbol "\n"
                ]
        )


parserUnusedRules : Parser ()
parserUnusedRules =
    let
        parserHelp =
            Parser.oneOf
                [ Parser.succeed (Loop ())
                    |. Parser.keyword "rule"
                    |. Parser.spaces
                    |. Parser.int
                    |. Parser.spaces
                    |. Parser.keyword "is"
                    |. Parser.spaces
                    |. Parser.keyword "unused"
                    |. Parser.symbol "\n"
                , Parser.succeed (Done ())
                ]
    in
    Parser.succeed ()
        |. Parser.loop () (\_ -> parserHelp)
        |. Parser.symbol "\n"


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
        |= parserRuleConflicts


parserRuleConflicts : Parser (List ShiftReduceAction)
parserRuleConflicts =
    let
        parserHelp : List ShiftReduceAction -> Parser (Step (List ShiftReduceAction) (List ShiftReduceAction))
        parserHelp actions =
            Parser.oneOf
                [ Parser.succeed (\action -> Loop (action :: actions))
                    |= parserRuleConflict
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


parserRuleConflict : Parser ShiftReduceAction
parserRuleConflict =
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


parserGrammarTotals : Parser ()
parserGrammarTotals =
    let
        parserGrammarTotalsLine category =
            Parser.succeed ()
                |. Parser.keyword "Number"
                |. Parser.spaces
                |. Parser.keyword "of"
                |. Parser.spaces
                |. Parser.keyword category
                |. Parser.symbol ":"
                |. Parser.spaces
                |. Parser.int
                |. Parser.symbol "\n"
    in
    Parser.succeed ()
        |. parserHeader "Grammar Totals"
        |. parserGrammarTotalsLine "rules"
        |. parserGrammarTotalsLine "terminals"
        |. parserGrammarTotalsLine "non-terminals"
        |. parserGrammarTotalsLine "states"



-- Application


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeGrammar newInput ->
            { model | grammar = newInput, parseEnabled = True, maybeError = Nothing }

        Parse ->
            case Parser.run parserInfo model.grammar of
                Err deadEnds ->
                    { model | parseEnabled = False, maybeError = Just (Debug.toString deadEnds) }

                Ok automaton ->
                    { model | parseEnabled = False, automaton = Just automaton, maybeError = Nothing }

        Step ->
            case model.maybeError of
                Just _ ->
                    model

                Nothing ->
                    case model.automaton of
                        Nothing ->
                            { model | maybeError = Just "Performed step with no automaton" }

                        Just automaton ->
                            case step (Maybe.map .state (List.head model.table)) model.currentRow automaton of
                                Ok ( newRow, action ) ->
                                    { model
                                        | currentRow = newRow
                                        , table = model.currentRow :: model.table
                                        , actions = action :: model.actions
                                    }

                                Err message ->
                                    { model | maybeError = Just message }


step : Maybe StateId -> Row -> Automaton -> Result String ( Row, Action )
step maybePreviousStateId currentRow automaton =
    case currentRow.state of
        StateId stateId ->
            case Dict.get stateId automaton.states of
                Nothing ->
                    Err ("State " ++ String.fromInt stateId ++ " not found")

                Just state ->
                    performLookahead maybePreviousStateId automaton.grammar currentRow state


performLookahead : Maybe StateId -> Dict Int Production -> Row -> State -> Result String ( Row, Action )
performLookahead maybePreviousStateId grammar currentRow state =
    case currentRow.unscanned of
        [] ->
            Err "Performed step but there are no unscanned tokens"

        token :: _ ->
            case Dict.get token state.lookaheads of
                Nothing ->
                    case Stack.pop currentRow.stack of
                        Nothing ->
                            Err "Checked lookahead symbol but the stack is empty"

                        Just ( head, _ ) ->
                            case Dict.get head state.gotos of
                                Nothing ->
                                    Err "There is neither a lookahead nor a goto defined in this state"

                                Just newStateId ->
                                    Ok
                                        ( { currentRow | state = newStateId }
                                        , GoTo newStateId
                                        )

                Just ( ShiftAndEnter nextStateId, _ ) ->
                    Ok
                        ( { state = nextStateId
                          , stack = Stack.push token currentRow.stack
                          , lookAhead = ""
                          , unscanned = List.drop 1 currentRow.unscanned
                          }
                        , Shift nextStateId
                        )

                Just ( ReduceUsingRule ruleId, _ ) ->
                    case Dict.get (ruleIdToInt ruleId) grammar of
                        Nothing ->
                            Err ("Rule " ++ ruleIdToString ruleId ++ " not found")

                        Just rule ->
                            case applyProduction rule currentRow.stack of
                                Err message ->
                                    Err message

                                Ok newStack ->
                                    case Stack.pop newStack of
                                        Nothing ->
                                            Err "Applying a rule did not produce an element on the stack"

                                        Just ( head, _ ) ->
                                            case Dict.get head state.gotos of
                                                Nothing ->
                                                    case maybePreviousStateId of
                                                        Nothing ->
                                                            Err "There is no previous state but no goto is defined either"

                                                        Just previousStateId ->
                                                            Ok
                                                                ( { currentRow
                                                                    | state = previousStateId
                                                                    , stack = newStack
                                                                  }
                                                                , Reduce ruleId
                                                                )

                                                Just newStateId ->
                                                    Ok
                                                        ( { currentRow
                                                            | state = newStateId
                                                            , stack = newStack
                                                          }
                                                        , ReduceAndGoTo ruleId newStateId
                                                        )

                Just ( Accept, _ ) ->
                    Ok ( currentRow, FinalAccept )


applyProduction : Production -> Stack -> Result String Stack
applyProduction (Production pre posts) stack =
    case match posts stack of
        Ok newStack ->
            Ok (Stack.push pre newStack)

        (Err _) as res ->
            res


match : List String -> Stack -> Result String Stack
match input stack =
    case input of
        [] ->
            Ok stack

        str :: strs ->
            case Stack.pop stack of
                Nothing ->
                    Err "Stack is empty"

                Just ( head, newStack ) ->
                    if str == head then
                        match strs newStack

                    else
                        Err "Head of stack does not match head of list"


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
        , button [ onClick Parse, disabled (not model.parseEnabled) ] [ text "Parse automaton" ]
        , button [ onClick Step, disabled (model.automaton == Nothing) ] [ text "Perform step" ]
        , case model.maybeError of
            Nothing ->
                text ""

            Just message ->
                text message
        , viewTable (model.currentRow :: model.table) (Nothing :: List.map Just model.actions)

        -- , text (Debug.toString model.automaton)
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
                            , "%eof"
                            ]
                        }
                  , table = []
                  , actions = []
                  , automaton = Nothing
                  , grammar = ""
                  , parseEnabled = False
                  }
                , Cmd.none
                )
        , subscriptions = \_ -> Sub.none
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        }
