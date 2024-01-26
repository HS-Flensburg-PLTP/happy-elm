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
import String.Extra as String



-- import Automaton


type Msg
    = Step
    | ParseInput
    | ChangeGrammar String
    | ParseInfo


type StateId
    = StateId Int


stateIdToInt : StateId -> Int
stateIdToInt (StateId no) =
    no


stateIdToString : StateId -> String
stateIdToString (StateId no) =
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


type PerformedAction
    = ShiftReduce ShiftReduceAction (List ShiftReduceAction)
    | GoTo StateId


actionToString : PerformedAction -> String
actionToString action =
    let
        showConflicts conflicts =
            case conflicts of
                [] ->
                    ""

                _ ->
                    "(" ++ String.pluralize "conflict" "conflicts" (List.length conflicts) ++ ")"
    in
    case action of
        ShiftReduce (ShiftAndEnter state) conflicts ->
            "shift and enter state " ++ stateIdToString state ++ " " ++ showConflicts conflicts

        ShiftReduce (ReduceUsingRule rule) conflicts ->
            "reduce using rule " ++ ruleIdToString rule ++ " " ++ showConflicts conflicts

        ShiftReduce Accept _ ->
            "accept"

        GoTo state ->
            "go to state " ++ stateIdToString state


type alias Model =
    { grammar : String
    , parseEnabled : Bool
    , maybeError : Maybe String
    , parserState : ParserState
    , history : List ParserState
    , actions : List PerformedAction
    , automaton : Maybe Automaton
    }


type alias ParserState =
    { valueStack : Stack String
    , controlStack : Stack StateId
    , unscanned : List String
    }


type alias Automaton =
    { grammar : Dict Int Production
    , states : Dict Int State
    }


type alias State =
    { stateId : StateId
    , lookaheads : Dict String Action
    , gotos : Dict String StateId
    }


type alias Action =
    { action : ShiftReduceAction
    , conflicts : List ShiftReduceAction
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
    let
        parserState =
            Parser.succeed (\stateId lookaheads gotos -> ( stateId, State (StateId stateId) lookaheads gotos ))
                |. Parser.keyword "State"
                |. Parser.spaces
                |= Parser.int
                |. Parser.spaces
                |. parserDict (parserProduction Rule)
                |= parserDict parserLookahead
                |= parserDict parserGoto
    in
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


parserLookahead : Parser ( String, Action )
parserLookahead =
    Parser.succeed (\( name, action ) conflicts -> ( name, Action action conflicts ))
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

        ParseInfo ->
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
                            case step automaton model.parserState of
                                Ok ( newParserState, action ) ->
                                    { model
                                        | parserState = newParserState
                                        , history = model.parserState :: model.history
                                        , actions = action :: model.actions
                                    }

                                Err message ->
                                    { model | maybeError = Just message }

        ParseInput ->
            case model.maybeError of
                Just _ ->
                    model

                Nothing ->
                    case model.automaton of
                        Nothing ->
                            { model | maybeError = Just "Performed step with no automaton" }

                        Just automaton ->
                            let
                                { finalState, history, actions, maybeError } =
                                    parseInput automaton model.parserState [] []
                            in
                            { model
                                | parserState = finalState
                                , history = history
                                , actions = actions
                                , maybeError = maybeError
                            }


parseInput :
    Automaton
    -> ParserState
    -> List ParserState
    -> List PerformedAction
    ->
        { finalState : ParserState
        , history : List ParserState
        , actions : List PerformedAction
        , maybeError : Maybe String
        }
parseInput automaton state states actions =
    case step automaton state of
        Err message ->
            { finalState = state, history = states, actions = actions, maybeError = Just message }

        Ok ( newState, newAction ) ->
            parseInput automaton newState (state :: states) (newAction :: actions)


step : Automaton -> ParserState -> Result String ( ParserState, PerformedAction )
step automaton parserState =
    case parserState.unscanned of
        [] ->
            Err "Performed step but there are no unscanned tokens"

        token :: _ ->
            case Stack.pop parserState.controlStack of
                Nothing ->
                    Err "Action stack is empty"

                Just ( currentStateId, _ ) ->
                    case Dict.get (stateIdToInt currentStateId) automaton.states of
                        Nothing ->
                            Err ("State " ++ String.fromInt (stateIdToInt currentStateId) ++ " not found")

                        Just state ->
                            performAction automaton state token parserState


performAction : Automaton -> State -> String -> ParserState -> Result String ( ParserState, PerformedAction )
performAction automaton state token currentParserState =
    case Dict.get token state.lookaheads of
        Nothing ->
            Err ("No lookahead found for token " ++ token)

        Just { action, conflicts } ->
            case action of
                ShiftAndEnter nextStateId ->
                    Ok
                        ( { currentParserState
                            | valueStack = Stack.push token currentParserState.valueStack
                            , controlStack = Stack.push nextStateId currentParserState.controlStack
                            , unscanned = List.drop 1 currentParserState.unscanned
                          }
                        , ShiftReduce action conflicts
                        )

                ReduceUsingRule ruleId ->
                    case Dict.get (ruleIdToInt ruleId) automaton.grammar of
                        Nothing ->
                            Err ("Rule " ++ ruleIdToString ruleId ++ " not found")

                        Just rule ->
                            case applyProduction automaton.states rule currentParserState.valueStack currentParserState.controlStack of
                                Err message ->
                                    Err message

                                Ok ( newValueStack, newControlStack ) ->
                                    Ok
                                        ( { currentParserState
                                            | valueStack = newValueStack
                                            , controlStack = newControlStack
                                          }
                                        , ShiftReduce action conflicts
                                        )

                Accept ->
                    Err "The input has been accepted"


applyProduction : Dict Int State -> Production -> Stack String -> Stack StateId -> Result String ( Stack String, Stack StateId )
applyProduction states (Production pre posts) valueStack controlStack =
    case match (List.reverse posts) valueStack of
        Nothing ->
            let
                noOfPosts =
                    List.length posts

                poppedControlStack =
                    Stack.popN noOfPosts controlStack
            in
            case Stack.peek poppedControlStack of
                Nothing ->
                    Err "Control stack does not have a state for the goto table"

                Just stateId ->
                    case Dict.get (stateIdToInt stateId) states of
                        Nothing ->
                            Err ("State " ++ stateIdToString stateId ++ " not available")

                        Just state ->
                            case Dict.get pre state.gotos of
                                Nothing ->
                                    Err ("Goto table does not have an entry for " ++ pre)

                                Just newStateId ->
                                    Ok
                                        ( Stack.push pre (Stack.popN noOfPosts valueStack)
                                        , Stack.push newStateId poppedControlStack
                                        )

        Just message ->
            Err message


match : List String -> Stack String -> Maybe String
match input valueStack =
    case input of
        [] ->
            Nothing

        str :: strs ->
            case Stack.pop valueStack of
                Nothing ->
                    Just "Stack is empty"

                Just ( head, newStack ) ->
                    if str == head then
                        match strs newStack

                    else
                        Just "Head of stack does not match head of list"


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
        , button [ onClick ParseInfo, disabled (not model.parseEnabled) ] [ text "Parse automaton" ]
        , button [ onClick Step, disabled (model.automaton == Nothing) ] [ text "Perform step" ]
        , button [ onClick ParseInput, disabled (model.automaton == Nothing) ] [ text "Parse input" ]
        , case model.maybeError of
            Nothing ->
                text ""

            Just message ->
                text message
        , viewTable (model.parserState :: model.history) (Nothing :: List.map Just model.actions)

        -- , text (Debug.toString model.automaton)
        ]


viewTable : List ParserState -> List (Maybe PerformedAction) -> Html Msg
viewTable rows actions =
    table [] (tableHeader :: List.map2 viewRow (List.reverse rows) (List.reverse actions))


tableHeader : Html Msg
tableHeader =
    tr []
        [ th [] [ text "Parse Stack" ]
        , th [] [ text "Control Stack" ]
        , th [] [ text "Unscanned" ]
        , th [] [ text "Parser Action" ]
        ]


viewRow : ParserState -> Maybe PerformedAction -> Html Msg
viewRow parserState maybeAction =
    tr []
        [ td [] [ text (Stack.toString identity parserState.valueStack) ]
        , td [] [ text (Stack.toString stateIdToString parserState.controlStack) ]
        , td [] [ text (String.join " " parserState.unscanned) ]
        , td [] [ text (Maybe.withDefault "" (Maybe.map actionToString maybeAction)) ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( { maybeError = Nothing
                  , parserState =
                        { valueStack = Stack.empty
                        , controlStack = Stack.push (StateId 0) Stack.empty
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
                  , history = []
                  , actions = []

                  --   , automaton = Just Automaton.o
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
