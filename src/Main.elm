module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, table, td, text, textarea, th, tr)
import Html.Attributes exposing (cols, placeholder, rows)
import Html.Events exposing (onClick, onInput)
import Parser exposing ((|.), (|=), Parser, Step(..), Trailing(..))
import Set
import Stack exposing (Stack)



-- Tokenizer


tokenize : String -> List String
tokenize input =
    []


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
    , automaton : Dict Int State
    }


type alias Row =
    { state : StateId
    , stack : Stack
    , lookAhead : String
    , unscanned : List String
    }


type alias State =
    { stateId : StateId
    , rules : Dict Int Rule
    , lookaheads : Dict String ShiftReduceAction
    , gotos : Dict String StateId
    }


type Rule
    = Rule String (List String)


testAutomaton : Dict Int State
testAutomaton =
    Dict.fromList [ ( 0, testState ) ]


testState : State
testState =
    { stateId = StateId 0
    , rules = testRules
    , lookaheads = testLookaheads
    , gotos = testGotos
    }


testRules : Dict Int Rule
testRules =
    Dict.fromList
        [ ( 0, Rule "%start_pImplementation" [ "Implementation" ] )
        ]


testLookaheads : Dict String ShiftReduceAction
testLookaheads =
    Dict.fromList
        [ ( "!", ShiftAndEnter (StateId 63) )
        , ( "(", ShiftAndEnter (StateId 64) )
        , ( "+", ShiftAndEnter (StateId 65) )
        , ( "+.", ShiftAndEnter (StateId 66) )
        , ( "-", ShiftAndEnter (StateId 67) )
        , ( "-.", ShiftAndEnter (StateId 68) )
        , ( ";;", ShiftAndEnter (StateId 69) )
        , ( "[", ShiftAndEnter (StateId 70) )
        , ( "[%", ShiftAndEnter (StateId 71) )
        , ( "[%%", ShiftAndEnter (StateId 72) )
        , ( "[@@@", ShiftAndEnter (StateId 73) )
        , ( "[|", ShiftAndEnter (StateId 74) )
        , ( "`", ShiftAndEnter (StateId 75) )
        , ( "assert", ShiftAndEnter (StateId 76) )
        , ( "begin", ShiftAndEnter (StateId 77) )
        , ( "class", ShiftAndEnter (StateId 78) )
        , ( "exception", ShiftAndEnter (StateId 79) )
        , ( "external", ShiftAndEnter (StateId 80) )
        , ( "false", ShiftAndEnter (StateId 81) )
        , ( "for", ShiftAndEnter (StateId 82) )
        , ( "fun", ShiftAndEnter (StateId 83) )
        , ( "function", ShiftAndEnter (StateId 84) )
        , ( "if", ShiftAndEnter (StateId 85) )
        , ( "include", ShiftAndEnter (StateId 86) )
        , ( "lazy", ShiftAndEnter (StateId 87) )
        , ( "let", ShiftAndEnter (StateId 88) )
        , ( "match", ShiftAndEnter (StateId 89) )
        , ( "module", ShiftAndEnter (StateId 90) )
        , ( "new", ShiftAndEnter (StateId 91) )
        , ( "object", ShiftAndEnter (StateId 92) )
        , ( "open", ShiftAndEnter (StateId 93) )
        , ( "true", ShiftAndEnter (StateId 94) )
        , ( "try", ShiftAndEnter (StateId 95) )
        , ( "type", ShiftAndEnter (StateId 96) )
        , ( "val", ShiftAndEnter (StateId 97) )
        , ( "while", ShiftAndEnter (StateId 98) )
        , ( "{'", ShiftAndEnter (StateId 99) )
        , ( "{<'", ShiftAndEnter (StateId 100) )
        , ( "_charac", ShiftAndEnter (StateId 101) )
        , ( "_quoted", ShiftAndEnter (StateId 102) )
        , ( "_FLOAT", ShiftAndEnter (StateId 103) )
        , ( "_LETOP", ShiftAndEnter (StateId 104) )
        , ( "_DecimalLiteralshift", ShiftAndEnter (StateId 105) )
        , ( "_DecimalLiteralModifiershift", ShiftAndEnter (StateId 106) )
        , ( "_HexLiteral", ShiftAndEnter (StateId 107) )
        , ( "_HexLiteralModifiershift", ShiftAndEnter (StateId 108) )
        , ( "_OctLiteral", ShiftAndEnter (StateId 109) )
        , ( "_OctLiteralModifiershift", ShiftAndEnter (StateId 110) )
        , ( "_BinLiteral", ShiftAndEnter (StateId 111) )
        , ( "_BinLiteralModifiershift", ShiftAndEnter (StateId 112) )
        , ( "_LIDENT", ShiftAndEnter (StateId 113) )
        , ( "_PREFIXOP", ShiftAndEnter (StateId 114) )
        , ( "_QUOTED_STRING_EXPRshift", ShiftAndEnter (StateId 115) )
        , ( "_QUOTED_STRING_ITEMshift", ShiftAndEnter (StateId 116) )
        , ( "_UIDENT", ShiftAndEnter (StateId 117) )
        , ( "%eof", ReduceUsingRule (RuleId 67) )
        ]


testGotos : Dict String StateId
testGotos =
    Dict.fromList
        [ ( "Char", StateId 3 )
        , ( "String", StateId 4 )
        , ( "FLOAT", StateId 5 )
        , ( "LETOP", StateId 6 )
        , ( "DecimalLiteral", StateId 7 )
        , ( "DecimalLiteralModifier", StateId 8 )
        , ( "HexLiteral", StateId 9 )
        , ( "HexLiteralModifier", StateId 10 )
        , ( "OctLiteral", StateId 11 )
        , ( "OctLiteralModifier", StateId 12 )
        , ( "BinLiteral", StateId 13 )
        , ( "BinLiteralModifier", StateId 14 )
        , ( "LIDENT", StateId 15 )
        , ( "PREFIXOP", StateId 16 )
        , ( "QUOTED_STRING_EXPR", StateId 17 )
        , ( "QUOTED_STRING_ITEM", StateId 18 )
        , ( "UIDENT", StateId 19 )
        , ( "INT", StateId 20 )
        , ( "Implementation", StateId 21 )
        , ( "ListStructureElement", StateId 22 )
        , ( "Structure", StateId 23 )
        , ( "StructureElement", StateId 24 )
        , ( "StructureItem", StateId 25 )
        , ( "ModuleTypeDeclaration", StateId 26 )
        , ( "OpenDeclaration", StateId 27 )
        , ( "OpenDotDeclaration", StateId 28 )
        , ( "ClassDeclarations", StateId 29 )
        , ( "ClassTypeDeclarations", StateId 30 )
        , ( "ClassTypeDeclaration", StateId 31 )
        , ( "FunSeqExpr", StateId 32 )
        , ( "SeqExpr", StateId 33 )
        , ( "FunExpr", StateId 34 )
        , ( "Expr", StateId 35 )
        , ( "FunExprAttrs", StateId 36 )
        , ( "SimpleExpr", StateId 37 )
        , ( "SimpleExprAttrs", StateId 38 )
        , ( "SimpleExpr_", StateId 39 )
        , ( "LetBindingsExt", StateId 40 )
        , ( "ValueDescription", StateId 41 )
        , ( "PrimitiveDeclaration", StateId 42 )
        , ( "TypeDeclarations", StateId 43 )
        , ( "TypeDeclaration", StateId 44 )
        , ( "StrExceptionDeclaration", StateId 45 )
        , ( "SigExceptionDeclaration", StateId 46 )
        , ( "StrTypeExtension", StateId 47 )
        , ( "Label", StateId 48 )
        , ( "Constant", StateId 49 )
        , ( "ValExtraIdent", StateId 50 )
        , ( "ValIdent", StateId 51 )
        , ( "ConstrExtraIdentgoto", StateId 52 )
        , ( "ConstrExtraNonprefixIdent", StateId 53 )
        , ( "ConstrLongident", StateId 54 )
        , ( "ValLongident", StateId 55 )
        , ( "ModLongident", StateId 56 )
        , ( "NameTag", StateId 57 )
        , ( "Subtractive", StateId 58 )
        , ( "Additive", StateId 59 )
        , ( "FloatingAttributegoto", StateId 60 )
        , ( "Extension", StateId 61 )
        , ( "ItemExtension", StateId 62 )
        ]


parserStates : Parser (Dict Int State)
parserStates =
    Parser.map Dict.fromList
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
    Parser.succeed
        (\stateId rules lookaheads gotos ->
            ( stateId, State (StateId stateId) rules lookaheads gotos )
        )
        |. Parser.keyword "State"
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces
        |= parserDict parserRule
        |= parserDict parserLookahead
        |= parserDict parserGoto


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


parserRule : Parser ( Int, Rule )
parserRule =
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
                    |. Parser.symbol " "
                ]
    in
    Parser.succeed (\pre posts ruleId -> ( ruleId, Rule pre posts ))
        |. Parser.symbol "\t"
        |= parserNonTerminal
        |. Parser.spaces
        |. Parser.symbol "->"
        |. Parser.spaces
        |= Parser.loop [] parserRHS
        |. Parser.spaces
        |. Parser.symbol "("
        |. Parser.keyword "rule"
        |. Parser.spaces
        |= Parser.int
        |. Parser.symbol ")"


parserLookahead : Parser ( String, ShiftReduceAction )
parserLookahead =
    Parser.succeed identity
        |. Parser.symbol "\t"
        |= parserTerminal
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
                        |= Parser.oneOf
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
            )


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
            case Parser.run parserStates model.grammar of
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


step : String -> Row -> Dict Int State -> Result String ( Row, Action )
step token row automaton =
    case row.state of
        StateId stateId ->
            case Dict.get stateId automaton of
                Nothing ->
                    Err ("State " ++ String.fromInt stateId ++ " not found")

                Just state ->
                    case Dict.get token state.lookaheads of
                        Nothing ->
                            Debug.todo "Implement this case"

                        Just ((ShiftAndEnter nextStateId) as action) ->
                            Ok
                                ( { state = nextStateId
                                  , stack = Stack.push token row.stack
                                  , lookAhead = ""
                                  , unscanned = List.drop 1 row.unscanned
                                  }
                                , ShiftReduce action
                                )

                        Just ((ReduceUsingRule (RuleId ruleId)) as action) ->
                            case Dict.get ruleId state.rules of
                                Nothing ->
                                    Err ("Rule " ++ String.fromInt ruleId ++ " not found")

                                Just rule ->
                                    Ok
                                        ( { state = row.state
                                          , stack = applyRule rule row.stack
                                          , lookAhead = ""
                                          , unscanned = row.unscanned
                                          }
                                        , ShiftReduce action
                                        )

                        Just Accept ->
                            Ok ( row, ShiftReduce Accept )


applyRule : Rule -> Stack -> Stack
applyRule (Rule pre posts) stack =
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
                  , automaton = testAutomaton
                  , grammar = ""
                  }
                , Cmd.none
                )
        , subscriptions = \_ -> Sub.none
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        }
