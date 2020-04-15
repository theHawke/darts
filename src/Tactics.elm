module Tactics exposing (Msg, State, isExitMsg, makeInitState, update, view)

import Array exposing (Array)
import Browser
import Css exposing (pt, px)
import Dartboard exposing (Dartboard(..), dartboard)
import Dict exposing (Dict)
import Html.Styled as S exposing (br, button, div, h1, span, table, td, text, th, toUnstyled, tr)
import Html.Styled.Attributes exposing (colspan, css, src, width)
import Html.Styled.Events exposing (onClick)
import Util exposing (zip)


type alias State =
    { currentTeam : Team
    , currentDarts : Int
    , teamANames : Array String
    , teamBNames : Array String
    , teamACurrent : Int
    , teamBCurrent : Int
    , teamAPoints : Int
    , teamBPoints : Int
    , scoreBoard : Dict Int ( Int, Int ) -- Bull -> 25, Double -> 22, Triple -> 23, 12-20
    , history : List HistoryEntry
    , choice : Maybe Choice
    }


type Msg
    = DartboardMsg Dartboard
    | ChoiceMsg ScoreEntry
    | UndoMsg
    | ExitMsg


type Team
    = A
    | B


type alias HistoryEntry =
    { category : Int
    , count : Int
    , points : Int
    }


type alias Choice =
    { value : Int
    , variant : Int
    }


type alias ScoreEntry =
    { value : Int
    , variant : Int
    , useSpecial : Bool -- whether to count as double or triple
    }


type Result
    = Undecided
    | Victory Team
    | Draw


isExitMsg : Msg -> Bool
isExitMsg m =
    m == ExitMsg


makeInitState : List String -> State
makeInitState players =
    let
        ( players_a, players_b ) =
            Util.unzipAlternate players
    in
    { currentTeam = A
    , currentDarts = 3
    , teamANames = Array.fromList players_a
    , teamBNames = Array.fromList players_b
    , teamACurrent = 0
    , teamBCurrent = 0
    , teamAPoints = 0
    , teamBPoints = 0
    , scoreBoard =
        Dict.fromList <|
            zip [ 20, 19, 18, 17, 16, 15, 14, 13, 12, 23, 22, 25 ]
                (List.repeat 12 ( 3, 3 ))
    , history = []
    , choice = Nothing
    }


update : Msg -> State -> State
update msg state =
    case msg of
        UndoMsg ->
            undoUpdate state

        ChoiceMsg scoreEntry ->
            scoreEntryUpdate scoreEntry state

        DartboardMsg dart ->
            dartThrowUpdate dart state

        _ ->
            state


dartThrowUpdate : Dartboard -> State -> State
dartThrowUpdate d s =
    let
        ( value, variant ) =
            case d of
                Bull ->
                    ( 25, 1 )

                DBull ->
                    ( 25, 2 )

                G n ->
                    ( n, 1 )

                K n ->
                    ( n, 1 )

                D n ->
                    ( n, 2 )

                T n ->
                    ( n, 3 )

                N ->
                    ( 0, 1 )

        choice =
            Dict.member value s.scoreBoard && Dict.member (variant + 20) s.scoreBoard

        useSpecial =
            Dict.member (variant + 20) s.scoreBoard
    in
    if choice then
        { s | choice = Just { value = value, variant = variant } }

    else
        scoreEntryUpdate { value = value, variant = variant, useSpecial = useSpecial } s


scoreEntryUpdate : ScoreEntry -> State -> State
scoreEntryUpdate { value, variant, useSpecial } s =
    let
        change_team =
            s.currentDarts == 1

        new_darts =
            if change_team then
                3

            else
                s.currentDarts - 1

        new_team =
            if change_team then
                otherTeam s.currentTeam

            else
                s.currentTeam

        new_teamACurrent =
            if change_team && s.currentTeam == A then
                modBy (Array.length s.teamANames) <| s.teamACurrent + 1

            else
                s.teamACurrent

        new_teamBCurrent =
            if change_team && s.currentTeam == B then
                modBy (Array.length s.teamBNames) <| s.teamBCurrent + 1

            else
                s.teamBCurrent

        count =
            if useSpecial then
                1

            else
                variant

        point_value =
            if useSpecial then
                variant * value

            else
                value

        category =
            if useSpecial then
                variant + 20

            else
                value

        ( category_actual, count_actual, points ) =
            case Dict.get category s.scoreBoard of
                Nothing ->
                    ( 0, 0, 0 )

                Just ( ca, cb ) ->
                    case s.currentTeam of
                        A ->
                            if count > ca then
                                ( category, ca, point_value * (count - ca) )

                            else
                                ( category, count, 0 )

                        B ->
                            if count > cb then
                                ( category, cb, point_value * (count - cb) )

                            else
                                ( category, count, 0 )

        new_teamAPoints =
            if s.currentTeam == A then
                s.teamAPoints + points

            else
                s.teamAPoints

        new_teamBPoints =
            if s.currentTeam == B then
                s.teamBPoints + points

            else
                s.teamBPoints

        scoreUpdate =
            Maybe.andThen
                (\( ca, cb ) ->
                    case s.currentTeam of
                        A ->
                            if ca - count_actual == 0 && cb == 0 then
                                Nothing

                            else
                                Just ( ca - count_actual, cb )

                        B ->
                            if ca == 0 && cb - count_actual == 0 then
                                Nothing

                            else
                                Just ( ca, cb - count_actual )
                )

        new_scoreBoard =
            if category_actual == 0 then
                s.scoreBoard

            else
                Dict.update category scoreUpdate s.scoreBoard

        historyEntry =
            { category = category_actual, count = count_actual, points = points }
    in
    if victory s == Undecided then
        { s
            | currentTeam = new_team
            , currentDarts = new_darts
            , teamACurrent = new_teamACurrent
            , teamBCurrent = new_teamBCurrent
            , teamAPoints = new_teamAPoints
            , teamBPoints = new_teamBPoints
            , scoreBoard = new_scoreBoard
            , history = historyEntry :: s.history
            , choice = Nothing
        }

    else
        s


undoUpdate : State -> State
undoUpdate s =
    case s.history of
        [] ->
            s

        { category, count, points } :: hist_remainder ->
            let
                change_team =
                    s.currentDarts == 3

                new_darts =
                    if change_team then
                        1

                    else
                        s.currentDarts + 1

                new_team =
                    if change_team then
                        otherTeam s.currentTeam

                    else
                        s.currentTeam

                new_teamACurrent =
                    if change_team && new_team == A then
                        modBy (Array.length s.teamANames) (s.teamACurrent - 1)

                    else
                        s.teamACurrent

                new_teamBCurrent =
                    if change_team && new_team == B then
                        modBy (Array.length s.teamBNames) (s.teamBCurrent - 1)

                    else
                        s.teamBCurrent

                new_teamAPoints =
                    if new_team == A then
                        s.teamAPoints - points

                    else
                        s.teamAPoints

                new_teamBPoints =
                    if new_team == B then
                        s.teamBPoints - points

                    else
                        s.teamBPoints

                scoreUpdate entry =
                    case entry of
                        Just ( ca, cb ) ->
                            if new_team == A then
                                Just ( ca + count, cb )

                            else
                                Just ( ca, cb + count )

                        Nothing ->
                            if new_team == B then
                                Just ( count, 0 )

                            else
                                Just ( 0, count )

                new_scoreBoard =
                    if count == 0 then
                        -- count 0 indicates no numbers were marked (but maybe points)
                        s.scoreBoard

                    else
                        Dict.update category scoreUpdate s.scoreBoard
            in
            { s
                | currentTeam = new_team
                , currentDarts = new_darts
                , teamACurrent = new_teamACurrent
                , teamBCurrent = new_teamBCurrent
                , teamAPoints = new_teamAPoints
                , teamBPoints = new_teamBPoints
                , scoreBoard = new_scoreBoard
                , history = hist_remainder
                , choice = Nothing
            }


otherTeam : Team -> Team
otherTeam t =
    case t of
        A ->
            B

        B ->
            A


tableRows : List ( Int, String )
tableRows =
    List.map (\n -> ( n, String.fromInt n )) [ 20, 19, 18, 17, 16, 15, 14, 13, 12 ]
        ++ [ ( 22, "Double" ), ( 23, "Triple" ), ( 25, "Bull" ) ]


tableRow : State -> ( Int, String ) -> S.Html a
tableRow s ( n, label ) =
    let
        ( ca, cb ) =
            Maybe.withDefault ( 0, 0 ) <| Dict.get n s.scoreBoard
    in
    tr []
        [ td [] <| List.repeat ca <| text "🎯"
        , td [] [ text label ]
        , td [] <| List.repeat cb <| text "🎯"
        ]


tableSeparator : S.Html a
tableSeparator =
    tr [] [ td [ colspan 100 ] [ S.hr [] [] ] ]


victory : State -> Result
victory s =
    let
        ( cas, cbs ) =
            List.unzip <| Dict.values s.scoreBoard

        a_finished =
            List.sum cas == 0

        b_finished =
            List.sum cbs == 0

        a_victory =
            a_finished && s.teamAPoints > s.teamBPoints

        b_victory =
            b_finished && s.teamBPoints > s.teamAPoints
    in
    if a_victory then
        Victory A

    else if b_victory then
        Victory B

    else if a_finished && b_finished then
        Draw

    else
        Undecided


view : State -> Browser.Document Msg
view s =
    let
        teamACurrent =
            Maybe.withDefault "" <| Array.get s.teamACurrent s.teamANames

        teamBCurrent =
            Maybe.withDefault "" <| Array.get s.teamBCurrent s.teamBNames

        currentPlayer =
            if s.currentTeam == A then
                teamACurrent

            else
                teamBCurrent

        nextUp =
            if s.currentTeam == A then
                teamBCurrent

            else
                teamACurrent

        teamA =
            String.join ", " <| Array.toList s.teamANames

        teamB =
            String.join ", " <| Array.toList s.teamBNames

        ( choice1Text, choice2Text ) =
            case s.choice of
                Nothing ->
                    ( "Invalid", "Invalid" )

                Just { value, variant } ->
                    let
                        ( ca_val, cb_val ) =
                            Maybe.withDefault ( 0, 0 ) <| Dict.get value s.scoreBoard

                        c_val =
                            if s.currentTeam == A then
                                ca_val

                            else
                                cb_val

                        points1S =
                            if c_val < variant then
                                " (" ++ (String.fromInt <| (variant - c_val) * value) ++ " points)"

                            else
                                ""

                        ( ca_var, cb_var ) =
                            Maybe.withDefault ( 0, 0 ) <| Dict.get (variant + 20) s.scoreBoard

                        c_var =
                            if s.currentTeam == A then
                                ca_var

                            else
                                cb_var

                        points2S =
                            if c_var == 0 then
                                " (" ++ (String.fromInt <| variant * value) ++ " points)"

                            else
                                ""

                        fieldString =
                            if value == 25 then
                                "Bull"

                            else
                                String.fromInt value
                    in
                    ( String.fromInt variant ++ " x " ++ fieldString ++ points1S
                    , (if variant == 2 then
                        "Double"

                       else
                        "Triple"
                      )
                        ++ points2S
                    )

        choice1Click =
            case s.choice of
                Nothing ->
                    []

                Just { value, variant } ->
                    [ onClick <| ChoiceMsg { value = value, variant = variant, useSpecial = False } ]

        choice2Click =
            case s.choice of
                Nothing ->
                    []

                Just { value, variant } ->
                    [ onClick <| ChoiceMsg { value = value, variant = variant, useSpecial = True } ]

        choiceVisible =
            case s.choice of
                Nothing ->
                    [ Css.visibility Css.hidden ]

                Just _ ->
                    [ Css.visibility Css.visible ]

        ( victory_a, victory_b ) =
            case victory s of
                Victory A ->
                    ( "☆", "" )

                Victory B ->
                    ( "", "☆" )

                _ ->
                    ( "", "" )
    in
    { title = "Tactics Scoreboard"
    , body =
        List.map toUnstyled
            [ h1 [ css [ Css.textAlign Css.center ] ] [ text "Tactics Scoreboard" ]
            , div []
                [ div [ css [ Css.float Css.left, Css.position Css.relative ] ]
                    [ S.map DartboardMsg <| dartboard 500 []
                    , div
                        [ css
                            ([ Css.backgroundColor Css.transparent
                             , Css.width <| px 500
                             , Css.height <| px 500
                             , Css.position Css.absolute
                             , Css.top <| px 0
                             , Css.left <| px 0
                             , Css.zIndex <| Css.int 1
                             ]
                                ++ choiceVisible
                            )
                        ]
                        [ div
                            [ css
                                [ Css.height <| px 200
                                , Css.width <| px 300
                                , Css.backgroundColor <| Css.rgba 255 255 255 0.8
                                , Css.margin2 (px 150) Css.auto
                                , Css.textAlign Css.center
                                ]
                            ]
                            [ button (css [ Css.width <| px 200, Css.marginTop <| px 50, Css.marginBottom <| px 10 ] :: choice1Click) [ text choice1Text ]
                            , br [] []
                            , span [] [ text "or" ]
                            , br [] []
                            , button (css [ Css.width <| px 200, Css.marginTop <| px 10 ] :: choice2Click) [ text choice2Text ]
                            ]
                        ]
                    ]
                , div
                    [ css
                        [ Css.float Css.left
                        , Css.width <| Css.pct 15
                        , Css.marginTop (px 100)
                        , Css.textAlign Css.center
                        ]
                    ]
                    ([ span [ css [ Css.fontSize <| pt 20 ] ] [ text currentPlayer ]
                     , br [] []
                     ]
                        ++ (List.repeat s.currentDarts <| S.img [ src "assets/dart.svg", width 20, css [ Css.margin <| px 2 ] ] [])
                        ++ [ br [] []
                           , span [ css [ Css.fontSize <| pt 50 ] ] [ text "↓" ]
                           , br [] []
                           , span [ css [ Css.fontSize <| pt 15 ] ] [ text nextUp ]
                           , br [] []
                           , button [ onClick UndoMsg, css [ Css.marginTop <| px 75 ] ] [ text "Undo" ]
                           , button [ onClick ExitMsg, css [ Css.marginTop <| px 25 ] ] [ text "Exit" ]
                           ]
                    )
                , div [ css [ Css.float Css.left, Css.width <| Css.pct 50 ] ]
                    [ table [ css [ Css.textAlign Css.center, Css.fontSize <| pt 16 ] ]
                        ([ tr []
                            [ th [] [ text teamA ]
                            , th [] []
                            , th [] [ text teamB ]
                            ]
                         , tableSeparator
                         ]
                            ++ Util.intersperseEvery 3
                                tableSeparator
                                (List.map (tableRow s) tableRows
                                    ++ [ tr []
                                            [ td [] [ text <| String.fromInt s.teamAPoints ]
                                            , td [] [ text "Points" ]
                                            , td [] [ text <| String.fromInt s.teamBPoints ]
                                            ]
                                       , tr []
                                            [ td [ css [ Css.fontSize <| pt 40 ] ] [ text victory_a ]
                                            , td [] []
                                            , td [ css [ Css.fontSize <| pt 40 ] ] [ text victory_b ]
                                            ]
                                       ]
                                )
                        )
                    ]
                ]
            ]
    }
