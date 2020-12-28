module Minus exposing (Msg, State, isExitMsg, makeInitState, update, view)

import Array exposing (Array)
import Browser
import Css exposing (pt, px)
import Dartboard exposing (Dartboard(..), dartboard)
import Dict exposing (Dict)
import Html.Styled as S exposing (a, br, button, div, h1, span, table, td, text, th, toUnstyled, tr)
import Html.Styled.Attributes exposing (css, src, width)
import Html.Styled.Events exposing (onClick)
import Util exposing (zip)


type alias State =
    { currentPlayer : Int
    , currentDarts : Int
    , currentPoints : Int
    , currentTarget : Int
    , playerLives : Array Int
    , swimming : Swimming
    , playerNames : Array String
    , history : List HistoryEntry
    }


type Msg
    = DartboardMsg Dartboard
    | UndoMsg
    | ExitMsg


type Swimming
    = Unused
    | Swimming Int
    | Drowned


type alias HistoryEntry =
    { prevPlayer : Int
    , prevDarts : Int
    , prevPoints : Int
    , prevTarget : Int
    , prevLives : Array Int
    , prevSwimmnig : Swimming
    }


isExitMsg : Msg -> Bool
isExitMsg m =
    m == ExitMsg


makeInitState : List String -> State
makeInitState players =
    let
        numPlayers =
            List.length players
    in
    { currentPlayer = 0
    , currentDarts = 3
    , currentPoints = 0
    , currentTarget = 200
    , playerLives = Array.fromList <| List.repeat numPlayers 3
    , swimming = Unused
    , playerNames = Array.fromList players
    , history = []
    }


update : Msg -> State -> State
update msg state =
    case msg of
        UndoMsg ->
            undoUpdate state

        DartboardMsg dart ->
            dartThrowUpdate dart state

        _ ->
            state


dartThrowUpdate : Dartboard -> State -> State
dartThrowUpdate d s =
    let
        findNextPlayer p =
            -- possibly skip over players that have already finished
            let
                nextCandidate =
                    modBy (Array.length s.playerNames) (p + 1)
            in
            if playerAlive s nextCandidate || nextCandidate == s.currentPlayer then
                nextCandidate

            else
                findNextPlayer nextCandidate

        nextPlayer =
            findNextPlayer s.currentPlayer

        newCurrentPoints =
            s.currentPoints + dartboardValue d

        historyEntry =
            { prevPlayer = s.currentPlayer
            , prevDarts = s.currentDarts
            , prevPoints = s.currentPoints
            , prevTarget = s.currentTarget
            , prevLives = s.playerLives
            , prevSwimmnig = s.swimming
            }

        nextTurn =
            s.currentDarts == 1

        loseLife =
            nextTurn && newCurrentPoints >= s.currentTarget

        currentLives =
            Array.get s.currentPlayer s.playerLives |> Maybe.withDefault 0

        newSwimming =
            if not loseLife then
                s.swimming

            else if currentLives == 1 && s.swimming == Unused then
                Swimming s.currentPlayer

            else if s.swimming == Swimming s.currentPlayer then
                Drowned

            else
                s.swimming

        currentPlayerLives =
            Array.get s.currentPlayer s.playerLives |> Maybe.withDefault 0

        newPlayerLives =
            if not loseLife then
                s.playerLives

            else
                Array.set s.currentPlayer (max 0 (currentPlayerLives - 1)) s.playerLives
    in
    if playersAlive s == 1 then
        s

    else if not nextTurn then
        { s
            | currentDarts = s.currentDarts - 1
            , currentPoints = newCurrentPoints
            , history = historyEntry :: s.history
        }

    else
        { s
            | currentDarts = 3
            , currentPoints = 0
            , currentPlayer = nextPlayer
            , currentTarget = newCurrentPoints
            , playerLives = newPlayerLives
            , swimming = newSwimming
            , history = historyEntry :: s.history
        }


undoUpdate : State -> State
undoUpdate s =
    case s.history of
        [] ->
            s

        he :: histRemainder ->
            { s
                | currentPlayer = he.prevPlayer
                , currentDarts = he.prevDarts
                , currentPoints = he.prevPoints
                , currentTarget = he.prevTarget
                , playerLives = he.prevLives
                , swimming = he.prevSwimmnig
                , history = histRemainder
            }


dartboardValue : Dartboard -> Int
dartboardValue d =
    case d of
        Bull ->
            -25

        DBull ->
            -50

        G n ->
            n

        K n ->
            n

        D n ->
            2 * n

        T n ->
            3 * n

        N ->
            25


playerAlive : State -> Int -> Bool
playerAlive s p =
    (Array.get p s.playerLives |> Maybe.withDefault 0) > 0 || s.swimming == Swimming p


playersAlive : State -> Int
playersAlive s =
    Array.toList s.playerLives
        |> List.map
            (\x ->
                if x > 0 then
                    1

                else
                    0
            )
        |> List.sum
        |> (+)
            (case s.swimming of
                Swimming _ ->
                    1

                _ ->
                    0
            )


livesTable : State -> List (S.Html a)
livesTable s =
    let
        rowFn p =
            let
                lives =
                    Array.get p s.playerLives |> Maybe.withDefault 0

                swimming =
                    s.swimming == Swimming p
            in
            tr []
                [ th [] [ text <| Maybe.withDefault "" <| Array.get p s.playerNames ]
                , td []
                    [ text <|
                        String.repeat lives "❤️"
                            ++ (if swimming then
                                    "🌊"

                                else
                                    ""
                               )
                            ++ (if lives == 0 && not swimming then
                                    "☠️"

                                else
                                    ""
                               )
                            ++ (if (lives > 0 || swimming) && playersAlive s == 1 then
                                    "👑"

                                else
                                    ""
                               )
                    ]
                ]
    in
    List.map rowFn <| List.range 0 (Array.length s.playerNames - 1)


view : State -> Browser.Document Msg
view s =
    let
        currentPlayer =
            Array.get s.currentPlayer s.playerNames |> Maybe.withDefault ""

        findNextPlayer p =
            -- possibly skip over players that have already finished
            let
                nextCandidate =
                    modBy (Array.length s.playerNames) (p + 1)
            in
            if playerAlive s nextCandidate || nextCandidate == s.currentPlayer then
                nextCandidate

            else
                findNextPlayer nextCandidate

        nextUp =
            Array.get (findNextPlayer s.currentPlayer) s.playerNames |> Maybe.withDefault ""
    in
    { title = "Minus Scoreboard"
    , body =
        List.map toUnstyled
            [ h1 [ css [ Css.textAlign Css.center ] ] [ text "Minus Scoreboard" ]
            , div []
                [ div [ css [ Css.float Css.left, Css.position Css.relative ] ]
                    [ S.map DartboardMsg <| dartboard 600 [] ]
                , div
                    [ css
                        [ Css.float Css.left
                        , Css.marginTop (px 100)
                        , Css.marginRight (px 20)
                        , Css.textAlign Css.center
                        ]
                    ]
                    ([ span [ css [ Css.fontSize <| pt 30 ] ]
                        [ text <|
                            if s.currentTarget > 180 then
                                "–"

                            else
                                String.fromInt s.currentTarget
                        ]
                     , br [] []
                     , span [ css [ Css.fontSize <| pt 20 ] ] [ text currentPlayer ]
                     , br [] []
                     ]
                        ++ (List.repeat s.currentDarts <| S.img [ src "dart.svg", width 20, css [ Css.margin <| px 2 ] ] [])
                        ++ [ br [] []
                           , span [ css [ Css.fontSize <| pt 30 ] ] [ text <| String.fromInt s.currentPoints ]
                           , br [] []
                           , span [ css [ Css.fontSize <| pt 50 ] ] [ text "↓" ]
                           , br [] []
                           , span [ css [ Css.fontSize <| pt 15 ] ] [ text nextUp ]
                           , br [] []
                           , button [ onClick UndoMsg, css [ Css.marginTop <| px 75 ] ] [ text "Undo" ]
                           , button [ onClick ExitMsg, css [ Css.marginTop <| px 25 ] ] [ text "Exit" ]
                           ]
                    )
                , div [ css [ Css.float Css.left ] ]
                    [ table [ css [ Css.textAlign Css.center, Css.fontSize <| pt 16 ] ]
                        (livesTable s)
                    ]
                ]
            ]
    }
