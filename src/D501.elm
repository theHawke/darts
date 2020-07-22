module D501 exposing (Msg, State, isExitMsg, makeInitState, update, view)

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
    { currentPlayer : Int
    , currentDarts : Int
    , currentPoints : Int
    , playerNames : Array String
    , playerPoints : Scoreboard
    , history : List HistoryEntry
    }


type Msg
    = DartboardMsg Dartboard
    | UndoMsg
    | ExitMsg


type alias Scoreboard =
    Dict Int (List Int)


type alias HistoryEntry =
    { player : Int
    , darts : Int
    , points : Int
    , turn : Bool
    }


isExitMsg : Msg -> Bool
isExitMsg m =
    m == ExitMsg


makeInitState : List String -> State
makeInitState players =
    let
        numPlayers =
            List.length players

        initPoints =
            zip (List.range 0 (numPlayers - 1)) <| List.repeat numPlayers []
    in
    { currentPlayer = 0
    , currentDarts = 3
    , currentPoints = 0
    , playerNames = Array.fromList players
    , playerPoints = Dict.fromList initPoints
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

                finished =
                    getPlayerPoints nextCandidate s.playerPoints == 0
            in
            if not finished || nextCandidate == s.currentPlayer then
                nextCandidate

            else
                findNextPlayer nextCandidate

        nextPlayer =
            findNextPlayer s.currentPlayer

        newCurrentPoints =
            s.currentPoints + dartboardValue d

        oldPlayerPoints =
            getPlayerPoints s.currentPlayer s.playerPoints

        newPlayerPoints =
            oldPlayerPoints - newCurrentPoints

        turnInvalid =
            (newPlayerPoints < 0) || (newPlayerPoints == 1) || (newPlayerPoints == 0 && not (isDouble d))

        playerPointsUpdate =
            addPlayerPoints s.currentPlayer
                s.playerPoints
                (if turnInvalid then
                    oldPlayerPoints

                 else
                    newPlayerPoints
                )

        turnFinished =
            s.currentDarts == 1 || turnInvalid || newPlayerPoints == 0

        historyEntry =
            { player = s.currentPlayer
            , darts = s.currentDarts
            , points = s.currentPoints
            , turn = turnFinished
            }
    in
    if turnFinished then
        { s
            | currentPlayer = nextPlayer
            , currentDarts = 3
            , currentPoints = 0
            , playerPoints = playerPointsUpdate
            , history = historyEntry :: s.history
        }

    else
        { s
            | currentDarts = s.currentDarts - 1
            , currentPoints = newCurrentPoints
            , history = historyEntry :: s.history
        }


undoUpdate : State -> State
undoUpdate s =
    case s.history of
        [] ->
            s

        { player, darts, points, turn } :: histRemainder ->
            if not turn then
                -- undo within the current turn
                { s
                    | currentDarts = darts
                    , currentPoints = points
                    , history = histRemainder
                }

            else
                { s
                    | currentPlayer = player
                    , currentDarts = darts
                    , currentPoints = points
                    , playerPoints = undoPlayerPoints player s.playerPoints
                    , history = histRemainder
                }


dartboardValue : Dartboard -> Int
dartboardValue d =
    case d of
        Bull ->
            25

        DBull ->
            50

        G n ->
            n

        K n ->
            n

        D n ->
            2 * n

        T n ->
            3 * n

        N ->
            0


isDouble : Dartboard -> Bool
isDouble d =
    case d of
        DBull ->
            True

        D _ ->
            True

        _ ->
            False


getPlayerPoints : Int -> Scoreboard -> Int
getPlayerPoints p t =
    Dict.get p t |> Maybe.andThen List.head |> Maybe.withDefault 501


addPlayerPoints : Int -> Scoreboard -> Int -> Scoreboard
addPlayerPoints player table points =
    Dict.update player (Maybe.andThen <| (::) points >> Maybe.Just) table


undoPlayerPoints : Int -> Scoreboard -> Scoreboard
undoPlayerPoints p t =
    Dict.update p (Maybe.andThen List.tail) t


preparePointsTableRows : State -> List (S.Html a)
preparePointsTableRows s =
    let
        players =
            List.range 0 (Array.length s.playerNames - 1)

        makeTD string =
            td [] [ text string ]

        pointsLists =
            List.map (\x -> Dict.get x s.playerPoints |> Maybe.withDefault [] |> List.reverse |> List.map String.fromInt) players

        entries =
            List.map List.length pointsLists

        rows =
            max 10 <| Maybe.withDefault 0 <| List.maximum entries

        rowStrings =
            transposeWithDefault rows "" pointsLists
    in
    List.map (tr [ css [ Css.padding (px 10) ] ] << List.map makeTD) rowStrings


transposeWithDefault : Int -> a -> List (List a) -> List (List a)
transposeWithDefault length default matrix =
    if length == 0 then
        []

    else
        List.map (List.head >> Maybe.withDefault default) matrix
            :: transposeWithDefault (length - 1) default (List.map (List.tail >> Maybe.withDefault []) matrix)


view : State -> Browser.Document Msg
view s =
    let
        currentPlayer =
            Array.get s.currentPlayer s.playerNames |> Maybe.withDefault ""

        currentScore =
            String.fromInt <| getPlayerPoints s.currentPlayer s.playerPoints - s.currentPoints

        findNextPlayer p =
            -- possibly skip over players that have already finished
            let
                nextCandidate =
                    modBy (Array.length s.playerNames) (p + 1)

                finished =
                    getPlayerPoints nextCandidate s.playerPoints == 0
            in
            if not finished || nextCandidate == s.currentPlayer then
                nextCandidate

            else
                findNextPlayer nextCandidate

        nextUp =
            Array.get (findNextPlayer s.currentPlayer) s.playerNames |> Maybe.withDefault ""
    in
    { title = "501 Scoreboard"
    , body =
        List.map toUnstyled
            [ h1 [ css [ Css.textAlign Css.center ] ] [ text "501 Scoreboard" ]
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
                    ([ span [ css [ Css.fontSize <| pt 20 ] ] [ text currentPlayer ]
                     , br [] []
                     ]
                        ++ (List.repeat s.currentDarts <| S.img [ src "dart.svg", width 20, css [ Css.margin <| px 2 ] ] [])
                        ++ [ br [] []
                           , span [ css [ Css.fontSize <| pt 30 ] ] [ text currentScore ]
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
                        ([ tr []
                            (Array.toList <|
                                Array.map
                                    (\name -> th [ css [ Css.padding (px 10) ] ] [ text name ])
                                    s.playerNames
                            )
                         , tr [] [ td [ colspan 100 ] [ S.hr [] [] ] ]
                         ]
                            ++ preparePointsTableRows s
                        )
                    ]
                ]
            ]
    }
