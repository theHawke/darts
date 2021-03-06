module Halbieren exposing (Msg, Round(..), State, defaultRounds, isExitMsg, makeInitState, update, view)

import Array exposing (Array)
import Browser
import Css exposing (decimal, pt, px)
import Dartboard exposing (Dartboard(..), dartboard)
import Dict exposing (Dict, singleton)
import Html.Styled as S exposing (br, button, div, h1, i, span, table, td, text, th, toUnstyled, tr)
import Html.Styled.Attributes exposing (colspan, css, src, width)
import Html.Styled.Events exposing (onClick)
import Svg.Attributes exposing (x)
import Util exposing (arrayAdd, arraySub, zip)


type alias State =
    { playerNames : Array String
    , currentPlayer : Int
    , currentDarts : Int
    , currentPoints : Int
    , playerPoints : Scoreboard
    , history : List HistoryEntry
    , rounds : Array Round
    }


type Msg
    = DartboardMsg Dartboard
    | UndoMsg
    | ExitMsg


type Result
    = Undecided
    | Won Int


type alias Scoreboard =
    Dict Int (List Int)


type alias HistoryEntry =
    { player : Int
    , darts : Int
    , points : Int
    , turn : Bool
    }


type Round
    = Number Int
    | Triple
    | Double
    | Red
    | Green
    | Bull
    | Tri Int
    | Dbl Int


roundName : Round -> String
roundName r =
    case r of
        Number i ->
            String.fromInt i

        Triple ->
            "Triple"

        Double ->
            "Double"

        Red ->
            "Red"

        Green ->
            "Green"

        Bull ->
            "Bull"

        Tri i ->
            "T" ++ String.fromInt i

        Dbl i ->
            "D" ++ String.fromInt i


defaultRounds : Array Round
defaultRounds =
    Array.fromList
        [ Number 20
        , Number 19
        , Triple
        , Number 18
        , Number 17
        , Double
        , Number 16
        , Number 15
        , Red
        , Number 14
        , Number 13
        , Green
        , Number 12
        , Number 11
        , Bull
        ]


isExitMsg : Msg -> Bool
isExitMsg m =
    m == ExitMsg


initPoints : Int -> Int -> Scoreboard
initPoints numPlayers initPts =
    Dict.fromList <| zip (List.range 0 (numPlayers - 1)) <| List.repeat numPlayers <| List.singleton initPts


makeInitState : Int -> Array Round -> List String -> State
makeInitState initPts rounds players =
    let
        numPlayers =
            List.length players
    in
    { currentPlayer = 0
    , currentDarts = 3
    , currentPoints = 0
    , playerNames = Array.fromList players
    , playerPoints = initPoints numPlayers initPts
    , history = []
    , rounds = rounds
    }


isNumber : Int -> Dartboard -> Bool
isNumber n dart =
    case dart of
        G x ->
            x == n

        K x ->
            x == n

        D x ->
            x == n

        T x ->
            x == n

        _ ->
            False


isDouble : Dartboard -> Bool
isDouble d =
    case d of
        D n ->
            True

        DBull ->
            True

        _ ->
            False


isTriple : Dartboard -> Bool
isTriple d =
    case d of
        T n ->
            True

        _ ->
            False


isBull : Dartboard -> Bool
isBull d =
    case d of
        Dartboard.Bull ->
            True

        DBull ->
            True

        _ ->
            False


reds : List Int
reds =
    [ 20, 18, 13, 10, 2, 3, 7, 8, 14, 12 ]


greens : List Int
greens =
    [ 1, 4, 6, 15, 17, 19, 16, 11, 9, 5 ]


isRed : Dartboard -> Bool
isRed d =
    case d of
        DBull ->
            True

        D n ->
            List.member n reds

        T n ->
            List.member n reds

        _ ->
            False


isGreen : Dartboard -> Bool
isGreen d =
    case d of
        Dartboard.Bull ->
            True

        D n ->
            List.member n greens

        T n ->
            List.member n greens

        _ ->
            False


isValidForRound : Round -> Dartboard -> Bool
isValidForRound round dart =
    case round of
        Number n ->
            isNumber n dart

        Triple ->
            isTriple dart

        Double ->
            isDouble dart

        Green ->
            isGreen dart

        Red ->
            isRed dart

        Bull ->
            isBull dart

        Tri n ->
            isNumber n dart && isTriple dart

        Dbl n ->
            isNumber n dart && isDouble dart


dartboardValue : Dartboard -> Int
dartboardValue d =
    case d of
        Dartboard.Bull ->
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
        numPlayers =
            Array.length s.playerNames

        nextPlayer =
            modBy numPlayers (s.currentPlayer + 1)

        currentRoundNo =
            (Dict.get s.currentPlayer s.playerPoints
                |> Maybe.withDefault [ 0 ]
                |> List.length
            )
                - 1

        currentRound =
            Array.get currentRoundNo s.rounds
                |> Maybe.withDefault (Number -1)

        gameFinished =
            currentRound == Number -1

        newCurrentPoints =
            if isValidForRound currentRound d then
                s.currentPoints + dartboardValue d

            else
                s.currentPoints

        oldPlayerPoints =
            getPlayerPoints s.currentPlayer s.playerPoints

        newPlayerPoints =
            if newCurrentPoints == 0 then
                (oldPlayerPoints + 1) // 2
                -- +1 to round up instead of down

            else
                oldPlayerPoints + newCurrentPoints

        playerPointsUpdate =
            addPlayerPoints s.currentPlayer
                s.playerPoints
                newPlayerPoints

        turnFinished =
            s.currentDarts == 1

        historyEntry =
            { player = s.currentPlayer
            , darts = s.currentDarts
            , points = s.currentPoints
            , turn = turnFinished
            }
    in
    if gameFinished then
        s

    else if turnFinished then
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


getPlayerPoints : Int -> Scoreboard -> Int
getPlayerPoints p t =
    Dict.get p t |> Maybe.andThen List.head |> Maybe.withDefault -1


addPlayerPoints : Int -> Scoreboard -> Int -> Scoreboard
addPlayerPoints player table points =
    Dict.update player (Maybe.map <| (::) points) table


undoPlayerPoints : Int -> Scoreboard -> Scoreboard
undoPlayerPoints p t =
    Dict.update p (Maybe.andThen List.tail) t


getScoreboardWithVictory : State -> Scoreboard
getScoreboardWithVictory s =
    let
        players =
            List.range 0 (Array.length s.playerNames - 1)

        points =
            List.map (\p -> Dict.get p s.playerPoints |> Maybe.withDefault []) players

        noRounds =
            Array.length s.rounds

        finished =
            List.all (\l -> List.length l == (noRounds + 1)) points

        lastPoints =
            List.map (List.head >> Maybe.withDefault 0) points

        maxScore =
            List.maximum lastPoints |> Maybe.withDefault 0

        winningPlayers =
            List.filterMap
                (\( pl, fs ) ->
                    if fs == maxScore then
                        Just pl

                    else
                        Nothing
                )
            <|
                List.map2 (\a b -> ( a, b )) players lastPoints
    in
    if not finished then
        s.playerPoints

    else
        List.foldl (\p sb -> addPlayerPoints p sb -1) s.playerPoints winningPlayers


preparePointsTableRows : State -> List (S.Html a)
preparePointsTableRows s =
    let
        playerPointsWithVictory =
            getScoreboardWithVictory s

        players =
            List.range 0 (Array.length s.playerNames - 1)

        makeTD string =
            td
                (if string == "☆" then
                    [ css [ Css.padding2 (px 0) (px 10), Css.fontSize <| pt 40 ] ]

                 else
                    [ css [ Css.padding2 (px 0) (px 10) ] ]
                )
                [ text string ]

        pointsToString points =
            if points == -1 then
                "☆"

            else
                String.fromInt points

        pointsLists =
            [ "" :: (List.map roundName <| Array.toList s.rounds) ]
                ++ List.map
                    (\x ->
                        Dict.get x playerPointsWithVictory
                            |> Maybe.withDefault []
                            |> List.reverse
                            |> List.map pointsToString
                    )
                    players

        rows =
            Array.length s.rounds + 2

        rowStrings =
            transposeWithDefault rows "" pointsLists
    in
    List.map (tr [ css [ Css.padding (px 10) ] ] << List.map makeTD) rowStrings
        |> addHBars [ 4, 3, 3, 3 ]


addHBars : List Int -> List (S.Html a) -> List (S.Html a)
addHBars strides rows =
    case strides of
        [] ->
            rows

        s :: ss ->
            let
                before =
                    List.take s rows

                after =
                    List.drop s rows
            in
            if List.length after <= 1 then
                rows

            else
                before
                    ++ td [ colspan 100, css [ Css.height <| px 10 ] ]
                        [ S.hr [] [] ]
                    :: addHBars ss after


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
        numPlayers =
            Array.length s.playerNames

        currentPlayer =
            Array.get s.currentPlayer s.playerNames |> Maybe.withDefault ""

        currentRoundNo =
            (Dict.get s.currentPlayer s.playerPoints
                |> Maybe.withDefault [ 0 ]
                |> List.length
            )
                - 1

        currentRound =
            Array.get currentRoundNo s.rounds |> Maybe.withDefault Bull

        currentScore =
            String.fromInt <| s.currentPoints

        nextUp =
            Array.get (modBy numPlayers (s.currentPlayer + 1)) s.playerNames |> Maybe.withDefault ""
    in
    { title = "Halbieren"
    , body =
        List.map toUnstyled
            [ h1 [ css [ Css.textAlign Css.center ] ] [ text "Halbieren" ]
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
                    ([ span [ css [ Css.fontSize <| pt 14 ] ] [ text "Round:" ]
                     , br [] []
                     , span [ css [ Css.fontSize <| pt 24 ] ] [ text <| roundName currentRound ]
                     , br [] []
                     , br [] []
                     , span [ css [ Css.fontSize <| pt 20 ] ] [ text currentPlayer ]
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
                           , button [ onClick UndoMsg, css [ Css.marginTop <| px 50 ] ] [ text "Undo" ]
                           , button [ onClick ExitMsg, css [ Css.marginTop <| px 50 ] ] [ text "Exit" ]
                           ]
                    )
                , div [ css [ Css.float Css.left ] ]
                    [ table [ css [ Css.textAlign Css.center, Css.fontSize <| pt 16 ] ]
                        ([ tr []
                            ([ th [] [ text "Round" ] ]
                                ++ (Array.toList <|
                                        Array.map
                                            (\name -> th [ css [ Css.padding (px 10) ] ] [ text name ])
                                            s.playerNames
                                   )
                            )
                         , tr [] [ td [ colspan 100 ] [ S.hr [] [] ] ]
                         ]
                            ++ preparePointsTableRows s
                        )
                    ]
                ]
            ]
    }
