module D501 exposing (Msg, State, isExitMsg, makeInitState, update, view)

import Array exposing (Array)
import Browser
import Css exposing (decimal, pt, px)
import Dartboard exposing (Dartboard(..), dartboard)
import Dict exposing (Dict)
import Html.Styled as S exposing (br, button, div, h1, span, table, td, text, th, toUnstyled, tr)
import Html.Styled.Attributes exposing (colspan, css, src, width)
import Html.Styled.Events exposing (onClick)
import Util exposing (arrayAdd, arraySub, zip)


type alias State =
    { currentPlayer : Int
    , currentDarts : Int
    , currentPoints : Int
    , playerNames : Array String
    , playerPoints : Scoreboard
    , legStatus : Result
    , playerLegsWon : Dict Int Int
    , history : List HistoryEntry
    , showStats : Bool
    , legScores : Array Int
    , legDarts : Array Int
    }


type Msg
    = DartboardMsg Dartboard
    | NextLegMsg
    | UndoMsg
    | ExitMsg
    | ToggleStatsMsg


type Result
    = Undecided
    | Won Int


type alias Scoreboard =
    Dict Int (List Int)


type HistoryEntry
    = DartHE HEDart
    | LegHE HELeg


type alias HELeg =
    { player : Int
    , darts : Int
    , points : Int
    , scoreboard : Scoreboard
    , legWinner : Int
    , legScores : Array Int
    , legDarts : Array Int
    }


type alias HEDart =
    { player : Int
    , darts : Int
    , points : Int
    , turn : Bool
    , result : Result
    }


isExitMsg : Msg -> Bool
isExitMsg m =
    m == ExitMsg


initPoints : Int -> Scoreboard
initPoints numPlayers =
    Dict.fromList <| zip (List.range 0 (numPlayers - 1)) <| List.repeat numPlayers [ 501 ]


makeInitState : List String -> State
makeInitState players =
    let
        numPlayers =
            List.length players

        plw =
            zip (List.range 0 (numPlayers - 1)) <| List.repeat numPlayers 0
    in
    { currentPlayer = 0
    , currentDarts = 3
    , currentPoints = 0
    , playerNames = Array.fromList players
    , playerPoints = initPoints numPlayers
    , legStatus = Undecided
    , playerLegsWon = Dict.fromList plw
    , history = []
    , showStats = False
    , legScores = Array.repeat numPlayers 0
    , legDarts = Array.repeat numPlayers 0
    }


update : Msg -> State -> State
update msg state =
    case msg of
        UndoMsg ->
            undoUpdate state

        DartboardMsg dart ->
            dartThrowUpdate dart state

        NextLegMsg ->
            nextLegUpdate state

        ToggleStatsMsg ->
            { state | showStats = not state.showStats }

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

        newLegStatus =
            if not turnInvalid && s.legStatus == Undecided then
                if newPlayerPoints == 0 then
                    Won s.currentPlayer

                else
                    Undecided

            else
                s.legStatus

        historyEntry =
            DartHE
                { player = s.currentPlayer
                , darts = s.currentDarts
                , points = s.currentPoints
                , turn = turnFinished
                , result = s.legStatus
                }

        newLegScores =
            if turnInvalid then
                arraySub s.legScores s.currentPlayer s.currentPoints

            else
                arrayAdd s.legScores s.currentPlayer (dartboardValue d)
    in
    if getPlayerPoints s.currentPlayer s.playerPoints == 0 then
        s
        -- disable updates when all players have reached 0 points in this leg

    else if turnFinished then
        { s
            | currentPlayer = nextPlayer
            , currentDarts = 3
            , currentPoints = 0
            , playerPoints = playerPointsUpdate
            , legStatus = newLegStatus
            , history = historyEntry :: s.history
            , legDarts = arrayAdd s.legDarts s.currentPlayer 1
            , legScores = newLegScores
        }

    else
        { s
            | currentDarts = s.currentDarts - 1
            , currentPoints = newCurrentPoints
            , history = historyEntry :: s.history
            , legDarts = arrayAdd s.legDarts s.currentPlayer 1
            , legScores = newLegScores
        }


undoUpdate : State -> State
undoUpdate s =
    case s.history of
        [] ->
            s

        (DartHE { player, darts, points, turn, result }) :: histRemainder ->
            if not turn then
                -- undo within the current turn
                { s
                    | currentDarts = darts
                    , currentPoints = points
                    , history = histRemainder
                    , legDarts = arraySub s.legDarts player 1
                    , legScores = arraySub s.legScores player (s.currentPoints - points)
                }

            else
                let
                    oldPlayerPoints =
                        undoPlayerPoints player s.playerPoints

                    lastScore =
                        getPlayerPoints player oldPlayerPoints - getPlayerPoints player s.playerPoints

                    oldLegScores =
                        arraySub s.legScores player (lastScore - points)
                in
                { s
                    | currentPlayer = player
                    , currentDarts = darts
                    , currentPoints = points
                    , playerPoints = oldPlayerPoints
                    , legStatus = result
                    , history = histRemainder
                    , legDarts = arraySub s.legDarts player 1
                    , legScores = oldLegScores
                }

        (LegHE { player, darts, points, scoreboard, legWinner, legDarts, legScores }) :: histRemainder ->
            let
                undoPlayerLegsWon =
                    Dict.update legWinner (Maybe.map (\x -> x - 1)) s.playerLegsWon
            in
            { s
                | currentPlayer = player
                , currentDarts = darts
                , currentPoints = points
                , playerPoints = scoreboard
                , history = histRemainder
                , legStatus = Won legWinner
                , playerLegsWon = undoPlayerLegsWon
                , legDarts = legDarts
                , legScores = legScores
            }


nextLegUpdate : State -> State
nextLegUpdate s =
    let
        numPlayers =
            Array.length s.playerNames

        legWinner =
            case s.legStatus of
                Won p ->
                    p

                Undecided ->
                    -1

        -- nextLegMsg should be impossible to trigger in an undecided leg
        newLegsWon =
            Dict.update legWinner (Maybe.map <| \x -> x + 1) s.playerLegsWon

        totalLegsPlayed =
            Dict.toList newLegsWon |> List.map (\( _, w ) -> w) |> List.sum

        he =
            LegHE
                { player = s.currentPlayer
                , darts = s.currentDarts
                , points = s.currentPoints
                , scoreboard = s.playerPoints
                , legWinner = legWinner
                , legScores = s.legScores
                , legDarts = s.legDarts
                }
    in
    { s
        | currentPlayer = modBy numPlayers totalLegsPlayed
        , currentDarts = 3
        , currentPoints = 0
        , playerPoints = initPoints numPlayers
        , playerLegsWon = newLegsWon
        , legStatus = Undecided
        , history = he :: s.history
        , legScores = Array.repeat numPlayers 0
        , legDarts = Array.repeat numPlayers 0
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
    Dict.update player (Maybe.map <| (::) points) table


undoPlayerPoints : Int -> Scoreboard -> Scoreboard
undoPlayerPoints p t =
    Dict.update p (Maybe.andThen List.tail) t


preparePointsTableRows : State -> List (S.Html a)
preparePointsTableRows s =
    let
        playerPointsWithVictory =
            case s.legStatus of
                Undecided ->
                    s.playerPoints

                Won p ->
                    addPlayerPoints p s.playerPoints -1

        players =
            List.range 0 (Array.length s.playerNames - 1)

        makeTD string =
            td
                (if string == "☆" then
                    [ css [ Css.fontSize <| pt 40 ] ]

                 else
                    []
                )
                [ text string ]

        pointsToString points =
            if points == -1 then
                "☆"

            else
                String.fromInt points

        pointsLists =
            List.map
                (\x ->
                    Dict.get x playerPointsWithVictory
                        |> Maybe.withDefault []
                        |> List.reverse
                        |> List.map pointsToString
                )
                players

        entries =
            List.map List.length pointsLists

        rows =
            max 10 <| Maybe.withDefault 0 <| List.maximum entries

        rowStrings =
            transposeWithDefault rows "" pointsLists
    in
    List.map (tr [ css [ Css.padding (px 10) ] ] << List.map makeTD) rowStrings


formatFloat : Int -> Float -> String
formatFloat n f =
    let
        s =
            String.fromFloat f

        parts =
            String.split "." s

        whole =
            List.head parts |> Maybe.withDefault ""

        decimal =
            List.tail parts |> Maybe.andThen List.head |> Maybe.withDefault "0"
    in
    if n > 0 then
        whole ++ "." ++ String.left n decimal

    else
        whole


prepareStatsTableRows : State -> List (S.Html a)
prepareStatsTableRows s =
    let
        dartsList =
            Array.toList s.legDarts

        scoresList =
            Array.toList s.legScores

        avgList =
            List.map2
                (\a b ->
                    if b == 0 then
                        0

                    else
                        toFloat a / toFloat b
                )
                scoresList
                dartsList
    in
    if s.showStats then
        [ tr [] (List.map (\darts -> td [] [ text <| String.fromInt darts ]) dartsList)
        , tr [] (List.map (\score -> td [] [ text <| String.fromInt score ]) scoresList)
        , tr [] (List.map (\avg -> td [] [ text <| formatFloat 1 avg ]) avgList)
        , tr [] [ td [ colspan 100 ] [ S.hr [] [] ] ]
        ]

    else
        []


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
                           , button [ onClick UndoMsg, css [ Css.marginTop <| px 50 ] ] [ text "Undo" ]
                           , button [ onClick ExitMsg, css [ Css.marginTop <| px 50 ] ] [ text "Exit" ]
                           , br [] []
                           , button [ onClick ToggleStatsMsg, css [ Css.marginTop <| px 25 ] ]
                                [ text
                                    (if s.showStats then
                                        "Hide Stats"

                                     else
                                        "Show Stats"
                                    )
                                ]
                           ]
                        ++ (if s.legStatus /= Undecided then
                                [ br [] []
                                , button [ onClick NextLegMsg, css [ Css.marginTop <| px 25 ] ] [ text "Next Leg" ]
                                ]

                            else
                                []
                           )
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
                         , tr [] (List.map (\legs -> td [] [ text <| String.fromInt legs ]) <| Dict.values s.playerLegsWon)
                         , tr [] [ td [ colspan 100 ] [ S.hr [] [] ] ]
                         ]
                            ++ prepareStatsTableRows s
                            ++ preparePointsTableRows s
                        )
                    ]
                ]
            ]
    }
