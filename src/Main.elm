module Main exposing (main)

import Browser exposing (Document)
import Css
import D501
import Html
import Html.Styled as S exposing (button, div, h1, option, select, text, textarea, toUnstyled)
import Html.Styled.Attributes as A exposing (css)
import Html.Styled.Events exposing (onClick, onInput)
import Platform.Cmd as C
import Tactics


main : Program () State Msg
main =
    Browser.document
        { init = \_ -> ( initState, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


type State
    = GameSelect String String
    | TacticsState Tactics.State
    | D501State D501.State


type Msg
    = PlayersChange String
    | SelectChange String
    | GameStart
    | TacticsMsg Tactics.Msg
    | D501Msg D501.Msg


initState : State
initState =
    GameSelect "Tactics" ""


games : List String
games =
    [ "Tactics"
    , "501"
    ]


update : Msg -> State -> ( State, C.Cmd a )
update m s =
    case ( m, s ) of
        ( TacticsMsg tm, TacticsState ts ) ->
            if Tactics.isExitMsg tm then
                ( initState, C.none )

            else
                ( TacticsState <| Tactics.update tm ts, C.none )

        ( D501Msg fm, D501State fs ) ->
            if D501.isExitMsg fm then
                ( initState, C.none )

            else
                ( D501State <| D501.update fm fs, C.none )

        ( gsm, GameSelect sel players ) ->
            case gsm of
                PlayersChange new_players ->
                    ( GameSelect sel new_players, C.none )

                SelectChange new_sel ->
                    ( GameSelect new_sel players, C.none )

                GameStart ->
                    case sel of
                        "Tactics" ->
                            ( TacticsState <| Tactics.makeInitState <| List.filter (\str -> not <| String.isEmpty str) <| String.split "\n" players, C.none )

                        "501" ->
                            ( D501State <| D501.makeInitState <| List.filter (\str -> not <| String.isEmpty str) <| String.split "\n" players, C.none )

                        _ ->
                            ( s, C.none )

                _ ->
                    ( s, C.none )

        _ ->
            ( s, C.none )


mapDocument : (msgA -> msgB) -> Document msgA -> Document msgB
mapDocument f { title, body } =
    { title = title, body = List.map (Html.map f) body }


view : State -> Document Msg
view s =
    case s of
        TacticsState ts ->
            mapDocument TacticsMsg <| Tactics.view ts

        D501State fs ->
            mapDocument D501Msg <| D501.view fs

        GameSelect sel players ->
            { title = "Darts Scoreboard"
            , body =
                List.map toUnstyled
                    [ h1 [ css [ Css.textAlign Css.center ] ] [ text "Select your Game" ]
                    , div [ css [ Css.textAlign Css.center, Css.margin2 (Css.px 20) Css.auto ] ]
                        [ select [ A.size 5, A.name "Select Game" ]
                            (List.map
                                (makeOption sel)
                                games
                            )
                        , textarea [ onInput PlayersChange, A.placeholder "Enter players, one per row", A.rows 6, A.cols 30 ] [ text players ]
                        , S.br [] []
                        , button [ onClick GameStart, css [ Css.textAlign Css.center, Css.margin2 (Css.px 20) Css.auto ] ] [ text "Start Game" ]
                        ]
                    ]
            }


makeOption : String -> String -> S.Html Msg
makeOption sel name =
    option [ onClick <| SelectChange name, A.selected <| sel == name ] [ text name ]
