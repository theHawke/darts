module Dartboard exposing (Dartboard(..), dartboard)

import Css exposing (fontSize, hover, int, opacity, px, scale, transform, transforms, translate2)
import Html.Styled exposing (Html, div)
import Svg.Styled exposing (Attribute, Svg, circle, g, path, svg, text, text_)
import Svg.Styled.Attributes exposing (css, cx, cy, d, fill, height, r, stroke, strokeWidth, viewBox, width, x, y)
import Svg.Styled.Events exposing (onClick)
import Util exposing (even, modByF, zip)


type Dartboard
    = Bull
    | DBull
    | G Int
    | K Int
    | D Int
    | T Int
    | N


dartboard : Int -> List (Attribute Dartboard) -> Html Dartboard
dartboard size attr =
    div attr
        [ svg [ width (String.fromInt size), height (String.fromInt size), viewBox "-1.5 -1.5 3 3" ]
            (circle [ cx "0", cy "0", r "1.3", fill "black", onClick N ] []
                :: List.map label segments
                ++ List.map (\n -> segment 0 0 1 False n []) segments
                ++ [ circle [ cx "0", cy "0", r "0.15", fill "green", stroke "grey", strokeWidth "0.01", onClick Bull, css [ hover [ transform (scale 1.1) ] ] ] []
                   , circle [ cx "0", cy "0", r "0.07", fill "red", stroke "grey", strokeWidth "0.01", onClick DBull, css [ hover [ transform (scale 1.15) ] ] ] []
                   ]
                ++ List.map (\n -> segment 0 0 1 True n []) segments
            )
        ]


label : ( Int, Int ) -> Svg alias
label ( pos, num ) =
    let
        h =
            0.14

        ( xx, yy ) =
            fromPolar ( 1.15, degrees <| 18.0 * toFloat pos - 90 )

        cx =
            if num < 10 then
                xx - 0.04

            else
                xx - 0.08

        cy =
            yy + h / 2.5
    in
    text_ [ x <| String.fromFloat cx, y <| String.fromFloat cy, fill "white", css [ fontSize <| px h ] ] [ text <| String.fromInt num ]


segment : Float -> Float -> Float -> Bool -> ( Int, Int ) -> List (Attribute Dartboard) -> Svg Dartboard
segment cx cy r top ( pos, num ) attr =
    let
        normal =
            if even pos then
                "black"

            else
                "#ffeecc"

        special =
            if even pos then
                "red"

            else
                "green"

        r1 =
            0.15 * r

        r2 =
            0.5 * r

        r3 =
            0.6 * r

        r4 =
            0.9 * r

        r5 =
            1.0 * r

        a0 =
            18 * toFloat pos - 9

        a1 =
            18 * toFloat pos + 9

        layer_css =
            css
                (if top then
                    [ opacity (int 0), hover [ opacity (int 1) ] ]

                 else
                    []
                )
    in
    g ([] ++ attr)
        [ circleArc cx cy r1 r2 a0 a1 top [ fill normal, stroke "grey", strokeWidth "0.01", onClick (K num), layer_css ]
        , circleArc cx cy r2 r3 a0 a1 top [ fill special, stroke "grey", strokeWidth "0.01", onClick (T num), layer_css ]
        , circleArc cx cy r3 r4 a0 a1 top [ fill normal, stroke "grey", strokeWidth "0.01", onClick (G num), layer_css ]
        , circleArc cx cy r4 r5 a0 a1 top [ fill special, stroke "grey", strokeWidth "0.01", onClick (D num), layer_css ]
        ]


circleArc : Float -> Float -> Float -> Float -> Float -> Float -> Bool -> List (Attribute a) -> Svg a
circleArc cx cy ri ro a0 a1 hover_zoom attr =
    let
        rm =
            (ri + ro) / 2

        am =
            a0 + modByF (a1 - a0) 360 / 2

        ox =
            cx + rm * sin (degrees am)

        oy =
            cy - rm * cos (degrees am)

        x0 =
            String.fromFloat <| cx + ro * sin (degrees a0)

        y0 =
            String.fromFloat <| cy - ro * cos (degrees a0)

        x1 =
            String.fromFloat <| cx + ro * sin (degrees a1)

        y1 =
            String.fromFloat <| cy - ro * cos (degrees a1)

        x2 =
            String.fromFloat <| cx + ri * sin (degrees a1)

        y2 =
            String.fromFloat <| cy - ri * cos (degrees a1)

        x3 =
            String.fromFloat <| cx + ri * sin (degrees a0)

        y3 =
            String.fromFloat <| cy - ri * cos (degrees a0)

        largeArc =
            if modByF (a1 - a0) 360 > 180 then
                "1"

            else
                "0"

        hover_css =
            css
                (if hover_zoom then
                    [ hover [ transforms [ scale 1.1, translate2 (px -(0.1 * ox / 1.1)) (px -(0.1 * oy / 1.1)) ] ] ]

                 else
                    []
                )
    in
    path
        ([ d <|
            String.concat
                [ "M ", x0, " ", y0, " A ", String.fromFloat ro, " ", String.fromFloat ro, " 0 ", largeArc, " 1 ", x1, " ", y1, " L ", x2, " ", y2, " A ", String.fromFloat ri, " ", String.fromFloat ri, " 0 ", largeArc, " 0 ", x3, " ", y3, " Z" ]
         , hover_css
         ]
            ++ attr
        )
        []


segments : List ( Int, Int )
segments =
    zip (List.range 0 19) [ 20, 1, 18, 4, 13, 6, 10, 15, 2, 17, 3, 19, 7, 16, 8, 11, 14, 9, 12, 5 ]
