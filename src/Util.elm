module Util exposing (..)


zip : List a -> List b -> List ( a, b )
zip =
    List.map2 (\a b -> ( a, b ))


even : Int -> Bool
even =
    modBy 2 >> (==) 0


modByF :
    Float
    -> Float
    -> Float -- floating point version of modBy
modByF x mod =
    x - mod * (toFloat <| floor <| x / mod)


intersperseEvery : Int -> a -> List a -> List a
intersperseEvery n x ls =
    let
        helper m l =
            case l of
                [] ->
                    []

                e :: ll ->
                    if m == 0 then
                        x :: e :: helper (n - 1) ll

                    else
                        e :: helper (m - 1) ll
    in
    helper n ls


unzipAlternate : List a -> ( List a, List a )
unzipAlternate l =
    let
        helper ll la lb =
            case ll of
                [] ->
                    ( List.reverse la, List.reverse lb )

                a :: [] ->
                    helper [] (a :: la) lb

                a :: b :: lr ->
                    helper lr (a :: la) (b :: lb)
    in
    helper l [] []
