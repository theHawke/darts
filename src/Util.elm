module Util exposing (..)

import Array exposing (Array)


zip : List a -> List b -> List ( a, b )
zip =
    List.map2 (\a b -> ( a, b ))


even : Int -> Bool
even =
    modBy 2 >> (==) 0


modByF :
    Float
    -> Float
    -> Float
modByF x mod =
    -- floating point version of modBy
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


arrayUpdate : Array a -> Int -> (a -> a) -> Array a
arrayUpdate a i f =
    case Array.get i a of
        Just v ->
            Array.set i (f v) a

        Nothing ->
            a


arrayAdd : Array Int -> Int -> Int -> Array Int
arrayAdd a i n =
    arrayUpdate a i (\x -> x + n)


arraySub : Array Int -> Int -> Int -> Array Int
arraySub a i n =
    arrayUpdate a i (\x -> x - n)
