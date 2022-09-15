module Extra.Events exposing (..)

import Html
import Html.Events exposing (custom, keyCode, on, preventDefaultOn, stopPropagationOn)
import Json.Decode as JD



-- Need the target attr to be set to _self as a workaround: https://github.com/elm/browser/issues/74


onClickPD : msg -> Html.Attribute msg
onClickPD msg =
    preventDefaultOn "click" <| JD.succeed ( msg, True )


onClickPD2 : msg -> Html.Attribute msg
onClickPD2 msg =
    custom "click" <|
        JD.map
            (\_ ->
                { message = msg
                , stopPropagation = True
                , preventDefault = True
                }
            )
            (JD.succeed msg)


onClickPos msg =
    --onClickPos : msg -> Html.Attribute msg
    on "click" (JD.map msg eventPos)


eventPos : JD.Decoder (Maybe ( Int, Int ))
eventPos =
    JD.maybe <|
        JD.map2 Tuple.pair
            --(JD.at [ "target", "offsetTop" ] JD.int)
            --(JD.at [ "target", "offsetLeft" ] JD.int)
            (JD.field "clientX" JD.int)
            (JD.field "clientY" JD.int)



-- onLoad


onLoad : msg -> Html.Attribute msg
onLoad msg =
    on "onrender" <| JD.succeed msg



-- Hot-key event


onKeydown : (Int -> msg) -> Html.Attribute msg
onKeydown msg =
    let
        decodeKey =
            JD.andThen JD.succeed keyCode
    in
    on "keydown" <| JD.map (\key -> msg key) decodeKey


onKeyup : (Int -> msg) -> Html.Attribute msg
onKeyup msg =
    let
        decodeKey =
            JD.andThen JD.succeed keyCode
    in
    on "keyup" <| JD.map (\key -> msg key) decodeKey


onKeyCode : Int -> Bool -> Bool -> msg -> Html.Attribute msg
onKeyCode expectedCode a b msg =
    --on "keyup"
    custom "keyup" <|
        JD.map
            (\_ ->
                { message = msg
                , stopPropagation = a
                , preventDefault = b
                }
            )
            (JD.andThen
                (\code ->
                    if code == expectedCode then
                        JD.succeed msg

                    else
                        JD.fail (String.fromInt code)
                )
                keyCode
            )


onEnter : msg -> Html.Attribute msg
onEnter msg =
    onKeyCode 13 False False msg


onTab : msg -> Html.Attribute msg
onTab msg =
    onKeyCode 9 False False msg
