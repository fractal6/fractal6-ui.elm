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
            (\x ->
                { message = msg
                , stopPropagation = True
                , preventDefault = True
                }
            )
            (JD.succeed msg)



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


onKeyCode : Int -> Bool -> Bool -> msg -> Html.Attribute msg
onKeyCode expectedCode a b msg =
    let
        isExpectedCode currentCode =
            if currentCode == expectedCode then
                JD.succeed msg

            else
                JD.fail "silent failure"
    in
    --on "keydown"
    custom "keydown" <|
        JD.map
            (\x ->
                { message = msg
                , stopPropagation = a
                , preventDefault = b
                }
            )
            (JD.andThen isExpectedCode keyCode)


onEnter : Bool -> Bool -> msg -> Html.Attribute msg
onEnter a b msg =
    onKeyCode 13 a b msg


onTab : Bool -> Bool -> msg -> Html.Attribute msg
onTab a b msg =
    onKeyCode 9 a b msg



--
--onKeyup : Bool -> Bool -> msg -> Html.Attribute msg
--onKeyup a b msg =
--    onKeyCode 38 a b msg
--
--
--onKeydown : Bool -> Bool -> msg -> Html.Attribute msg
--onKeydown a b msg =
--    onKeyCode 40 a b msg
--
