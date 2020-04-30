module Extra exposing (onClickLink, onClickPD)

import Html
import Html.Events exposing (custom, preventDefaultOn)
import Json.Decode as JD exposing (Value, decodeValue)



--
-- Special Event
--
-- Doesn'tWork


eventConfig : Bool -> Bool -> msg -> { message : msg, stopPropagation : Bool, preventDefault : Bool }
eventConfig stopPropagation preventDefault msg =
    { message = msg
    , stopPropagation = stopPropagation
    , preventDefault = preventDefault
    }


onClickLink : msg -> Html.Attribute msg
onClickLink msg =
    custom "onclick" <| JD.map (eventConfig True True) (JD.succeed msg)



-- Need the target attr to be set to _self as a workaround: https://github.com/elm/browser/issues/74


onClickPD : msg -> Html.Attribute msg
onClickPD msg =
    preventDefaultOn "click" <| JD.succeed ( msg, True )
