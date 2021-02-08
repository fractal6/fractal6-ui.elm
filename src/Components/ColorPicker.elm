module Components.ColorPicker exposing (..)

import Html exposing (Html, a, button, div, header, hr, i, nav, span, text)
import Html.Attributes as Attr exposing (attribute, class, classList, href, id, style)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Text as T exposing (textH, textT)


type alias ColorPicker =
    { isOpen : Bool
    , color : String
    , colors : List String
    }


init : ColorPicker
init =
    { isOpen = False
    , color = "#001f3f"
    , colors =
        [ "#001f3f"
        , "#0074D9"
        , "#7FDBFF"
        , "#39CCCC"
        , "#3D9970"
        , "#2ECC40"
        , "#01FF70"
        , "#FFDC00"
        , "#FF851B"
        , "#FF4136"
        , "#85144b"
        , "#F012BE"
        , "#B10DC9"
        , "#111111"
        , "#AAAAAA"
        , "#DDDDDD"
        ]
    }



-- State control


open : ColorPicker -> ColorPicker
open data =
    { data | isOpen = True }


close : ColorPicker -> ColorPicker
close data =
    { data | isOpen = False }


setColor : String -> ColorPicker -> ColorPicker
setColor color data =
    { data | color = color }


type alias Op msg =
    { data : ColorPicker
    , onOpen : msg
    , onClose : msg
    , onSelect : String -> msg
    }


view : Op msg -> Html msg
view op =
    span []
        [ button
            [ class "buttonColor"
            , attribute "style" ("background-color:" ++ op.data.color ++ ";")
            , onClick op.onOpen
            ]
            []
        , div
            [ id "colorPicker"
            , classList [ ( "is-hidden", op.data.isOpen == False ) ]
            ]
            [ div [ class "colorBoxes" ]
                [ span [ class "is-size-7" ]
                    [ text "Select a color:" ]
                , div []
                    (op.data.colors
                        |> List.map
                            (\c ->
                                button
                                    [ class "buttonColor"
                                    , onClick (op.onSelect c)
                                    , attribute "style" ("background-color:" ++ c ++ ";")
                                    ]
                                    []
                            )
                    )
                ]
            ]
        ]
