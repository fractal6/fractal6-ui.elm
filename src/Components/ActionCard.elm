module Components.ActionCard exposing (Action(..), Config, view)

import Assets as A
import Html exposing (Attribute, Html, a, button, span, text)
import Html.Attributes exposing (attribute, class, classList, href, type_)
import Html.Events exposing (onClick)


{-| Links navigate; buttons emit messages without submitting a surrounding form.
-}
type Action msg
    = Link String
    | Click msg


type alias Config msg =
    { icon : String
    , title : String
    , description : String
    , featured : Bool
    , action : Action msg
    }


view : List (Attribute msg) -> Config msg -> Html msg
view attributes config =
    let
        ( element, actionAttributes ) =
            case config.action of
                Link url ->
                    ( a, [ href url ] )

                Click msg ->
                    ( button, [ type_ "button", onClick msg ] )
    in
    element
        ([ class "action-card box media"
         , classList [ ( "is-featured", config.featured ) ]
         ]
            ++ actionAttributes
            ++ attributes
        )
        [ span [ class "media-left", attribute "aria-hidden" "true" ]
            [ span [ class "action-card-icon" ] [ A.icon config.icon ] ]
        , span [ class "media-content" ]
            [ span [ class "is-block has-text-weight-semibold has-text-strong" ] [ text config.title ]
            , span [ class "is-block is-size-7 is-discrete" ] [ text config.description ]
            ]
        ]
