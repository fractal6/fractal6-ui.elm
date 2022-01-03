module Assets exposing (..)

import Assets.Logo as Logo
import Html exposing (Html, a, div, h1, h2, i, p, span, text)
import Html.Attributes exposing (attribute, class, href, style)
import Text as T


{-| Assets, such as images, videos, and audio. (We only have images for now.)

We should never expose asset URLs directly; this module should be in charge of
all of them. One source of truth!

-}
type Image
    = Image String



-- IMAGES


error =
    image "error.jpg"


loading =
    image "loading.svg"


defaultAvatar =
    image "smiley-cyrus.jpg"


image : String -> String
image filename =
    "/assets/images/" ++ filename



-- ICONS


icon : String -> Html msg
icon cls =
    i [ class cls ] []


icon0 : String -> Html msg
icon0 cls =
    span [] [ icon cls, text T.space_ ]


icon1 : String -> String -> Html msg
icon1 cls words =
    span [] [ icon cls, text (T.space_ ++ T.space_ ++ words) ]



-- VIEWS


logo0 : String -> Html msg
logo0 color =
    Logo.logo_fractal { color = color, h = "30", w = "38" }


logo1 : String -> Html msg
logo1 color =
    Logo.logo_fractal { color = color, h = "38", w = "54" }


logo2 : String -> Html msg
logo2 color =
    Logo.logo_fractal { color = color, h = "96", w = "128" }


{-| A burger button used when the terminal is in mobile/collapse state.
-}
burger : String -> Html msg
burger targetid =
    div
        [ class "burger navbar-burger"
        , attribute "data-target" targetid
        , attribute "aria-expanded" "false"
        , attribute "aria-label" "menu"
        , attribute "role" "button"
        ]
        [ span [ attribute "aria-hidden" "true" ] []
        , span [ attribute "aria-hidden" "true" ] []
        , span [ attribute "aria-hidden" "true" ] []
        ]


welcome : Html msg
welcome =
    p [ class "field content is-aligned-center" ]
        [ h2 [ class "mb-0 is-highlight-3" ] [ text "Welcome to Fractale" ]
        , p [ class "has-text-grey-light" ] [ text "a collective intelligence platform." ]
        ]


viewNotFound : Html msg
viewNotFound =
    div [ class "section" ]
        [ h1 [ class "title" ] [ text "Sorry, Page Not found" ]
        ]
