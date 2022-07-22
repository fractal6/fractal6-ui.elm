module Assets exposing (..)

import Assets.Logo as Logo
import Extra exposing (space_, ternary, textH, upH)
import Generated.Route as Route exposing (Route, toHref)
import Html exposing (Html, a, div, h1, h2, hr, i, p, small, span, text)
import Html.Attributes exposing (attribute, class, href, style)
import Html.Lazy as Lazy
import String.Format as Format
import Text as T


{-| Assets, such as images, videos, and audio. (We only have images for now.)

We should never expose asset URLs directly; this module should be in charge of
all of them. One source of truth!

-}
type Image
    = Image String



-- IMAGES


image : String -> String
image filename =
    "/assets/images/" ++ filename


error =
    image "error.jpg"


loading =
    image "loading.svg"



-- ICONS


icon : String -> Html msg
icon cls =
    i [ class cls ] []


icon0 : String -> Html msg
icon0 cls =
    span [] [ icon cls, text space_ ]


icon1 : String -> String -> Html msg
icon1 cls words =
    span [] [ icon cls, text (space_ ++ space_ ++ words) ]



-- VIEWS


logo0 : String -> Html msg
logo0 color =
    Lazy.lazy Logo.logo_fractal { color = color, h = "30", w = "38" }


logo1 : String -> Html msg
logo1 color =
    Lazy.lazy Logo.logo_fractal { color = color, h = "38", w = "54" }


logo2 : String -> Html msg
logo2 color =
    Lazy.lazy Logo.logo_fractal { color = color, h = "96", w = "128" }


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


viewNotFound : Html msg
viewNotFound =
    div [ class "section" ]
        [ h1 [ class "title" ] [ text "Sorry, Page Not found" ]
        ]


welcome : Html msg
welcome =
    p [ class "field content is-aligned-center" ]
        [ h2 [ class "mb-0 is-highlight-3" ] [ text "Welcome to Fractale" ]
        , p [ class "has-text-grey-light" ] [ text "a collective intelligence platform." ]
        ]


almostThere : String -> String -> String -> Html msg
almostThere username aim link =
    div []
        [ div [ class "title is-aligned-center" ] [ textH (T.almostThere ++ "...") ]
        , div [ class "notification is-light is-info" ]
            [ textH ("Please check your email ({{}}) {{}}." |> Format.value username |> Format.value aim)
            , hr [ class "has-background-grey-light mt-5 mb-5" ] []
            , small [] [ textH "No confirmation email received? Check your spam folder or ", a [ href link ] [ text "request new confirmation email." ] ]
            ]
        ]
