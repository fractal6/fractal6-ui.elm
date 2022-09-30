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


logo_about : String -> Html msg
logo_about color =
    Lazy.lazy Logo.logo_about { color = color, h = "", w = "500" }


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
        [ h1 [ class "title" ] [ text T.pageNotFound ]
        ]


welcome : Html msg
welcome =
    p [ class "field content is-aligned-center" ]
        [ h2 [ class "mb-0 is-highlight-3" ] [ text T.welcome ]
        , p [ class "has-text-grey-light" ] [ text T.welcomeSub ]
        ]


almostThere : String -> String -> String -> Html msg
almostThere username aim link =
    div []
        [ div [ class "title is-aligned-center" ] [ text (T.almostThere ++ "...") ]
        , div [ class "notification is-light is-info" ]
            [ text (T.checkYourEmail |> Format.value username |> Format.value aim)
            , hr [ class "has-background-grey-light mt-5 mb-5" ] []
            , small []
                [ text T.checkConfirmationEmail
                , text (" " ++ T.or_ ++ " ")
                , a [ href link ] [ text T.requestNewEmail ]
                ]
            ]
        ]
