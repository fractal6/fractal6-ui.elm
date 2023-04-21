{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2023 Fractale Co

   This file is part of Fractale.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU Affero General Public License as
   published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Affero General Public License for more details.

   You should have received a copy of the GNU Affero General Public License
   along with Fractale.  If not, see <http://www.gnu.org/licenses/>.
-}


module Assets exposing (..)

import Assets.Logo as Logo
import Extra exposing (space_)
import Html exposing (Html, a, div, h1, h2, hr, i, p, small, span, text)
import Html.Attributes exposing (attribute, class, href)
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


error : String
error =
    image "error.jpg"


loading : String
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


logo0 : Html msg
logo0 =
    Lazy.lazy Logo.logo_fractal { h = "30", w = "38" }


logo1 : Html msg
logo1 =
    Lazy.lazy Logo.logo_fractal { h = "38", w = "54" }


logo2 : Html msg
logo2 =
    Lazy.lazy Logo.logo_fractal { h = "96", w = "128" }


logo_about : Html msg
logo_about =
    Lazy.lazy Logo.logo_about { h = "", w = "500" }


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
        [ h1 [ class "title" ] [ text T.pageNotFound ] ]


welcome : Html msg
welcome =
    p [ class "field content is-aligned-center" ]
        [ h2 [ class "mb-0 is-highlight-2" ] [ text T.welcome ]
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
