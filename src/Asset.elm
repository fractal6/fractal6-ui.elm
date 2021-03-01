module Asset exposing (..)

import Html exposing (Html, a, div, h1, text)
import Html.Attributes exposing (class, href)


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



--logo =
--    Logo.logo_fractale


viewNotFound : Html msg
viewNotFound =
    div [ class "section" ]
        [ h1 [ class "title" ] [ text "Sorry, Page Not found" ]
        ]
