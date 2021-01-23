module Components.Asset exposing (Image, defaultAvatar, error, loading)

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



--logo =
--    Logo.logo_fractale


image : String -> String
image filename =
    "/assets/images/" ++ filename
