module Fractale.Welcome exposing (view)

import Assets as A
import Components.ActionCard as ActionCard
import Generated.Route as Route exposing (toHref)
import Html exposing (Attribute, Html, div, h1, p, span, text)
import Html.Attributes exposing (attribute, class)
import String.Format as Format
import Text as T


{-| The modal supplies close-and-navigate handlers; the page leaves links native.
-}
view : { username : String, linkAttributes : String -> List (Attribute msg) } -> Html msg
view options =
    let
        createLink =
            toHref Route.New_Orga

        exploreLink =
            toHref Route.Explore
    in
    div []
        [ div [ class "has-text-centered mb-5" ]
            [ span [ attribute "aria-hidden" "true" ] [ A.icon "icon-check-circle icon-2x has-text-success" ]
            , h1 [ class "title is-4 mt-3 mb-1" ]
                [ text (T.welcomeUser |> Format.namedValue "username" options.username) ]
            , p [ class "is-discrete" ] [ text T.accountActivated ]
            ]
        , div [ class "columns is-multiline" ]
            [ div [ class "column is-half" ]
                [ ActionCard.view (options.linkAttributes createLink)
                    { icon = "icon-git-branch"
                    , title = T.newOrganisation
                    , description = T.createOrgaHint
                    , featured = True
                    , action = ActionCard.Link createLink
                    }
                ]
            , div [ class "column is-half" ]
                [ ActionCard.view (options.linkAttributes exploreLink)
                    { icon = "icon-globe"
                    , title = T.explore
                    , description = T.exploreOrgaHint
                    , featured = False
                    , action = ActionCard.Link exploreLink
                    }
                ]
            ]
        ]
