{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2026 Fractale Co

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


module Components.UserCard exposing (Msg, State, init, subscriptions, update, view)

{-| User hover card, shared by every avatar (see docs/avatars.md). Shown and positioned by `assets/js/avatars.js`.
-}

import Assets as A
import Dict exposing (Dict)
import Fractale.Codecs exposing (FractalBaseRoute(..), toLink)
import Fractale.View exposing (getAvatar2)
import Html exposing (Html, a, div, p, text)
import Html.Attributes exposing (attribute, class, href, id)
import Loading exposing (GqlData, RequestResult(..))
import Maybe exposing (withDefault)
import ModelSchema exposing (UserCard)
import Ports
import Query.QueryUser exposing (queryUserCard)
import Session exposing (Apis, SessionCommon)
import Utils.Html exposing (showIf, showMaybe)


type State
    = State Model


type alias Model =
    { username : Maybe String -- shown card
    , cards : Dict String (GqlData UserCard) -- one query per user per session
    }


init : State
init =
    State { username = Nothing, cards = Dict.empty }


type Msg
    = OnUserCard (Maybe String)
    | GotUserCard String (GqlData UserCard)


update : Apis -> Msg -> State -> ( State, Cmd Msg )
update apis msg (State model) =
    case msg of
        OnUserCard (Just username) ->
            if Dict.member username model.cards then
                ( State { model | username = Just username }, Cmd.none )

            else
                ( State { model | username = Just username, cards = Dict.insert username Loading model.cards }
                , queryUserCard apis username (GotUserCard username)
                )

        OnUserCard Nothing ->
            ( State { model | username = Nothing }, Cmd.none )

        GotUserCard username result ->
            ( State { model | cards = Dict.insert username result model.cards }, Cmd.none )


subscriptions : Sub Msg
subscriptions =
    Ports.userCardFromJs OnUserCard


{-| No events: rendered by Global.view in the page's msg.
-}
view : SessionCommon -> State -> Html msg
view session (State model) =
    showMaybe model.username (\u -> viewCard session u (Dict.get u model.cards |> withDefault Loading))


viewCard : SessionCommon -> String -> GqlData UserCard -> Html msg
viewCard session username data =
    let
        link =
            toLink UsersBaseUri username []

        user =
            case data of
                Success u ->
                    u

                _ ->
                    { username = username, name = Nothing, avatar = Nothing, bio = Nothing, location = Nothing }
    in
    div [ id "userCard", class "box", attribute "data-username" username ]
        [ div [ class "media" ]
            [ div [ class "media-left" ] [ a [ href link ] [ getAvatar2 session user ] ]
            , div [ class "media-content" ]
                [ showMaybe user.name (\n -> a [ href link, class "is-strong is-block" ] [ text n ])
                , a [ href link, class "is-discrete is-block" ] [ text ("@" ++ username) ]
                ]
            ]
        , showMaybe user.bio (\b -> p [ class "mt-2" ] [ text b ])
        , showMaybe user.location (\l -> p [ class "mt-2 is-discrete is-size-7" ] [ A.icon1 "icon-map-pin" l ])
        , showIf (Loading.isLoading data) (div [ class "is-loading-text is-discrete is-size-7" ] [ text "..." ])
        ]
