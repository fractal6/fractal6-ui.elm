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


module Pages.About exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Assets as A
import Browser.Navigation as Nav
import Form.Help as Help
import Fractale.Codecs exposing (FractalBaseRoute(..), toLink)
import Fractale.User exposing (UserState(..))
import Global exposing (Msg(..))
import Html exposing (Html, dd, div, dl, dt, figcaption, figure, h1, h2, hr, i, iframe, img, nav, strong, text, textarea)
import Html.Attributes exposing (alt, class, height, id, rows, src, style, title, width)
import Http
import Markdown exposing (renderMarkdown)
import Page exposing (Document, Page)
import Pages.Login as Login
import Ports
import Requests exposing (fetchStaticPage)
import Schema.Enum.Lang as Lang
import Session exposing (GlobalCmd(..))
import Text as T
import Utils.Bool exposing (ternary)
import Utils.Cmd exposing (send)
import Utils.String exposing (upH)



---- PROGRAM ----


page : Page Flags Model Msg
page =
    Page.component
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Flags =
    ()


mapGlobalOutcmds : List GlobalCmd -> ( List (Cmd Msg), List (Cmd Global.Msg) )
mapGlobalOutcmds gcmds =
    gcmds
        |> List.map
            (\m ->
                case m of
                    DoNavigate link ->
                        ( Cmd.none, send (NavigateRaw link) )

                    DoReplaceUrl url ->
                        ( Cmd.none, send (ReplaceUrl url) )

                    DoUpdateToken ->
                        ( Cmd.none, send UpdateUserToken )

                    DoUpdateUserSession uctx ->
                        ( Cmd.none, send (UpdateUserSession uctx) )

                    _ ->
                        ( Cmd.none, Cmd.none )
            )
        |> List.unzip



---- MODEL ----


type alias Model =
    { signBox : Login.Model
    , lang : Lang.Lang
    , isHome : Bool
    , empty : {}
    , staticContent : StaticContentState

    -- Commons
    , help : Help.State
    }


type StaticContentState
    = NotLoaded
    | LoadingStatic
    | StaticSuccess String
    | StaticFailure



---- INIT ----


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        isHome =
            global.url.path == "/"

        gcmd =
            case global.session.common.user of
                LoggedIn uctx ->
                    if isHome then
                        Nav.replaceUrl global.key <| toLink UsersBaseUri uctx.username []

                    else
                        Cmd.none

                LoggedOut ->
                    Cmd.none

        model =
            { signBox = Login.initModel global
            , lang = global.session.common.lang
            , isHome = isHome
            , empty = {}
            , staticContent = NotLoaded
            , help = Help.init global.session.common
            }

        -- Fetch static welcome content if not logged in
        fetchCmd =
            Cmd.none
    in
    ( model
    , fetchCmd
    , Cmd.batch [ gcmd, send (UpdateSessionFocus Nothing) ]
    )



---- MSG ----


type Msg
    = LoginMsg Login.Msg
    | GotStaticContent String (Result Http.Error String)
    | HelpMsg Help.Msg



---- UPDATE ----


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global message model =
    let
        apis =
            global.session.apis
    in
    case message of
        LoginMsg msg ->
            let
                ( signBox, cmd, gcmd ) =
                    Login.update global msg model.signBox
            in
            ( { model | signBox = signBox }, Cmd.map LoginMsg cmd, gcmd )

        GotStaticContent elementId result ->
            case result of
                Ok content ->
                    ( { model | staticContent = StaticSuccess content }
                    , Ports.setInnerHtml { id = elementId, html = content }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | staticContent = StaticFailure }, Cmd.none, Cmd.none )

        HelpMsg msg ->
            let
                ( data, out ) =
                    Help.update apis msg model.help

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | help = data }, out.cmds |> List.map (\m -> Cmd.map HelpMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions _ _ =
    []
        ++ (Help.subscriptions |> List.map (\s -> Sub.map HelpMsg s))
        |> Sub.batch



---- VIEW ----


view : Global.Model -> Model -> Document Msg
view global model =
    { title = T.welcome ++ " - " ++ T.welcome2
    , body =
        [ view_ global model
        , Help.view model.empty model.help |> Html.map HelpMsg
        ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    div [ id "welcome", class "top-section" ]
        [ div [ id "welcome-1" ] [ viewHero model ]
        , viewStaticContent model
        ]


viewHero : Model -> Html Msg
viewHero model =
    div []
        [ div [ class "columns is-centered custom-b-1 m-0" ]
            [ div [ class "column is-7-desktop is-7-widescreen is-6-fullhd" ]
                [ h1 [ class "title top-title" ]
                    [ renderMarkdown "" "is-huma" T.heroTitle ]

                --[ text "Build unstoppable organisations" ]
                --[ text "Self-organisation", br [] [], text "for humans" ]
                --[ text "Collective Intelligence", br [] [], text "at Work" ]
                , div [ class "columns is-vcentered" ]
                    [ div [ class "column is-7", style "min-width" "300px" ]
                        [ h2 [ class "subtitle" ] [ renderMarkdown "" "" T.heroSubtitle ] ]
                    ]
                ]
            , div [ class "column is-5-desktop is-4-widescreen is-3-fullhd is-hidden-mobile" ]
                [ if model.isHome then
                    Login.viewCard model.signBox |> Html.map LoginMsg

                  else
                    --div [ class "has-text-centered" ] [ A.logo_about "#34663d" ]
                    --div [ class "has-text-centered" ] [ A.logo_about "#7888ae" ]
                    div [ class "logo" ] [ A.logo_about ]
                ]
            ]
        ]


viewStaticContent : Model -> Html Msg
viewStaticContent model =
    case model.staticContent of
        StaticSuccess content ->
            div [ id "static-welcome" ] []

        LoadingStatic ->
            text ""

        NotLoaded ->
            text ""

        StaticFailure ->
            text ""
