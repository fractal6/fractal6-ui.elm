{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2022 Fractale Co

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


module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav exposing (Key)
import Generated.Pages as Pages
import Generated.Route as Route exposing (Route(..))
import Global exposing (Msg(..))
import Html
import Ports
import Url exposing (Url)


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- INIT


type alias Flags =
    Global.Flags


type alias Model =
    { key : Key
    , url : Url
    , global : Global.Model
    , page : Pages.Model
    , nvt_msg1 : String -> Msg
    }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( global, globalCmd ) =
            Global.init flags url key

        ( page, pageCmd, pageGlobalCmd ) =
            Pages.init (fromUrl url) global
    in
    ( Model key url global page (Global << ReplaceUrl)
    , Cmd.batch
        [ Cmd.map Global globalCmd
        , Cmd.map Global pageGlobalCmd
        , Cmd.map Page pageCmd
        ]
    )


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | UrlChanged_ Url
    | Global Global.Msg
    | Page Pages.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked (Browser.Internal url) ->
            if Route.fromUrl url == Just Route.Logout then
                ( model, Nav.replaceUrl model.key (Url.toString url) )

            else
                ( model, Nav.pushUrl model.key (Url.toString url) )

        LinkClicked (Browser.External href) ->
            ( model, Nav.load href )

        UrlChanged url ->
            ( model
            , Cmd.batch
                [ Global.send (UrlChanged_ url)
                , Cmd.map Global (Global.send (UpdateReferer model.url))
                , Cmd.map Global Global.now
                ]
            )

        UrlChanged_ url ->
            let
                global =
                    model.global |> (\g -> { g | url = url })

                ( page, pageCmd, globalCmd ) =
                    Pages.init (fromUrl url) global
            in
            ( { model | url = url, page = page, global = global }
            , Cmd.batch
                [ Cmd.map Page pageCmd
                , Cmd.map Global globalCmd

                -- @warning: bulma_driver:
                -- * check if jwt cookie has expired !
                -- * activate the Subscrition triggers
                , Ports.bulma_driver ""
                , Ports.show "footBar"
                , if
                    (String.split "/" model.url.path |> List.take 2)
                        == (String.split "/" url.path |> List.take 2)
                  then
                    Cmd.none

                  else
                    Ports.resetScroll
                ]
            )

        Global globalMsg ->
            let
                ( global, globalCmd ) =
                    Global.update globalMsg model.global
            in
            ( { model | global = global }
            , Cmd.map Global globalCmd
            )

        Page pageMsg ->
            let
                ( page, pageCmd, globalCmd ) =
                    Pages.update pageMsg model.page model.global
            in
            ( { model | page = page }
            , Cmd.batch
                [ Cmd.map Page pageCmd
                , Cmd.map Global globalCmd
                ]
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ model.global
            |> Global.subscriptions
            |> Sub.map Global
        , model.page
            |> (\page -> Pages.subscriptions page model.global)
            |> Sub.map Page
        ]


view : Model -> Browser.Document Msg
view model =
    let
        documentMap :
            (msg1 -> msg2)
            -> Document msg1
            -> Document msg2
        documentMap fn doc =
            { title = doc.title
            , body = List.map (Html.map fn) doc.body
            }
    in
    Global.view
        { page = Pages.view model.page model.global |> documentMap Page
        , global = model.global
        , url = model.url -- @debug url change in global is not passed to Global.view. Why ?
        , msg1 = model.nvt_msg1
        }


fromUrl : Url -> Route
fromUrl =
    Route.fromUrl >> Maybe.withDefault Route.NotFound
