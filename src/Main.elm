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
    }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( global, globalCmd ) =
            Global.init flags url key

        ( page, pageCmd, pageGlobalCmd ) =
            Pages.init (fromUrl url) global
    in
    ( Model key url global page
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
                    model.global

                ( page, pageCmd, globalCmd ) =
                    Pages.init (fromUrl url) { global | url = url }
            in
            ( { model | url = url, page = page }
            , Cmd.batch
                [ Cmd.map Page pageCmd
                , Cmd.map Global globalCmd
                , Ports.bulma_driver "" -- @warning: this also check if jwt cookie has expired !
                , Ports.show "footBar"
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
        , toMsg = Global
        }


fromUrl : Url -> Route
fromUrl =
    Route.fromUrl >> Maybe.withDefault Route.NotFound
