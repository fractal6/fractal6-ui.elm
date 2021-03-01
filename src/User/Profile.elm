module User.Profile exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Auth exposing (AuthState(..), doRefreshToken, refreshAuthModal)
import Browser.Navigation as Nav
import Codecs exposing (QuickDoc)
import Components.HelperBar as HelperBar
import Components.Loading as Loading exposing (GqlData, ModalData, RequestResult(..), WebData, viewAuthNeeded, viewGqlErrors, viewHttpErrors)
import Date exposing (formatTime)
import Dict exposing (Dict)
import Extra exposing (ternary)
import Form exposing (isPostSendable)
import Form.Help as Help
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Generated.Route as Route exposing (Route, toHref)
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, li, nav, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, placeholder, rows, type_)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Icon as I
import Iso8601 exposing (fromTime)
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Codecs exposing (FractalBaseRoute(..), NodeFocus, uriFromNameid)
import ModelCommon.Requests exposing (getQuickDoc, login)
import ModelCommon.View exposing (getAvatar, roleColor)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.AddTension exposing (addOneTension)
import Query.QueryNode exposing (NodeExt, queryNodeExt)
import Query.QueryUser exposing (queryUctx)
import RemoteData exposing (RemoteData)
import Task
import Text as T exposing (textH, textT)
import Time
import Url exposing (Url)



---- PROGRAM ----


type alias Flags =
    { param1 : String }


page : Page Flags Model Msg
page =
    Page.component
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


mapGlobalOutcmds : List GlobalCmd -> ( List (Cmd Msg), List (Cmd Global.Msg) )
mapGlobalOutcmds gcmds =
    gcmds
        |> List.map
            (\m ->
                case m of
                    DoNavigate link ->
                        ( send (Navigate link), Cmd.none )

                    DoAuth uctx ->
                        ( send (DoOpenAuthModal uctx), Cmd.none )

                    DoUpdateToken ->
                        ( Cmd.none, send UpdateUserToken )

                    _ ->
                        ( Cmd.none, Cmd.none )
            )
        |> List.unzip



---- MODEL----


type alias Model =
    { username : String
    , user : GqlData UserCtx
    , user_data : UserDict
    , uctx_m : Maybe UserCtx

    -- Common
    , modalAuth : ModalAuth
    , help : Help.State
    , refresh_trial : Int
    }


type alias UserDict =
    Dict String UserData


{-| User organisation media
-}
type alias UserData =
    { root : GqlData NodeExt
    , roles : List UserRole
    }


buildUserDict : UserCtx -> UserDict
buildUserDict uctx =
    let
        toTuples : UserRole -> List ( String, UserData )
        toTuples role =
            [ ( role.rootnameid, UserData NotAsked [ role ] ) ]

        toDict : List ( String, UserData ) -> Dict String UserData
        toDict tupleList =
            List.foldl
                (\( k, v ) dict -> Dict.update k (addParam v) dict)
                Dict.empty
                tupleList

        addParam : UserData -> Maybe UserData -> Maybe UserData
        addParam value maybeData =
            case maybeData of
                Just data ->
                    Just { data | roles = data.roles ++ value.roles }

                Nothing ->
                    Just (UserData NotAsked value.roles)
    in
    uctx.roles
        |> List.concatMap toTuples
        |> toDict



---- MSG ----


type Msg
    = PassedSlowLoadTreshold -- timer
    | Submit (Time.Posix -> Msg) -- Get Current Time
    | PushTension TensionForm (GqlData Tension -> Msg)
    | LoadNodes
    | GotNodes (GqlData (List NodeExt))
    | GotUctx (GqlData UserCtx)
      -- Token refresh
    | DoOpenAuthModal UserCtx -- ports receive / Open  modal
    | DoCloseAuthModal -- ports receive / Close modal
    | ChangeAuthPost String String
    | SubmitUser UserAuthForm
    | GotSignin (WebData UserCtx)
    | SubmitKeyDown Int -- Detect Enter (for form sending)
      -- Common
    | NoMsg
    | LogErr String
    | Navigate String
    | DoOpenModal -- ports receive / Open  modal
    | DoCloseModal ModalData -- ports receive / Close modal
      -- Help
    | HelpMsg Help.Msg



-- INIT --


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        apis =
            global.session.apis

        username =
            flags.param1 |> Url.percentDecode |> withDefault ""

        uctx_m =
            case global.session.user of
                LoggedIn uctx ->
                    Just uctx

                LoggedOut ->
                    Nothing

        uctx_data =
            case uctx_m of
                Just uctx ->
                    if uctx.username == username then
                        Success uctx

                    else
                        Loading

                Nothing ->
                    Loading

        model =
            { username = username
            , user = uctx_data
            , user_data =
                case uctx_data of
                    Success uctx ->
                        buildUserDict uctx

                    _ ->
                        Dict.empty
            , uctx_m = uctx_m

            -- common
            , modalAuth = Inactive
            , help = Help.init global.session.user
            , refresh_trial = 0
            }

        cmds =
            [ case uctx_data of
                Success uctx ->
                    send LoadNodes

                _ ->
                    queryUctx apis.gql username GotUctx
            , sendSleep PassedSlowLoadTreshold 500
            ]
    in
    ( model
    , Cmd.batch cmds
    , Cmd.none
    )



---- UPDATE ----


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global message model =
    let
        apis =
            global.session.apis
    in
    case message of
        PushTension form ack ->
            ( model, addOneTension apis.gql form ack, Cmd.none )

        LoadNodes ->
            ( model, queryNodeExt apis.gql (Dict.keys model.user_data) GotNodes, Cmd.none )

        PassedSlowLoadTreshold ->
            let
                user =
                    ternary (model.user == Loading) LoadingSlowly model.user
            in
            ( { model | user = user }, Cmd.none, Cmd.none )

        Submit nextMsg ->
            ( model, Task.perform nextMsg Time.now, Cmd.none )

        GotNodes result ->
            case doRefreshToken result model.refresh_trial of
                Authenticate ->
                    ( model, send (DoOpenAuthModal (withDefault initUserctx model.uctx_m)), Cmd.none )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, sendSleep LoadNodes 500, send UpdateUserToken )

                OkAuth data ->
                    let
                        resDict =
                            data
                                |> List.map (\n -> ( n.nameid, n ))
                                |> Dict.fromList

                        newUD =
                            model.user_data
                                |> Dict.map
                                    (\k v ->
                                        { v
                                            | root =
                                                case Dict.get k resDict of
                                                    Just n ->
                                                        Success n

                                                    Nothing ->
                                                        Failure [ "no root node found" ]
                                        }
                                    )
                    in
                    ( { model | user_data = newUD }, Cmd.none, Cmd.none )

                NoAuth ->
                    ( model, Cmd.none, Cmd.none )

        GotUctx result ->
            case result of
                Success uctx ->
                    let
                        user_data =
                            buildUserDict uctx
                    in
                    ( { model | user = result, user_data = user_data }, send LoadNodes, Cmd.none )

                _ ->
                    ( { model | user = result }, Cmd.none, Cmd.none )

        -- Token refresh
        DoOpenAuthModal uctx ->
            ( { model
                | modalAuth =
                    Active
                        { post = Dict.fromList [ ( "username", uctx.username ) ]
                        , result = RemoteData.NotAsked
                        }
              }
            , Cmd.none
            , Ports.open_auth_modal
            )

        DoCloseAuthModal ->
            ( { model | modalAuth = Inactive }, Cmd.none, Ports.close_auth_modal )

        ChangeAuthPost field value ->
            case model.modalAuth of
                Active form ->
                    let
                        newForm =
                            { form | post = Dict.insert field value form.post }
                    in
                    ( { model | modalAuth = Active newForm }, Cmd.none, Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        SubmitUser form ->
            ( model, login apis.auth form.post GotSignin, Cmd.none )

        GotSignin result ->
            case result of
                RemoteData.Success uctx ->
                    ( { model | modalAuth = Inactive }
                    , Cmd.batch [ send DoCloseAuthModal, send LoadNodes ]
                    , send (UpdateUserSession uctx)
                    )

                other ->
                    case model.modalAuth of
                        Active form ->
                            ( { model | modalAuth = Active { form | result = result } }, Cmd.none, Cmd.none )

                        Inactive ->
                            ( model, Cmd.none, Cmd.none )

        SubmitKeyDown key ->
            case key of
                13 ->
                    let
                        form =
                            case model.modalAuth of
                                Active f ->
                                    f

                                Inactive ->
                                    UserAuthForm Dict.empty RemoteData.NotAsked
                    in
                    --ENTER
                    if isPostSendable [ "password" ] form.post then
                        ( model, send (SubmitUser form), Cmd.none )

                    else
                        ( model, Cmd.none, Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        -- Common
        NoMsg ->
            ( model, Cmd.none, Cmd.none )

        LogErr err ->
            ( model, Ports.logErr err, Cmd.none )

        Navigate url ->
            ( model, Cmd.none, Nav.pushUrl global.key url )

        DoOpenModal ->
            ( model, Ports.open_modal "actionModal", Cmd.none )

        DoCloseModal data ->
            let
                gcmd =
                    if data.link /= "" then
                        send (Navigate data.link)

                    else
                        Cmd.none
            in
            ( model, gcmd, Ports.close_modal )

        -- Help
        HelpMsg msg ->
            let
                ( help, out ) =
                    Help.update apis msg model.help

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | help = help }, out.cmds |> List.map (\m -> Cmd.map HelpMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions global model =
    [ Ports.mcPD Ports.closeModalFromJs LogErr DoCloseModal
    ]
        ++ (Help.subscriptions |> List.map (\s -> Sub.map HelpMsg s))
        |> Sub.batch



---- VIEW ----


view : Global.Model -> Model -> Document Msg
view global model =
    { title = model.username
    , body =
        [ case model.user of
            Success user ->
                view_ global model user

            NotAsked ->
                text ""

            Loading ->
                text ""

            LoadingSlowly ->
                div [ class "spinner" ] []

            Failure err ->
                viewGqlErrors err
        , Help.view {} model.help |> Html.map HelpMsg
        , case model.modalAuth of
            -- @debug: should not be necessary...
            Active _ ->
                refreshAuthModal model.modalAuth { closeModal = DoCloseAuthModal, changePost = ChangeAuthPost, submit = SubmitUser, submitEnter = SubmitKeyDown }

            Inactive ->
                text ""
        ]
    }


view_ : Global.Model -> Model -> UserCtx -> Html Msg
view_ global model uctx =
    div [ id "profile", class "section" ]
        [ div [ class "columns" ]
            [ div [ class "column is-3" ]
                [ div [ class "columns is-centered" ]
                    [ viewProfileLeft model uctx ]
                ]
            , div [ class "column is-7 " ]
                [ viewProfileRight model uctx ]
            ]
        ]


viewProfileLeft : Model -> UserCtx -> Html Msg
viewProfileLeft model uctx =
    div []
        [ div [ class "content image circleBase circle3" ] [ getAvatar uctx.username ]
        , div [ class "content" ]
            [ case uctx.name of
                Just name ->
                    div [ class "title is-4" ] [ text name ]

                Nothing ->
                    div [] []
            , div [ class "is-size-5" ] [ text ("@" ++ uctx.username) ]
            ]
        ]


viewProfileRight : Model -> UserCtx -> Html Msg
viewProfileRight model uctx =
    div []
        [ h1 [ class "subtitle" ] [ textH T.organisations ]
        , if Dict.isEmpty model.user_data then
            p [ class "section" ] <|
                List.intersperse (text " ")
                    [ text "You have no organisations yet."
                    , br [] []
                    , text "You can"
                    , a [ href (Route.toHref Route.Explore) ] [ text "Explore" ]
                    , text "public organisations"
                    , text ", or create a"
                    , a [ href (Route.toHref Route.New_Orga) ] [ textH T.newOrganisation ]
                    ]

          else
            viewUserOrgas model.user_data
        ]


viewUserOrgas : UserDict -> Html Msg
viewUserOrgas user_data =
    Dict.values user_data
        |> List.map
            (\ud ->
                case ud.root of
                    Success root ->
                        let
                            n_member =
                                root.stats |> Maybe.map (\s -> s.n_member |> withDefault 0) |> withDefault 0 |> String.fromInt

                            n_guest =
                                root.stats |> Maybe.map (\s -> s.n_guest |> withDefault 0) |> withDefault 0 |> String.fromInt
                        in
                        div [ class "media box" ]
                            [ div [ class "media-left" ]
                                [ a
                                    [ class "image circleBase circle2"
                                    , href (uriFromNameid OverviewBaseUri root.nameid)
                                    ]
                                    [ getAvatar root.name ]
                                ]
                            , div [ class "media-content" ]
                                [ div [ class "columns" ]
                                    [ div [ class "column is-8" ]
                                        [ a [ href (uriFromNameid OverviewBaseUri root.nameid) ] [ text root.name ]
                                        , case root.about of
                                            Just about ->
                                                p [ class "is-italic pt-1" ] [ text about ]

                                            Nothing ->
                                                text ""
                                        ]
                                    , div [ class "column is-4" ]
                                        [ div [ class "field is-grouped is-grouped-multiline is-pulled-right" ]
                                            [ div [ class "control" ]
                                                [ div [ class "tags has-addons" ]
                                                    [ span [ class "tag is-light" ] [ text "member" ]
                                                    , span [ class "tag is-white" ] [ text n_member ]
                                                    ]
                                                ]
                                            , div [ class "control" ]
                                                [ div [ class "tags has-addons" ]
                                                    [ span [ class "tag is-light" ] [ text "guest" ]
                                                    , span [ class "tag is-white" ] [ text n_guest ]
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ]
                                , div [ id "icons", class "level is-mobile" ]
                                    [ div [ class "level-left" ]
                                        [ if root.isPrivate then
                                            span [ class "level-item" ] [ I.icon "icon-lock" ]

                                          else
                                            text ""
                                        ]
                                    ]
                                , hr [] []
                                , div [ class "buttons" ] <|
                                    (ud.roles
                                        |> List.filter (\r -> r.role_type /= RoleType.Member)
                                        |> List.map
                                            (\r ->
                                                a
                                                    [ class ("button buttonRole is-small has-text-weight-semibold toolti has-tooltip-bottom is-" ++ roleColor r.role_type)
                                                    , attribute "data-tooltip" (r.name ++ " of " ++ getParentFragmentFromRole r)
                                                    , href <| uriFromNameid OverviewBaseUri r.nameid
                                                    ]
                                                    [ text r.name ]
                                            )
                                    )
                                ]
                            ]

                    Failure err ->
                        viewGqlErrors err

                    _ ->
                        div [ class "media box" ]
                            [ div [ class "media-content" ]
                                [ div [ class "columns" ]
                                    [ div [ class "column is-8" ]
                                        [ div [ class "ph-line is-0" ] []
                                        , div [ class "ph-line is-1" ] []
                                        ]
                                    ]
                                ]
                            ]
            )
        |> div [ class "nodesList" ]
