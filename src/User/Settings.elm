module User.Settings exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Browser.Navigation as Nav
import Codecs exposing (QuickDoc)
import Components.AuthModal as AuthModal
import Dict exposing (Dict)
import Extra exposing (mor, ternary)
import Extra.Events exposing (onClickPD, onEnter, onKeydown, onTab)
import Extra.Url exposing (queryBuilder, queryParser)
import Form exposing (isPostSendable, isPostSendableOr)
import Form.Help as Help
import Fractal.Enum.Lang as Lang
import Fractal.Enum.RoleType as RoleType
import Generated.Route as Route exposing (Route, toHref)
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, h3, h4, h5, h6, hr, i, input, label, li, nav, option, p, select, span, strong, text, textarea, ul)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, for, href, id, name, placeholder, rows, selected, spellcheck, style, target, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Html.Lazy as Lazy
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Loading exposing (GqlData, ModalData, RequestResult(..), WebData, viewAuthNeeded, viewGqlErrors, viewHttpErrors, withMaybeData)
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Codecs exposing (FractalBaseRoute(..), NodeFocus, getRoles, getRootids, nid2rootid, uriFromNameid)
import ModelCommon.View exposing (viewProfileC)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.PatchUser exposing (patchUser)
import Query.QueryNode exposing (queryNodeExt)
import Query.QueryUser exposing (queryUserFull)
import RemoteData exposing (RemoteData)
import Session exposing (GlobalCmd(..))
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



---- MODEL----


type alias Model =
    { username : String
    , user : GqlData UserFull
    , user_result : GqlData UserFull
    , menuFocus : MenuSettings
    , menuList : List MenuSettings
    , hasUnsavedData : Bool
    , switch_index : Int
    , form : UserProfileForm

    -- Common
    , help : Help.State
    , refresh_trial : Int
    , authModal : AuthModal.State
    , empty : {}
    }


type MenuSettings
    = ProfileMenu
    | EmailMenu


menuList : List MenuSettings
menuList =
    [ ProfileMenu, EmailMenu ]


menuEncoder : MenuSettings -> String
menuEncoder menu =
    case menu of
        ProfileMenu ->
            "profile"

        EmailMenu ->
            "email"


menuDecoder : String -> MenuSettings
menuDecoder menu =
    case menu of
        "profile" ->
            ProfileMenu

        "email" ->
            EmailMenu

        _ ->
            ProfileMenu


menuToString : MenuSettings -> ( String, String )
menuToString menu =
    case menu of
        ProfileMenu ->
            ( "Profile", "Public profile" )

        EmailMenu ->
            ( "Email settings", "Email settings" )


menuToIcon : MenuSettings -> String
menuToIcon menu =
    case menu of
        ProfileMenu ->
            "icon-user"

        EmailMenu ->
            "icon-mail"



---- MSG ----


type Msg
    = PassedSlowLoadTreshold -- timer
    | Submit (Time.Posix -> Msg) -- Get Current Time
    | SubmitProfile Time.Posix
    | GotUserFull (GqlData UserFull)
    | GotUserPatch (GqlData UserFull)
    | ChangeMenuFocus MenuSettings
    | OnChangePost String String
    | SwitchLang Lang.Lang
    | SwitchNotifyByEmail Int Bool
      -- Common
    | NoMsg
    | LogErr String
    | Navigate String
    | DoOpenModal
    | DoCloseModal ModalData
      -- Help
    | HelpMsg Help.Msg
    | AuthModalMsg AuthModal.Msg



-- INIT --


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        apis =
            global.session.apis

        username =
            flags.param1 |> Url.percentDecode |> withDefault ""

        -- Query parameters
        query =
            queryParser global.url

        menu =
            Dict.get "m" query |> withDefault [] |> List.head |> withDefault "" |> menuDecoder

        model =
            { username = username
            , user = Loading
            , user_result = NotAsked
            , menuFocus = menu
            , menuList = menuList
            , hasUnsavedData = False
            , switch_index = -1
            , form = initUserProfileForm username

            -- common
            , refresh_trial = 0
            , help = Help.init global.session.user
            , authModal = AuthModal.init global.session.user Nothing
            , empty = {}
            }

        cmds =
            [ queryUserFull apis username GotUserFull
            , sendSleep PassedSlowLoadTreshold 500
            ]
    in
    ( model
    , Cmd.batch cmds
    , send (UpdateSessionFocus Nothing)
    )



---- UPDATE ----


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global message model =
    let
        apis =
            global.session.apis
    in
    case message of
        PassedSlowLoadTreshold ->
            let
                user =
                    ternary (model.user == Loading) LoadingSlowly model.user
            in
            ( { model | user = user }, Cmd.none, Cmd.none )

        Submit nextMsg ->
            ( model, Task.perform nextMsg Time.now, Cmd.none )

        SubmitProfile time ->
            ( { model | user_result = Loading }, patchUser apis model.form GotUserPatch, Cmd.none )

        GotUserFull result ->
            case result of
                Success uctx ->
                    ( { model | user = result }, Cmd.none, Cmd.none )

                _ ->
                    ( { model | user = result }, Cmd.none, Cmd.none )

        GotUserPatch result ->
            case result of
                Success user ->
                    ( { model
                        | switch_index = -1
                        , user = Success user
                        , user_result = result
                        , form = initUserProfileForm model.username
                      }
                    , Cmd.none
                    , Cmd.none
                    )

                _ ->
                    ( { model | user_result = result }, Cmd.none, Cmd.none )

        ChangeMenuFocus menu ->
            let
                url =
                    toHref (Route.Dynamic_Settings { param1 = model.username })

                query =
                    queryBuilder
                        [ ( "m", menuEncoder menu ) ]
            in
            ( model, Cmd.none, Nav.pushUrl global.key (url ++ "?" ++ query) )

        OnChangePost field value ->
            let
                f =
                    model.form
            in
            ( { model | form = { f | post = Dict.insert field value f.post } }, Cmd.none, Cmd.none )

        SwitchLang lang ->
            ( { model | user_result = Loading }
            , patchUser apis (initUserProfileForm model.username |> (\x -> { x | lang = Just lang })) GotUserPatch
            , Cmd.none
            )

        SwitchNotifyByEmail i _ ->
            let
                val =
                    withMaybeData model.user |> Maybe.map .notifyByEmail |> withDefault False
            in
            ( { model | user_result = Loading, switch_index = i }
            , patchUser apis (initUserProfileForm model.username |> (\x -> { x | notifyByEmail = Just (not val) })) GotUserPatch
            , Cmd.none
            )

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

        AuthModalMsg msg ->
            let
                ( data, out ) =
                    AuthModal.update apis msg model.authModal

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds

                -- reload silently the page if needed
                cmds_extra =
                    out.result
                        |> Maybe.map
                            (\o ->
                                if Tuple.first o == True then
                                    [ Nav.replaceUrl global.key (Url.toString global.url) ]

                                else
                                    []
                            )
                        |> withDefault []
            in
            ( { model | authModal = data }, out.cmds |> List.map (\m -> Cmd.map AuthModalMsg m) |> List.append (cmds ++ cmds_extra) |> Cmd.batch, Cmd.batch gcmds )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions _ _ =
    [ Ports.mcPD Ports.closeModalFromJs LogErr DoCloseModal
    ]
        ++ (Help.subscriptions |> List.map (\s -> Sub.map HelpMsg s))
        ++ (AuthModal.subscriptions |> List.map (\s -> Sub.map AuthModalMsg s))
        |> Sub.batch



---- VIEW ----


view : Global.Model -> Model -> Document Msg
view global model =
    { title = model.username ++ "'s settings"
    , body =
        [ view_ model
        , Help.view model.empty model.help |> Html.map HelpMsg
        , AuthModal.view model.empty model.authModal |> Html.map AuthModalMsg
        ]
    }


view_ : Model -> Html Msg
view_ model =
    div [ id "settings", class "columns is-centered is-marginles" ]
        [ div [ class "column is-12 is-11-desktop is-9-fullhd" ]
            [ div [ class "section" ]
                [ div [ class "columns" ]
                    [ div [ class "column is-one-fifth" ] [ viewSettingsMenu model ]
                    , div [ class "column" ]
                        [ case model.user of
                            Success user ->
                                viewSettingsContent user model

                            NotAsked ->
                                text ""

                            Loading ->
                                text ""

                            LoadingSlowly ->
                                div [ class "spinner" ] []

                            Failure err ->
                                viewGqlErrors err
                        ]
                    ]
                ]
            ]
        ]


viewSettingsMenu : Model -> Html Msg
viewSettingsMenu model =
    nav [ id "menuSettings", class "menu" ]
        [ ul [ class "menu-list" ] <|
            (model.menuList
                |> List.map
                    (\x ->
                        [ li []
                            [ a [ onClickPD (ChangeMenuFocus x), target "_blank", classList [ ( "is-active", x == model.menuFocus ) ] ]
                                [ A.icon1 (menuToIcon x) (menuToString x |> Tuple.first) ]
                            ]
                        ]
                    )
                |> List.concat
            )
        ]


viewSettingsContent : UserFull -> Model -> Html Msg
viewSettingsContent user model =
    case model.menuFocus of
        ProfileMenu ->
            div [ class "columns" ]
                [ div [ class "column is-6" ]
                    [ viewProfileSettings user model.user_result model.switch_index model.menuFocus model.form ]
                , div [ class "column is-offset-1" ]
                    [ viewProfileC user ]
                ]

        EmailMenu ->
            div [ class "columns" ]
                [ div [ class "column is-6" ]
                    [ viewEmailSettings user model.user_result model.switch_index model.menuFocus ]
                ]


viewProfileSettings : UserFull -> GqlData UserFull -> Int -> MenuSettings -> UserProfileForm -> Html Msg
viewProfileSettings user result switch_index menuFocus form =
    let
        isLoading =
            result == Loading

        isSendable =
            isPostSendableOr [ "name", "bio", "location" ] form.post
    in
    div []
        [ h2 [ class "subtitle is-size-3" ] [ textT (menuToString menuFocus |> Tuple.second) ]
        , div [ class "mb-4" ]
            [ div [ class "field" ]
                [ label [ class "label" ] [ text "Name" ]
                , div [ class "control" ]
                    [ input
                        [ class "input"
                        , type_ "text"
                        , placeholder "Choose a name for your public profile"
                        , value (withDefault "" (mor (Dict.get "name" form.post) user.name))
                        , onInput (OnChangePost "name")
                        ]
                        []
                    ]
                ]
            ]
        , div [ class "mb-4" ]
            [ div [ class "field" ]
                [ label [ class "label" ] [ text "Bio" ]
                , div [ class "control" ]
                    [ textarea
                        [ class "textarea"
                        , placeholder "Tell us more about yourself"
                        , value (withDefault "" (mor (Dict.get "bio" form.post) user.bio))
                        , onInput (OnChangePost "bio")
                        ]
                        []
                    ]
                ]
            ]
        , div [ class "mb-4" ]
            [ div [ class "field" ]
                [ label [ class "label" ] [ A.icon1 "icon-map-pin" "Location" ]
                , div [ class "control" ]
                    [ input
                        [ class "input"
                        , type_ "text"
                        , placeholder "City, country..."
                        , value (withDefault "" (mor (Dict.get "location" form.post) user.location))
                        , onInput (OnChangePost "location")
                        ]
                        []
                    ]
                ]
            ]
        , case result of
            Failure err ->
                viewGqlErrors err

            _ ->
                text ""
        , div [ class "is-pulled-right" ]
            [ div [ class "buttons" ]
                [ button
                    ([ class "button is-success"
                     , classList [ ( "is-loading", isLoading ) ]
                     , disabled (not isSendable)
                     ]
                        ++ ternary isSendable [ onClick (Submit SubmitProfile) ] []
                    )
                    [ textH "Update profile" ]
                ]
            ]

        --, hr [ class "has-border-light", style "margin-top" "80px" ] []
        --, div [ class "mb-4" ]
        --    [ div [ class "field is-horizontal" ]
        --        [ div [ class "field-label" ] [ label [ class "label" ] [ A.icon1 "icon-globe" "Language" ] ]
        --        , div [ class "field-body control" ]
        --            [ span [ class "select" ]
        --                [ select [] <|
        --                    List.map
        --                        (\x ->
        --                            let
        --                                locale =
        --                                    case x of
        --                                        Lang.En ->
        --                                            "english"
        --                                        Lang.Fr ->
        --                                            "français"
        --                                        Lang.It ->
        --                                            "italian"
        --                            in
        --                            option [ onClick (SwitchLang x), selected (x == user.lang) ] [ textH locale ]
        --                        )
        --                    <|
        --                        Lang.list
        --                ]
        --            ]
        --        ]
        --    ]
        ]


type alias SwitchRecord =
    { index : Int -- reference index
    , msg :
        Int
        -> Bool
        -> Msg -- Msg
    , title : String -- title text
    , help : String -- help text
    , val : UserFull -> Bool
    }


viewEmailSettings : UserFull -> GqlData UserFull -> Int -> MenuSettings -> Html Msg
viewEmailSettings user result switch_index menuFocus =
    let
        switches =
            [ SwitchRecord 0 SwitchNotifyByEmail T.notifyByEmail "" .notifyByEmail
            ]
    in
    switches
        |> List.map
            (\x ->
                let
                    ref_name =
                        "switch" ++ String.fromInt x.index
                in
                div [ class "media" ]
                    [ div [ class "field" ]
                        [ input [ onClick (x.msg x.index False), id ref_name, class "switch is-rounded is-success", type_ "checkbox", name ref_name, checked (x.val user == True) ] []
                        , label [ for ref_name ]
                            [ text T.space_
                            , text x.title

                            -- Use loadingSlowly because here it causes eyes distraction !
                            --, loadingSpin ((result ==Loading) && switch_index == x.index)
                            ]
                        , case result of
                            Failure e ->
                                if switch_index == x.index then
                                    viewGqlErrors e

                                else
                                    text ""

                            _ ->
                                text ""
                        , span [ class "help" ] [ text x.help ]
                        ]
                    ]
            )
        |> List.append
            [ div [ class "mb-4" ]
                [ div [ class "field" ]
                    [ label [ class "label" ] [ text "Email" ]
                    , div [ class "control" ]
                        [ input [ class "input", type_ "text", value user.email, disabled True ] []
                        ]
                    ]
                , div [ class "help-label" ] [ text "Your address is private, only you can see it." ]
                ]
            ]
        |> List.append [ h2 [ class "subtitle is-size-3" ] [ textT (menuToString menuFocus |> Tuple.second) ] ]
        |> div []
