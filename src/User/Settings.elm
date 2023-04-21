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


module User.Settings exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Assets as A
import Assets.Logo as Logo
import Auth exposing (ErrState(..))
import Browser.Navigation as Nav
import Bulk exposing (..)
import Bulk.Codecs exposing (FractalBaseRoute(..))
import Bulk.Error exposing (viewGqlErrors, viewHttpErrors)
import Bulk.View exposing (lang2str, viewProfileC)
import Components.AuthModal as AuthModal
import Dict
import Extra exposing (mor, space_, ternary, textH)
import Extra.Events exposing (onClickPD)
import Extra.Url exposing (queryBuilder, queryParser)
import Form exposing (getd, isPostSendable, isPostSendableOr)
import Form.Help as Help
import Fractal.Enum.Lang as Lang
import Generated.Route as Route exposing (toHref)
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, button, div, h2, hr, i, input, label, li, nav, option, select, span, text, textarea, ul)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, for, href, id, name, placeholder, required, selected, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Loading exposing (GqlData, ModalData, RequestResult(..), RestData, withMaybeData)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.PatchUser exposing (patchUser)
import Query.QueryUser exposing (queryUserFull)
import RemoteData
import Requests exposing (updatePassword)
import Session exposing (GlobalCmd(..))
import Task
import Text as T
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



---- MODEL----


type alias Model =
    { username : String
    , user : GqlData UserFull
    , user_result : GqlData UserFull
    , password_result : RestData UserCtx
    , menuFocus : MenuSettings
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
    | AccountMenu
    | EmailMenu


menuList : List MenuSettings
menuList =
    [ ProfileMenu, AccountMenu, EmailMenu ]


menuEncoder : MenuSettings -> String
menuEncoder menu =
    case menu of
        ProfileMenu ->
            "profile"

        AccountMenu ->
            "account"

        EmailMenu ->
            "email"


menuDecoder : String -> MenuSettings
menuDecoder menu =
    case menu of
        "profile" ->
            ProfileMenu

        "account" ->
            AccountMenu

        "email" ->
            EmailMenu

        _ ->
            ProfileMenu


menuToString : MenuSettings -> ( String, String )
menuToString menu =
    case menu of
        ProfileMenu ->
            ( T.profile, T.publicProfil )

        AccountMenu ->
            ( T.account, "" )

        EmailMenu ->
            --( "Email settings", "Email settings" )
            ( T.emailConf, "" )


menuToIcon : MenuSettings -> String
menuToIcon menu =
    case menu of
        ProfileMenu ->
            "icon-user"

        AccountMenu ->
            "icon-settings"

        EmailMenu ->
            "icon-mail"



-- INIT --


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        apis =
            global.session.apis

        conf =
            { screen = global.session.screen, now = global.now, lang = global.session.lang, url = global.url }

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
            , password_result = RemoteData.NotAsked
            , menuFocus = menu
            , hasUnsavedData = False
            , switch_index = -1
            , form = initUserProfileForm username

            -- common
            , refresh_trial = 0
            , help = Help.init global.session.user conf
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
    | OnPasswordUpdate
    | OnPasswordUpdateAck (RestData UserCtx)
      -- Common
    | NoMsg
    | LogErr String
    | DoOpenModal
    | DoCloseModal ModalData
      -- Help
    | HelpMsg Help.Msg
    | AuthModalMsg AuthModal.Msg



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
            let
                form =
                    model.form
            in
            case result of
                Success user ->
                    ( { model
                        | switch_index = -1
                        , user = Success user
                        , user_result = result
                        , form = initUserProfileForm model.username
                      }
                    , case form.lang of
                        Just lang ->
                            Ports.reloadLang (Lang.toString lang)

                        Nothing ->
                            Cmd.none
                    , Cmd.none
                    )

                _ ->
                    ( { model | user_result = result, form = { form | lang = Nothing } }, Cmd.none, Cmd.none )

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
            let
                form =
                    model.form
            in
            ( { model | user_result = Loading, form = { form | lang = Just lang } }
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

        OnPasswordUpdate ->
            let
                post =
                    model.form.post |> Dict.insert "username" model.username
            in
            ( model, updatePassword apis post OnPasswordUpdateAck, Cmd.none )

        OnPasswordUpdateAck result ->
            ( { model | password_result = result }
            , Cmd.none
            , case result of
                RemoteData.Success uctx ->
                    send (UpdateUserSession uctx)

                _ ->
                    Cmd.none
            )

        -- Common
        NoMsg ->
            ( model, Cmd.none, Cmd.none )

        LogErr err ->
            ( model, Ports.logErr err, Cmd.none )

        DoOpenModal ->
            ( model, Ports.open_modal "actionModal", Cmd.none )

        DoCloseModal data ->
            let
                gcmd =
                    if data.link /= "" then
                        send (NavigateRaw data.link)

                    else
                        Cmd.none
            in
            ( model, Cmd.none, Cmd.batch [ gcmd, Ports.close_modal ] )

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
                                if Tuple.first o then
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
    div [ id "settings", class "columns is-centered top-section" ]
        [ div [ class "column is-12 is-11-desktop is-9-fullhd" ]
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


viewSettingsMenu : Model -> Html Msg
viewSettingsMenu model =
    nav [ id "menuSettings", class "menu" ]
        [ ul [ class "menu-list" ] <|
            (menuList
                |> List.concatMap
                    (\x ->
                        [ li []
                            [ a [ onClickPD (ChangeMenuFocus x), target "_blank", classList [ ( "is-active", x == model.menuFocus ) ] ]
                                [ A.icon1 (menuToIcon x) (menuToString x |> Tuple.first) ]
                            ]
                        ]
                    )
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

        AccountMenu ->
            div [ class "columns" ]
                [ div [ class "column is-6" ]
                    [ viewAccountSettings user model.user_result model.password_result model.switch_index model.menuFocus model.form ]
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
            Loading.isLoading result

        isSendable =
            isPostSendableOr [ "name", "bio", "location" ] form.post
    in
    div []
        [ h2 [ class "subtitle is-size-3" ] [ text (menuToString menuFocus |> Tuple.second) ]
        , div [ class "mb-4" ]
            [ div [ class "field" ]
                [ label [ class "label" ] [ text T.name ]
                , div [ class "control" ]
                    [ input
                        [ class "input"
                        , type_ "text"
                        , placeholder T.namePH
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
                        , placeholder T.bioPH
                        , value (withDefault "" (mor (Dict.get "bio" form.post) user.bio))
                        , onInput (OnChangePost "bio")
                        ]
                        []
                    ]
                ]
            ]
        , div [ class "mb-4" ]
            [ div [ class "field" ]
                [ label [ class "label" ] [ A.icon1 "icon-map-pin" T.location ]
                , div [ class "control" ]
                    [ input
                        [ class "input"
                        , type_ "text"
                        , placeholder T.locationPH
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
                    [ text T.updateProfile ]
                ]
            ]
        , hr [ class "has-border-light", style "margin-top" "80px" ] []
        , div [ class "mb-4" ]
            [ div [ class "field is-horizontal" ]
                --[ div [ class "field-label" ] [ label [ class "label" ] [ A.icon1 "icon-globe" T.language ] ]
                [ div [ class "field-label" ] [ label [ class "label" ] [ span [ class "is-inline-flex" ] [ Logo.i18n, text (space_ ++ T.language) ] ] ]
                , div [ class "field-body control" ]
                    [ span [ class "select" ]
                        [ select [] <|
                            List.map
                                (\x ->
                                    option [ onClick (SwitchLang x), selected (x == user.lang) ] [ text (lang2str x) ]
                                )
                                Lang.list
                        ]
                    ]
                ]
            , div [ class "help-label" ] [ text T.languageHelp ]
            ]
        ]


type alias SwitchRecord =
    { index : Int
    , msg :
        Int
        -> Bool
        -> Msg
    , title : String
    , help : String
    , val : UserFull -> Bool
    }


viewEmailSettings : UserFull -> GqlData UserFull -> Int -> MenuSettings -> Html Msg
viewEmailSettings user result switch_index menuFocus =
    let
        switches =
            [ SwitchRecord 0 SwitchNotifyByEmail T.notifyByEmail "" .notifyByEmail
            ]
    in
    List.map
        -- Switches
        (\x ->
            let
                ref_name =
                    "switch" ++ String.fromInt x.index
            in
            div [ class "media" ]
                [ div [ class "field" ]
                    [ input [ onClick (x.msg x.index False), id ref_name, class "switch is-rounded is-success", type_ "checkbox", name ref_name, checked (x.val user) ] []
                    , label [ for ref_name ]
                        [ text space_
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
        switches
        |> List.append
            [ div [ class "mb-4" ]
                [ div [ class "field" ]
                    [ label [ class "label" ] [ text T.email ]
                    , div [ class "control" ]
                        [ input [ class "input", type_ "text", value user.email, disabled True ] []
                        ]
                    ]
                , div [ class "help-label" ] [ text T.emailHelp ]
                ]
            ]
        |> List.append [ h2 [ class "subtitle is-size-3" ] [ text (menuToString menuFocus |> Tuple.second) ] ]
        |> div []


viewAccountSettings : UserFull -> GqlData UserFull -> RestData UserCtx -> Int -> MenuSettings -> UserProfileForm -> Html Msg
viewAccountSettings user user_result password_result switch_index menuFocus form =
    let
        switches =
            []

        isSendable =
            isPostSendable [ "password", "newPassword", "confirmPassword" ] form.post
    in
    div [] <|
        [ h2 [ class "subtitle is-size-3" ] [ text (menuToString menuFocus |> Tuple.second) ]
        , div [ class "mb-4" ]
            [ div [ class "field" ]
                [ label [ class "label" ]
                    [ text T.username ]
                , div [ class "control" ]
                    [ input [ class "input", type_ "text", value user.username, disabled True ] [] ]
                ]
            , div [ class "help-label" ] [ text T.usernameHelp ]
            ]
        ]
            ++ List.map
                -- Switches
                (\x ->
                    let
                        ref_name =
                            "switch" ++ String.fromInt x.index
                    in
                    div [ class "media" ]
                        [ div [ class "field" ]
                            [ input [ onClick (x.msg x.index False), id ref_name, class "switch is-rounded is-success", type_ "checkbox", name ref_name, checked (x.val user) ] []
                            , label [ for ref_name ]
                                [ text space_
                                , text x.title

                                -- Use loadingSlowly because here it causes eyes distraction !
                                --, loadingSpin ((result == Loading) && switch_index == x.index)
                                ]
                            , case user_result of
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
                switches
            ++ [ -- Reset password Form
                 div [ class "box is-warning-light my-6" ]
                    [ h2 [ class "subtitle" ] [ text "Change Password" ]
                    , div [ class "field" ]
                        [ label [ class "label" ] [ text "Current password" ]
                        , div [ class "control" ]
                            [ input
                                [ class "input followFocus"
                                , attribute "data-nextfocus" "newPassword"
                                , type_ "password"
                                , placeholder "Enter your current password"
                                , value (getd "password" form.post)
                                , onInput (OnChangePost "password")
                                , required True
                                ]
                                []
                            ]
                        ]
                    , div [ class "field" ]
                        [ label [ class "label" ] [ text "New password" ]
                        , div [ class "control" ]
                            [ input
                                [ id "newPassword"
                                , class "input followFocus"
                                , attribute "data-nextfocus" "confirmPassword"
                                , type_ "password"
                                , placeholder "Enter your new password"
                                , value (getd "newPassword" form.post)
                                , onInput (OnChangePost "newPassword")
                                , required True
                                ]
                                []
                            ]
                        ]
                    , div [ class "field" ]
                        [ label [ class "label" ] [ text "Confirm password" ]
                        , div [ class "control" ]
                            [ input
                                [ id "confirmPassword"
                                , class "input"
                                , type_ "password"
                                , placeholder "Confirm your new password"
                                , value (getd "confirmPassword" form.post)
                                , onInput (OnChangePost "confirmPassword")
                                , required True
                                ]
                                []
                            ]
                        ]
                    , div [ class "field" ]
                        [ div [ class "control is-text-aligned" ]
                            [ button
                                [ class "button is-primary"
                                , classList [ ( "is-loading", Loading.isLoadingRest password_result ) ]
                                , type_ "submit"
                                , disabled (not isSendable)
                                , ternary isSendable
                                    (onClick OnPasswordUpdate)
                                    (onClick NoMsg)
                                ]
                                [ text "Update Password" ]
                            , a [ class "underlined-link mx-4 has-text-info", href (toHref Route.PasswordReset ++ "?email=" ++ user.email) ] [ textH T.passwordForgotten ]
                            ]
                        ]
                    , case password_result of
                        RemoteData.Success _ ->
                            div [ class "notification is-success" ] [ text "Your password has been reset successfully." ]

                        RemoteData.Failure e ->
                            viewHttpErrors e

                        _ ->
                            text ""
                    ]
               ]
