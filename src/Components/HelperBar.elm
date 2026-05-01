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


module Components.HelperBar exposing (Msg(..), State, init, subscriptions, update, view)

import Assets as A
import Fractale.Codecs exposing (DocType(..), FractalBaseRoute(..), NodeFocus, getOrgaRoles, isPending, isProjectBaseUri, isTensionBaseUri, nearestCircleid, nid2rootid, nid2type, toLink)
import Fractale.Graph exposing (getParent)
import Fractale.User exposing (UserState(..))
import Fractale.View exposing (counter, viewRole, visibility2icon)
import Generated.Route as Route exposing (toHref)
import Html exposing (Html, a, div, i, li, nav, p, span, text, ul)
import Html.Attributes exposing (attribute, class, classList, href, id, title)
import Html.Events exposing (onClick)
import List.Extra as LE
import Loading exposing (RequestResult(..))
import Maybe exposing (withDefault)
import ModelSchema exposing (LocalGraph, OrgaInfo, UserCtx, UserRole, getSourceTid)
import Ports
import Schema.Enum.NodeType as NodeType
import Schema.Enum.NodeVisibility as NodeVisibility
import Schema.Enum.RoleType as RoleType
import Session exposing (Apis, GlobalCmd(..), LabelSearchPanelOnClickAction(..), SessionCommon, ViewMode(..))
import String.Format as Format
import Text as T
import Utils.Bool exposing (ternary)
import Utils.Html exposing (showIf)
import Utils.Maybe exposing (unwrap, unwrap2)



-- ------------------------------
-- M O D E L
-- ------------------------------


type State
    = State Model


type alias Model =
    { rolesState : RolesState
    , focus : NodeFocus

    -- Common
    , session : SessionCommon
    , refresh_trial : Int -- use to refresh user token
    , baseUri : FractalBaseRoute
    , uriQuery : Maybe String
    }


type RolesState
    = Expanded
    | Collapsed


initModel : FractalBaseRoute -> Maybe String -> NodeFocus -> SessionCommon -> Model
initModel baseUri uriQuery focus session =
    { rolesState = Collapsed
    , focus = focus

    -- Common
    , session = session
    , refresh_trial = 0
    , baseUri = baseUri
    , uriQuery = uriQuery
    }


init : FractalBaseRoute -> Maybe String -> NodeFocus -> SessionCommon -> State
init baseUri uriQuery focus session =
    initModel baseUri uriQuery focus session |> State


expand : Model -> Model
expand model =
    { model | rolesState = Expanded }


collapse : Model -> Model
collapse model =
    { model | rolesState = Collapsed }


numberRolesCollapsed : Int
numberRolesCollapsed =
    4



-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = OnExpand
    | OnCollapse
    | OnToggleTreeMenu
    | OnJoin
    | OnOpenPanel String String (Maybe ( Int, Int ))
    | OnToggleWatch
      -- Common
    | NoMsg
    | LogErr String
    | UpdateUctx UserCtx


type alias Out =
    { cmds : List (Cmd Msg)
    , gcmds : List GlobalCmd
    , result : Maybe Bool
    }


noOut : Out
noOut =
    Out [] [] Nothing


out0 : List (Cmd Msg) -> Out
out0 cmds =
    Out cmds [] Nothing


out1 : List GlobalCmd -> Out
out1 cmds =
    Out [] cmds Nothing


out2 : List (Cmd Msg) -> List GlobalCmd -> Out
out2 cmds gcmds =
    Out cmds gcmds Nothing


update : Apis -> Msg -> State -> ( State, Out )
update apis message (State model) =
    update_ apis message model
        |> Tuple.mapFirst State


update_ : Apis -> Msg -> Model -> ( Model, Out )
update_ apis message model =
    case message of
        OnExpand ->
            ( expand model, noOut )

        OnCollapse ->
            ( collapse model, noOut )

        OnJoin ->
            ( model, out1 [ DoJoinOrga model.focus.rootnameid ] )

        OnToggleTreeMenu ->
            ( model, out1 [ DoToggleTreeMenu ] )

        OnOpenPanel domid nameid pos ->
            ( model, out1 [ DoOpenActionPanel domid nameid pos ] )

        OnToggleWatch ->
            ( model, out1 [ DoToggleWatchOrga model.focus.rootnameid ] )

        -- Common
        NoMsg ->
            ( model, noOut )

        LogErr err ->
            ( model, out0 [ Ports.logErr err ] )

        UpdateUctx uctx ->
            let
                session =
                    model.session
            in
            ( { model | session = { session | user = LoggedIn uctx } }, noOut )


subscriptions : List (Sub Msg)
subscriptions =
    [ Ports.uctxPD Ports.loadUserCtxFromJs LogErr UpdateUctx
    ]



-- ------------------------------
-- V I E W
-- ------------------------------


type alias Op =
    { path_data : Maybe LocalGraph
    , isPanelOpen : Bool
    , orgaInfo : Maybe OrgaInfo
    }


view : Op -> State -> Html Msg
view op (State model) =
    -- @debug: padding-top overflow column.width p-0
    div [ id "helperBar", class "columns is-centered m-0" ]
        [ div [ class "column is-12 is-11-desktop is-10-fullhd p-0" ] <|
            case model.session.viewMode of
                DesktopView ->
                    [ div [ class "ml-3 mb-4 mx-mobile" ] [ viewPathContext op model ]
                    , viewNavTabs op model
                    ]

                EmbedView ->
                    [ div [ class "ml-3 mb-4 mx-mobile" ] [ viewPathContextEmbed op model ] ]
        ]


viewPathContextEmbed : Op -> Model -> Html Msg
viewPathContextEmbed op model =
    nav [ class "level" ]
        [ div [ class "level-left" ] [ viewPath model.baseUri model.uriQuery op.path_data ]
        ]


viewPathContext : Op -> Model -> Html Msg
viewPathContext op model =
    let
        ( rootnameid, userCanJoin ) =
            case op.path_data of
                Just path ->
                    ( path.root |> Maybe.map .nameid |> withDefault ""
                    , path.root |> Maybe.map (\r -> r.userCanJoin == Just True) |> withDefault False
                    )

                Nothing ->
                    ( "", False )

        ( watch_icon, watch_txt, watch_title ) =
            if unwrap2 False .isWatching op.orgaInfo then
                ( "icon-eye is-liked", T.unwatch, T.unwatchThisOrganisation )

            else
                ( "icon-eye", T.watch, T.watchThisOrganisation )
    in
    nav [ class "level" ]
        [ div [ class "level-left" ] [ viewPath model.baseUri model.uriQuery op.path_data ]
        , div [ class "level-right" ]
            [ case op.path_data of
                Just _ ->
                    div
                        [ class "tag has-border-light-small is-rounded-light mr-3 is-w is-h"
                        , attribute "style" "padding: 14px 15px;"
                        , title watch_title
                        , onClick OnToggleWatch
                        ]
                        [ A.icon1 watch_icon watch_txt
                        , case unwrap 0 .n_watchers op.orgaInfo of
                            0 ->
                                text ""

                            i ->
                                counter i
                        ]

                Nothing ->
                    text ""
            , div [ id "rolesMenu", class "is-hidden-mobile" ]
                [ case model.session.user of
                    LoggedIn uctx ->
                        case op.path_data of
                            Just path ->
                                let
                                    roles =
                                        getOrgaRoles [ rootnameid ] uctx.roles
                                in
                                if isPending uctx rootnameid then
                                    -- show Pending button (redicrect to contract)...
                                    div [ id "joinPending", class "button is-small has-text-weight-semibold is-warning joinPendingTrigger" ]
                                        [ text T.pendingInvitation ]

                                else if List.length roles == 0 && userCanJoin then
                                    joinButton op

                                else
                                    memberButtons roles op model

                            Nothing ->
                                div [ class "buttons" ] [ div [ class "ph-button-1" ] [] ]

                    LoggedOut ->
                        if userCanJoin then
                            joinButton op

                        else
                            div [ class "buttons" ] [ div [ class "ph-button-1" ] [] ]
                ]
            ]
        ]


viewNavTabs : Op -> Model -> Html Msg
viewNavTabs op model =
    let
        focusid =
            Maybe.map (\x -> x.focus.nameid) op.path_data
                |> withDefault model.focus.nameid
    in
    nav [ class "tabs is-boxed" ]
        [ ul [ class "" ]
            ([ li [ classList [ ( "is-active", model.baseUri == OverviewBaseUri ) ] ]
                [ a [ href (toLink OverviewBaseUri focusid []) ] [ A.icon1 "icon-sun" T.overview ] ]
             , li [ classList [ ( "is-active", model.baseUri == TensionsBaseUri || isTensionBaseUri model.baseUri ) ] ]
                [ a [ href (toLink TensionsBaseUri focusid []) ]
                    [ A.icon1 "icon-exchange" (T.tensions model.session.lexicon)
                    , case unwrap 0 .n_tensions op.orgaInfo of
                        0 ->
                            text ""

                        i ->
                            counter i
                    ]
                ]
             , li [ classList [ ( "is-active", model.baseUri == ProjectsBaseUri || isProjectBaseUri model.baseUri ) ] ]
                [ a [ href (toLink ProjectsBaseUri focusid []) ]
                    [ A.icon1 "icon-layout" T.projects
                    , case unwrap 0 .n_projects op.orgaInfo of
                        0 ->
                            text ""

                        i ->
                            counter i
                    ]
                ]
             ]
                ++ (Maybe.map
                        (\path ->
                            if path.focus.type_ /= NodeType.Role then
                                [ li [ classList [ ( "is-active", model.baseUri == MembersBaseUri ) ] ]
                                    [ a [ href (toLink MembersBaseUri focusid []) ]
                                        [ A.icon1 "icon-user" T.members
                                        , case unwrap 0 .n_members op.orgaInfo of
                                            0 ->
                                                text ""

                                            i ->
                                                counter i
                                        ]
                                    ]
                                ]

                            else
                                []
                        )
                        op.path_data
                        |> withDefault []
                   )
                ++ (Maybe.map
                        (\path ->
                            if model.session.user /= LoggedOut && path.focus.type_ == NodeType.Circle then
                                [ li [ class "vbar" ] []
                                , li [ classList [ ( "is-active", model.baseUri == SettingsBaseUri ) ] ]
                                    [ a [ href (toLink SettingsBaseUri focusid []) ] [ A.icon1 "icon-settings" T.settings ] ]
                                ]

                            else
                                []
                        )
                        op.path_data
                        |> withDefault []
                   )
                ++ (Maybe.map
                        (\path ->
                            [ showIf (path.focus.nameid /= unwrap "" .nameid path.root) <|
                                li []
                                    [ span
                                        [ class "help-label button-light is-goroot is-align-self-flex-start"
                                        ]
                                        [ a [ class "is-smaller", href (toLink model.baseUri (getParent path |> withDefault "") []) ] [ A.icon "arrow-up", text T.goUp ] ]
                                    ]
                            ]
                        )
                        op.path_data
                        |> withDefault []
                   )
            )
        ]


viewPath : FractalBaseRoute -> Maybe String -> Maybe LocalGraph -> Html Msg
viewPath baseUri uriQuery maybePath =
    div
        [ class "breadcrumb has-succeeds-separator wrapped-container"
        , attribute "aria-label" "breadcrumbs"
        ]
    <|
        case maybePath of
            Just g ->
                let
                    q =
                        uriQuery |> Maybe.map (\uq -> "?" ++ uq) |> Maybe.withDefault ""

                    icon =
                        --span [ onClick OnToggleTreeMenu ] [ A.icon0 ("button-light has-text-weight-bold icon-bg " ++ action2icon { doc_type = NODE g.focus.type_ }) ]
                        --span [ class "button-light", onClick OnToggleTreeMenu ] [ A.icon0 "icon-layers icon-lg" ]
                        A.icon0 "icon-layers icon-lg"
                in
                [ g.path
                    |> List.indexedMap
                        (\i p ->
                            if i < (List.length g.path - 1) then
                                li [ class "wrapped-container" ]
                                    [ ternary (i == 0) icon (text "")
                                    , if List.member baseUri [ MandateBaseUri, ContractsBaseUri ] then
                                        -- Fix issue with path not updated when moving from mandate (due to the no anonuymous path change policie)
                                        a [ class "is-block is-wrapped", href (toLink baseUri p.nameid [ getSourceTid p ] ++ "#" ++ q) ]
                                            [ text p.name ]

                                      else
                                        a [ class "is-block is-wrapped", href (toLink baseUri p.nameid [ getSourceTid p ] ++ q) ]
                                            [ text p.name ]
                                    ]

                            else
                                li [ class "wrapped-container" ]
                                    [ ternary (i == 0) icon (text "")
                                    , a [ class "is-block is-wrapped has-text-weight-bold has-text-strong", href (toLink baseUri p.nameid [ getSourceTid p ] ++ q) ] [ text p.name ]
                                    , span
                                        [ class ""
                                        , title (T.thisThingIs |> Format.value (NodeType.toString (nid2type p.nameid)) |> Format.value (NodeVisibility.toString g.focus.visibility))
                                        , href (toHref (Route.Tension_Dynamic_Dynamic_Action { param1 = nid2rootid p.nameid, param2 = getSourceTid p }))
                                        ]
                                        [ A.icon (visibility2icon g.focus.visibility) ]
                                    ]
                        )
                    |> ul []
                ]

            Nothing ->
                [ div [ class "ph-line is-1" ] [] ]


joinButton : Op -> Html Msg
joinButton op =
    div [ id "join", class "button is-small has-text-weight-semibold is-primary", onClick OnJoin ]
        [ text T.joinOrga ]


memberButtons : List UserRole -> Op -> Model -> Html Msg
memberButtons roles_ op model =
    let
        roles =
            case model.rolesState of
                Expanded ->
                    roles_

                Collapsed ->
                    List.take numberRolesCollapsed roles_

        lastButton =
            case model.rolesState of
                Expanded ->
                    div [ class "button is-small", onClick OnCollapse ] [ A.icon "icon-chevrons-left" ]

                Collapsed ->
                    let
                        roleMoreLen =
                            List.length roles_ - List.length roles
                    in
                    if roleMoreLen > 0 then
                        div [ class "button has-font-weight-semibold is-small", onClick OnExpand ]
                            [ text ("+" ++ String.fromInt roleMoreLen)
                            , A.icon "icon-chevrons-right icon-padding-left"
                            ]

                    else
                        div [] []
    in
    roles
        |> List.concatMap
            (\r ->
                if r.role_type == RoleType.Member then
                    []

                else
                    [ viewRole "" True True Nothing Nothing (ternary op.isPanelOpen (\_ _ _ -> NoMsg) OnOpenPanel) r ]
            )
        |> List.reverse
        |> List.append [ lastButton ]
        |> List.reverse
        |> div [ class "buttons" ]
