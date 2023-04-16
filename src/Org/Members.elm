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


module Org.Members exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Array
import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Browser.Navigation as Nav
import Bulk exposing (..)
import Bulk.Codecs exposing (ActionType(..), DocType(..), Flags_, FractalBaseRoute(..), NodeFocus, basePathChanged, contractIdCodec, focusFromNameid, focusState, hasLazyAdminRole, nameidFromFlags, uriFromNameid, uriFromUsername)
import Bulk.Error exposing (viewAuthNeeded, viewGqlErrors)
import Bulk.View exposing (viewRole2, viewUser, viewUsernameLink)
import Codecs exposing (QuickDoc)
import Components.ActionPanel as ActionPanel
import Components.AuthModal as AuthModal
import Components.HelperBar as HelperBar
import Components.JoinOrga as JoinOrga
import Components.OrgaMenu as OrgaMenu
import Components.TreeMenu as TreeMenu
import Dict exposing (Dict)
import Extra exposing (ternary, textH, upH)
import Extra.Date exposing (formatDate)
import Extra.Events exposing (onClickPD, onEnter, onKeydown, onTab)
import Form exposing (isPostSendable)
import Form.Help as Help
import Form.NewTension as NTF exposing (NewTensionInput(..), TensionTab(..))
import Fractal.Enum.Lang as Lang
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Generated.Route as Route exposing (Route, toHref)
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, br, button, datalist, div, h1, h2, hr, i, input, li, nav, option, p, span, table, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, placeholder, rows, style, target, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter, onMouseLeave)
import Html.Lazy as Lazy
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Loading exposing (GqlData, ModalData, RequestResult(..), WebData, fromMaybeData, withDefaultData, withMaybeData)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.PatchTension exposing (actionRequest)
import Query.QueryContract exposing (getContractId)
import Query.QueryNode exposing (queryLocalGraph, queryMembersLocal)
import RemoteData exposing (RemoteData)
import Requests exposing (fetchMembersSub)
import Session exposing (Conf, GlobalCmd(..))
import Task
import Text as T
import Time
import Url as Url


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
                    -- Global
                    DoFocus nameid ->
                        ( [], send (NavigateNode nameid) )

                    DoNavigate link ->
                        ( [], send (NavigateRaw link) )

                    DoReplaceUrl url ->
                        ( [], send (ReplaceUrl url) )

                    DoUpdateToken ->
                        ( [], send UpdateUserToken )

                    DoUpdateUserSession uctx ->
                        ( [], send (UpdateUserSession uctx) )

                    DoUpdatePath path ->
                        ( [], send (UpdateSessionPath path) )

                    DoUpdateTree tree ->
                        ( [], send (UpdateSessionTree tree) )

                    DoUpdateOrgs orgs ->
                        ( [], send (UpdateSessionOrgs orgs) )

                    DoToggleWatchOrga a ->
                        ( [], send (ToggleWatchOrga a) )

                    -- Component
                    DoCreateTension ntm a ->
                        case ntm of
                            Nothing ->
                                ( [ Cmd.map NewTensionMsg <| send (NTF.OnOpen (FromNameid a)) ], Cmd.none )

                            Just NodeType.Circle ->
                                ( [ Cmd.map NewTensionMsg <| send (NTF.OnOpenCircle (FromNameid a)) ], Cmd.none )

                            Just NodeType.Role ->
                                ( [ Cmd.map NewTensionMsg <| send (NTF.OnOpenRole (FromNameid a)) ], Cmd.none )

                    DoJoinOrga a ->
                        ( [ Cmd.map JoinOrgaMsg <| send (JoinOrga.OnOpen a JoinOrga.JoinOne) ], Cmd.none )

                    DoOpenActionPanel a b c ->
                        ( [ send <| OpenActionPanel a b c ], Cmd.none )

                    DoToggleTreeMenu ->
                        ( [ Cmd.map TreeMenuMsg <| send TreeMenu.OnToggle ], Cmd.none )

                    DoFetchNode nameid ->
                        ( [ Cmd.map TreeMenuMsg <| send (TreeMenu.FetchNewNode nameid False) ], Cmd.none )

                    DoAddNodes nodes ->
                        ( [ Cmd.map TreeMenuMsg <| send (TreeMenu.AddNodes nodes) ], Cmd.none )

                    --DoUpdateNode nameid fun ->
                    --    ( Cmd.map TreeMenuMsg <| send (TreeMenu.UpdateNode nameid fun), Cmd.none )
                    DoDelNodes nameids ->
                        ( [ Cmd.map TreeMenuMsg <| send (TreeMenu.DelNodes nameids) ], Cmd.none )

                    DoMoveNode a b c ->
                        ( [ Cmd.map TreeMenuMsg <| send (TreeMenu.MoveNode a b c) ], Cmd.none )

                    -- App
                    DoUpdateNode nameid fun ->
                        ( [ Cmd.map TreeMenuMsg <| send (TreeMenu.UpdateNode nameid fun), send OnReload ], Cmd.none )

                    _ ->
                        ( [], Cmd.none )
            )
        |> List.unzip
        |> Tuple.mapFirst List.concat



--
-- Model
--


type alias Model =
    { -- Focus
      node_focus : NodeFocus
    , path_data : GqlData LocalGraph

    -- Page
    , members_top : GqlData (List Member)
    , members_sub : GqlData (List Member)
    , pending_hover : Bool
    , pending_hover_i : Maybe Int

    -- Common
    , conf : Conf
    , refresh_trial : Int
    , empty : {}

    -- Components
    , helperBar : HelperBar.State
    , actionPanel : ActionPanel.State
    , help : Help.State
    , joinOrga : JoinOrga.State
    , tensionForm : NTF.State
    , authModal : AuthModal.State
    , orgaMenu : OrgaMenu.State
    , treeMenu : TreeMenu.State
    }



--
-- Msg
--


type Msg
    = PassedSlowLoadTreshold -- timer
    | Submit (Time.Posix -> Msg) -- Get Current Time
    | OnReload
      -- Data Queries
    | GotPath Bool (GqlData LocalGraph) -- GraphQL
      -- Page
    | GotMembers (GqlData (List Member)) -- GraphQL
    | GotMembersSub (GqlData (List Member)) -- Rest
    | OnPendingHover Bool
    | OnPendingRowHover (Maybe Int)
    | OnGoToContract String
    | OnGoContractAck (GqlData IdPayload)
      -- Common
    | NoMsg
    | LogErr String
    | OnGoRoot
    | OpenActionPanel String String (Maybe ( Int, Int ))
      -- Components
    | HelperBarMsg HelperBar.Msg
    | HelpMsg Help.Msg
    | NewTensionMsg NTF.Msg
    | JoinOrgaMsg JoinOrga.Msg
    | AuthModalMsg AuthModal.Msg
    | OrgaMenuMsg OrgaMenu.Msg
    | TreeMenuMsg TreeMenu.Msg
    | ActionPanelMsg ActionPanel.Msg



--
-- INIT
--


type alias Flags =
    Flags_


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        apis =
            global.session.apis

        conf =
            { screen = global.session.screen, now = global.now, lang = global.session.lang, url = global.url }

        -- Focus
        newFocus =
            flags
                |> nameidFromFlags
                |> focusFromNameid

        -- What has changed
        fs =
            focusState MembersBaseUri global.session.referer global.url global.session.node_focus newFocus

        model =
            { node_focus = newFocus
            , path_data =
                global.session.path_data
                    |> Maybe.map (\x -> Success x)
                    |> withDefault Loading
            , members_top = Loading
            , members_sub = Loading
            , pending_hover = False
            , pending_hover_i = Nothing

            -- Common
            , conf = conf
            , tensionForm = NTF.init global.session.user conf
            , refresh_trial = 0
            , empty = {}
            , helperBar = HelperBar.init MembersBaseUri global.url.query newFocus global.session.user
            , help = Help.init global.session.user conf
            , joinOrga = JoinOrga.init newFocus.nameid global.session.user global.session.screen
            , authModal = AuthModal.init global.session.user Nothing
            , orgaMenu = OrgaMenu.init newFocus global.session.orga_menu global.session.orgs_data global.session.user
            , treeMenu = TreeMenu.init MembersBaseUri global.url.query newFocus global.session.tree_menu global.session.tree_data global.session.user
            , actionPanel = ActionPanel.init global.session.user global.session.screen
            }

        cmds =
            [ ternary fs.focusChange (queryLocalGraph apis newFocus.nameid True (GotPath True)) Cmd.none
            , send OnReload
            , sendSleep PassedSlowLoadTreshold 500
            , Cmd.map OrgaMenuMsg (send OrgaMenu.OnLoad)
            , Cmd.map TreeMenuMsg (send TreeMenu.OnLoad)
            ]
    in
    ( model
    , Cmd.batch cmds
    , if fs.refresh then
        send (UpdateSessionFocus (Just newFocus))

      else
        Cmd.none
    )


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global message model =
    let
        apis =
            global.session.apis
    in
    case message of
        PassedSlowLoadTreshold ->
            let
                members_top =
                    ternary (model.members_top == Loading) LoadingSlowly model.members_top

                members_sub =
                    ternary (model.members_sub == Loading) LoadingSlowly model.members_sub
            in
            ( { model | members_top = members_top, members_sub = members_sub }, Cmd.none, Cmd.none )

        Submit nextMsg ->
            ( model, Task.perform nextMsg Time.now, Cmd.none )

        OnReload ->
            ( model
            , Cmd.batch
                [ queryMembersLocal apis model.node_focus.nameid GotMembers
                , fetchMembersSub apis model.node_focus.nameid GotMembersSub
                ]
            , Cmd.none
            )

        -- Data queries
        GotPath isInit result ->
            case result of
                Success path ->
                    let
                        prevPath =
                            if isInit then
                                { path | path = [] }

                            else
                                withDefaultData path model.path_data
                    in
                    case path.root of
                        Just root ->
                            let
                                newPath =
                                    { prevPath | root = Just root, path = path.path ++ (List.tail prevPath.path |> withDefault []) }
                            in
                            ( { model | path_data = Success newPath }, Cmd.none, send (UpdateSessionPath (Just newPath)) )

                        Nothing ->
                            let
                                newPath =
                                    { prevPath | path = path.path ++ (List.tail prevPath.path |> withDefault []) }

                                nameid =
                                    List.head path.path |> Maybe.map (\p -> p.nameid) |> withDefault ""
                            in
                            ( { model | path_data = Success newPath }, queryLocalGraph apis nameid False (GotPath False), Cmd.none )

                _ ->
                    ( { model | path_data = result }, Cmd.none, Cmd.none )

        GotMembers result ->
            let
                newModel =
                    { model | members_top = result }
            in
            ( newModel, Cmd.none, Cmd.none )

        GotMembersSub result ->
            case result of
                Success mbs ->
                    ( { model
                        | members_sub =
                            List.filterMap
                                (\m ->
                                    case memberRolesFilter m.roles of
                                        [] ->
                                            Nothing

                                        roles ->
                                            Just { m | roles = roles }
                                )
                                mbs
                                |> Success
                      }
                    , Cmd.none
                    , Cmd.none
                    )

                _ ->
                    ( { model | members_sub = result }, Cmd.none, Cmd.none )

        OnPendingHover b ->
            ( { model | pending_hover = b }, Cmd.none, Cmd.none )

        OnPendingRowHover i ->
            ( { model | pending_hover_i = i }, Cmd.none, Cmd.none )

        OnGoToContract username ->
            let
                tid =
                    tidFromPath model.path_data |> withDefault ""

                contractid =
                    contractIdCodec tid (TensionEvent.toString TensionEvent.UserJoined) "" username
            in
            ( model, getContractId apis contractid OnGoContractAck, Cmd.none )

        OnGoContractAck result ->
            case result of
                Success c ->
                    let
                        tid =
                            tidFromPath model.path_data |> withDefault ""

                        link =
                            toHref <| Route.Tension_Dynamic_Dynamic_Contract_Dynamic { param1 = model.node_focus.rootnameid, param2 = tid, param3 = c.id }
                    in
                    ( model, Cmd.none, send (NavigateRaw link) )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        -- Common
        NoMsg ->
            ( model, Cmd.none, Cmd.none )

        LogErr err ->
            ( model, Ports.logErr err, Cmd.none )

        OnGoRoot ->
            let
                query =
                    global.url.query |> Maybe.map (\uq -> "?" ++ uq) |> Maybe.withDefault ""
            in
            ( model, Cmd.none, send (NavigateRaw (uriFromNameid MembersBaseUri model.node_focus.rootnameid [] ++ query)) )

        OpenActionPanel domid nameid pos ->
            ( model, Cmd.map ActionPanelMsg (send <| ActionPanel.OnOpen domid nameid (TreeMenu.getOrgaData_ model.treeMenu) pos), Cmd.none )

        -- Components
        HelperBarMsg msg ->
            let
                ( data, out ) =
                    HelperBar.update apis msg model.helperBar

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | helperBar = data }, out.cmds |> List.map (\m -> Cmd.map HelperBarMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

        NewTensionMsg msg ->
            let
                ( tf, out ) =
                    NTF.update apis msg model.tensionForm

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | tensionForm = tf }, out.cmds |> List.map (\m -> Cmd.map NewTensionMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

        HelpMsg msg ->
            let
                ( help, out ) =
                    Help.update apis msg model.help

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | help = help }, out.cmds |> List.map (\m -> Cmd.map HelpMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

        JoinOrgaMsg msg ->
            let
                ( data, out ) =
                    JoinOrga.update apis msg model.joinOrga

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | joinOrga = data }, out.cmds |> List.map (\m -> Cmd.map JoinOrgaMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

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

        OrgaMenuMsg msg ->
            let
                ( data, out ) =
                    OrgaMenu.update apis msg model.orgaMenu

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | orgaMenu = data }, out.cmds |> List.map (\m -> Cmd.map OrgaMenuMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

        TreeMenuMsg msg ->
            let
                ( data, out ) =
                    TreeMenu.update apis msg model.treeMenu

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | treeMenu = data }, out.cmds |> List.map (\m -> Cmd.map TreeMenuMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

        ActionPanelMsg msg ->
            let
                ( data, out ) =
                    ActionPanel.update apis msg model.actionPanel

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | actionPanel = data }, out.cmds |> List.map (\m -> Cmd.map ActionPanelMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions _ model =
    []
        ++ (HelperBar.subscriptions |> List.map (\s -> Sub.map HelperBarMsg s))
        ++ (Help.subscriptions |> List.map (\s -> Sub.map HelpMsg s))
        ++ (NTF.subscriptions model.tensionForm |> List.map (\s -> Sub.map NewTensionMsg s))
        ++ (JoinOrga.subscriptions model.joinOrga |> List.map (\s -> Sub.map JoinOrgaMsg s))
        ++ (AuthModal.subscriptions |> List.map (\s -> Sub.map AuthModalMsg s))
        ++ (OrgaMenu.subscriptions |> List.map (\s -> Sub.map OrgaMenuMsg s))
        ++ (TreeMenu.subscriptions |> List.map (\s -> Sub.map TreeMenuMsg s))
        ++ (ActionPanel.subscriptions model.actionPanel |> List.map (\s -> Sub.map ActionPanelMsg s))
        |> Sub.batch



---- VIEW ----


view : Global.Model -> Model -> Document Msg
view global model =
    let
        helperData =
            { path_data = withMaybeData model.path_data
            , isPanelOpen = ActionPanel.isOpen_ "actionPanelHelper" model.actionPanel
            , orgaInfo = global.session.orgaInfo
            }

        panelData =
            { tc = { action = TensionAction.EditRole, action_type = EDIT, doc_type = NODE NodeType.Role }
            , isRight = True
            , domid = "actionPanelHelper"
            , tree_data = TreeMenu.getOrgaData_ model.treeMenu
            }
    in
    { title =
        (String.join "/" <| LE.unique [ model.node_focus.rootnameid, model.node_focus.nameid |> String.split "#" |> LE.last |> withDefault "" ])
            ++ " · "
            ++ T.members
    , body =
        [ div [ class "orgPane" ]
            [ HelperBar.view helperData model.helperBar |> Html.map HelperBarMsg
            , div [ id "mainPane" ] [ view_ global model ]
            ]
        , Help.view model.empty model.help |> Html.map HelpMsg
        , NTF.view { tree_data = TreeMenu.getOrgaData_ model.treeMenu, path_data = model.path_data } model.tensionForm |> Html.map NewTensionMsg
        , JoinOrga.view model.empty model.joinOrga |> Html.map JoinOrgaMsg
        , AuthModal.view model.empty model.authModal |> Html.map AuthModalMsg
        , OrgaMenu.view model.empty model.orgaMenu |> Html.map OrgaMenuMsg
        , TreeMenu.view model.empty model.treeMenu |> Html.map TreeMenuMsg
        , ActionPanel.view panelData model.actionPanel |> Html.map ActionPanelMsg
        ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    let
        rtid =
            tidFromPath model.path_data |> withDefault ""

        isAdmin =
            case global.session.user of
                LoggedIn uctx ->
                    hasLazyAdminRole uctx model.node_focus.rootnameid

                LoggedOut ->
                    False

        isPanelOpen =
            ActionPanel.isOpen_ "actionPanelHelper" model.actionPanel
    in
    div [ class "columns is-centered" ]
        [ div [ class "column is-12 is-11-desktop is-9-fullhd mt-5" ]
            [ div [ class "section_ mt-2" ]
                [ if isAdmin then
                    div
                        [ class "button is-primary is-pulled-right"
                        , onClick (JoinOrgaMsg (JoinOrga.OnOpen model.node_focus.rootnameid JoinOrga.InviteOne))
                        ]
                        [ A.icon1 "icon-user-plus" T.inviteMembers ]

                  else
                    text ""
                , div [ class "columns mb-6 px-3" ]
                    [ Lazy.lazy4 viewMembers model.conf model.members_sub model.node_focus isPanelOpen ]
                , div [ class "columns mb-6 px-3" ]
                    [ div [ class "column is-5 is-4-fullhd pl-0" ] [ Lazy.lazy4 viewGuest model.conf model.members_top model.node_focus isPanelOpen ]
                    , div [ class "column is-3" ] [ viewPending model.conf model.members_top model.node_focus model.pending_hover model.pending_hover_i rtid ]
                    ]
                ]
            ]
        ]


viewMembers : Conf -> GqlData (List Member) -> NodeFocus -> Bool -> Html Msg
viewMembers conf data focus isPanelOpen =
    let
        goToParent =
            if focus.nameid /= focus.rootnameid then
                span [ class "help-label button-light is-h is-discrete", onClick OnGoRoot ] [ A.icon "arrow-up", text T.goRoot ]

            else
                text ""
    in
    case data of
        Success members ->
            if List.length members == 0 then
                div [] [ text T.noMemberYet, goToParent ]

            else
                div []
                    [ h2 [ class "subtitle is-size-3" ] [ text T.members, goToParent ]
                    , div [ class "table-container" ]
                        [ div [ class "table is-fullwidth" ]
                            [ thead []
                                [ tr []
                                    [ th [] []
                                    , th [] [ text T.username ]
                                    , th [] [ text T.name ]
                                    , th [] [ text T.rolesHere ]
                                    , th [] [ text T.rolesSub ]
                                    ]
                                ]
                            , tbody [] <|
                                List.map
                                    (\m ->
                                        Lazy.lazy4 viewMemberRow conf focus m isPanelOpen
                                    )
                                    members
                            ]
                        ]
                    ]

        Failure err ->
            viewGqlErrors err

        LoadingSlowly ->
            div [ class "spinner" ] []

        _ ->
            text ""


viewGuest : Conf -> GqlData (List Member) -> NodeFocus -> Bool -> Html Msg
viewGuest conf members_d focus isPanelOpen =
    let
        guests =
            members_d
                |> withDefaultData []
                |> List.filter
                    (\u ->
                        u.roles
                            |> List.map (\r -> r.role_type)
                            |> List.member RoleType.Guest
                    )
    in
    if List.length guests > 0 then
        div []
            [ h2 [ class "subtitle has-text-weight-semibold" ] [ text T.guest ]
            , div [ class "table-container" ]
                [ div [ class "table is-fullwidth" ]
                    [ thead []
                        [ tr []
                            [ th [] []
                            , th [] [ text T.username ]
                            , th [] [ text T.name ]
                            , th [] [ text T.roles ]
                            ]
                        ]
                    , tbody [] <|
                        List.indexedMap
                            (\i m ->
                                Lazy.lazy3 viewGuestRow conf m isPanelOpen
                            )
                            guests
                    ]
                ]
            ]

    else
        div [] []


viewPending : Conf -> GqlData (List Member) -> NodeFocus -> Bool -> Maybe Int -> String -> Html Msg
viewPending _ members_d focus pending_hover pending_hover_i tid =
    let
        guests =
            members_d
                |> withDefaultData []
                |> List.filter
                    (\u ->
                        u.roles
                            |> List.map (\r -> r.role_type)
                            |> List.member RoleType.Pending
                    )
    in
    if List.length guests > 0 then
        div []
            [ h2 [ class "subtitle has-text-weight-semibold", onMouseEnter (OnPendingHover True), onMouseLeave (OnPendingHover False) ]
                [ text T.pending
                , a
                    [ class "button is-small is-primary mx-3"
                    , classList [ ( "is-invisible", not pending_hover ) ]
                    , href <| toHref <| Route.Tension_Dynamic_Dynamic_Contract { param1 = "", param2 = tid }
                    ]
                    [ text T.goContracts ]
                ]
            , div [ class "table-container", style "min-width" "375px" ]
                [ div [ class "table is-fullwidth" ]
                    [ thead []
                        [ tr []
                            [ th [] [ text T.username ]
                            , th [] [ text T.name ]
                            ]
                        ]
                    , tbody [] <|
                        List.indexedMap
                            (\i m ->
                                tr [ onMouseEnter (OnPendingRowHover (Just i)), onMouseLeave (OnPendingRowHover Nothing) ]
                                    [ td [] [ viewUsernameLink m.username ]
                                    , td [] [ m.name |> withDefault "--" |> text ]
                                    , if Just i == pending_hover_i then
                                        td [] [ div [ class "button is-small is-primary", onClick (OnGoToContract m.username) ] [ text T.goContract ] ]

                                      else
                                        text ""
                                    ]
                            )
                            guests
                    ]
                ]
            ]

    else
        div [] []


viewMemberRow : Conf -> NodeFocus -> Member -> Bool -> Html Msg
viewMemberRow conf focus m isPanelOpen =
    let
        ( roles_, sub_roles_ ) =
            List.foldl
                (\r ( roles, sub_roles ) ->
                    if (Maybe.map .nameid r.parent |> withDefault focus.nameid) == focus.nameid then
                        ( r :: roles, sub_roles )

                    else
                        ( roles, r :: sub_roles )
                )
                ( [], [] )
                m.roles
                |> (\( x, y ) -> ( List.reverse x, List.reverse y ))
    in
    tr []
        [ td [ class "pt-2 pr-0" ] [ viewUser True m.username ]
        , td [ class "pt-3" ] [ viewUsernameLink m.username ]
        , td [ class "pt-3" ] [ m.name |> withDefault "--" |> text ]
        , td [ class "pt-3" ]
            [ case roles_ of
                [] ->
                    text "--"

                _ ->
                    viewMemberRoles conf OverviewBaseUri roles_ isPanelOpen
            ]
        , td [ class "pt-3" ]
            [ case sub_roles_ of
                [] ->
                    text "--"

                _ ->
                    viewMemberRoles conf OverviewBaseUri sub_roles_ isPanelOpen
            ]
        ]


viewGuestRow : Conf -> Member -> Bool -> Html Msg
viewGuestRow conf m isPanelOpen =
    tr []
        [ td [ class "pt-2 pr-0" ] [ viewUser True m.username ]
        , td [ class "pt-3" ] [ viewUsernameLink m.username ]
        , td [ class "pt-3" ] [ m.name |> withDefault "--" |> text ]
        , td [ class "pt-3" ] [ viewMemberRoles conf OverviewBaseUri m.roles isPanelOpen ]
        ]


viewMemberRoles : Conf -> FractalBaseRoute -> List UserRoleExtended -> Bool -> Html Msg
viewMemberRoles conf baseUri roles isPanelOpen =
    div [ class "buttons" ] <|
        List.map
            (\r ->
                viewRole2 (Just ( conf, r.createdAt )) r (ternary isPanelOpen (\_ _ _ -> NoMsg) OpenActionPanel)
            )
            roles



--
-- Utils
--
--


memberRolesFilter : List UserRoleExtended -> List UserRoleExtended
memberRolesFilter roles =
    roles
        |> List.concatMap
            (\r ->
                if List.member r.role_type [ RoleType.Guest, RoleType.Pending ] then
                    -- Filter Special roles
                    []

                else if r.role_type == RoleType.Member && List.length roles > 1 then
                    -- Filter Member with roles
                    []

                else
                    [ r ]
            )
