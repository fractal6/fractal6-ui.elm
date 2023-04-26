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


module Org.Project exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), hasLazyAdminRole)
import Browser.Navigation as Nav
import Bulk exposing (..)
import Bulk.Codecs exposing (ActionType(..), DocType(..), Flags_, FractalBaseRoute(..), NodeFocus, contractIdCodec, focusFromNameid, focusState, id3Changed, nameidFromFlags, nearestCircleid, uriFromNameid)
import Bulk.Error exposing (viewGqlErrors)
import Bulk.View exposing (viewRole, viewUserFull)
import Components.ActionPanel as ActionPanel
import Components.AuthModal as AuthModal
import Components.HelperBar as HelperBar
import Components.JoinOrga as JoinOrga
import Components.OrgaMenu as OrgaMenu
import Components.SearchBar exposing (viewSearchBar)
import Components.TreeMenu as TreeMenu
import Dict
import Extra exposing (ternary, unwrap)
import Extra.Url exposing (queryBuilder, queryParser)
import Form.Help as Help
import Form.NewTension as NTF exposing (NewTensionInput(..), TensionTab(..))
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Generated.Route as Route exposing (toHref)
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, div, h2, i, input, span, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, classList, href, id, style, type_)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Html.Lazy as Lazy
import List.Extra as LE
import Loading exposing (GqlData, RequestResult(..), fromMaybeData, withDefaultData, withMapData, withMaybeData)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.QueryNode exposing (queryLocalGraph)
import Query.QueryProject exposing (queryProject)
import Session exposing (Conf, GlobalCmd(..))
import Task
import Text as T
import Time
import Url


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
                        ( [ Cmd.map TreeMenuMsg <| send (TreeMenu.UpdateNode nameid fun), send DoLoad ], Cmd.none )

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
    , projectid : String
    , project_data : GqlData ProjectData

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
-- INIT
--


type alias Flags =
    { param1 : String, param2 : String }


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        apis =
            global.session.apis

        conf =
            { screen = global.session.screen, now = global.now, lang = global.session.lang, url = global.url }

        -- Query parameters
        query =
            queryParser global.url

        -- Focus
        rootnameid =
            flags.param1 |> Url.percentDecode |> withDefault ""

        projectid =
            "0x" ++ flags.param2

        newFocus =
            NodeFocus rootnameid rootnameid NodeType.Circle

        -- What has changed
        fs =
            focusState ProjectBaseUri global.session.referer global.url global.session.node_focus newFocus

        model =
            { node_focus = newFocus
            , path_data =
                global.session.path_data
                    |> Maybe.map (\x -> Success x)
                    |> withDefault Loading
            , projectid = projectid
            , project_data = ternary fs.orgChange Loading (fromMaybeData global.session.project_data Loading)

            -- Common
            , conf = conf
            , tensionForm = NTF.init global.session.user conf
            , refresh_trial = 0
            , empty = {}
            , helperBar = HelperBar.init ProjectsBaseUri global.url.query newFocus global.session.user
            , help = Help.init global.session.user conf
            , joinOrga = JoinOrga.init newFocus.nameid global.session.user global.session.screen
            , authModal = AuthModal.init global.session.user Nothing
            , orgaMenu = OrgaMenu.init newFocus global.session.orga_menu global.session.orgs_data global.session.user
            , treeMenu = TreeMenu.init ProjectsBaseUri global.url.query newFocus global.session.tree_menu global.session.tree_data global.session.user
            , actionPanel = ActionPanel.init global.session.user global.session.screen
            }

        cmds =
            [ ternary fs.focusChange (queryLocalGraph apis newFocus.nameid True (GotPath True)) Cmd.none
            , send DoLoad
            , sendSleep PassedSlowLoadTreshold 500
            , Cmd.map OrgaMenuMsg (send OrgaMenu.OnLoad)
            , Cmd.map TreeMenuMsg (send TreeMenu.OnLoad)
            ]

        refresh =
            Maybe.map (\x -> id3Changed x.id global.url) global.session.project_data |> withDefault True
    in
    ( model
    , Cmd.batch cmds
    , if fs.menuChange || refresh then
        send (UpdateSessionFocus (Just newFocus))

      else
        Cmd.none
    )



--
-- Msg
--


type Msg
    = --Loading
      PassedSlowLoadTreshold -- timer
    | Submit (Time.Posix -> Msg) -- Get Current Time
    | GotPath Bool (GqlData LocalGraph) -- GraphQL
      -- Page
    | DoLoad
    | GotProject (GqlData ProjectData) -- Rest
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

        DoLoad ->
            ( model
            , Cmd.batch
                [ queryProject apis model.projectid GotProject
                ]
            , Cmd.none
            )

        GotProject result ->
            ( { model = result }, Cmd.none, Cmd.none )

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
            ( model, Cmd.none, send (NavigateRaw (uriFromNameid ProjectsBaseUri model.node_focus.rootnameid [] ++ query)) )

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
            ++ T.projects
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
        isAdmin =
            case global.session.user of
                LoggedIn uctx ->
                    hasLazyAdminRole uctx (withMaybeData model.path_data |> unwrap Nothing (\p -> Maybe.map .mode p.root)) model.node_focus.rootnameid

                LoggedOut ->
                    False

        isRoot =
            model.node_focus.nameid == model.node_focus.rootnameid

        isPanelOpen =
            ActionPanel.isOpen_ "actionPanelHelper" model.actionPanel

        opSearch =
            { onChangePattern = ChangePattern
            , onSearchKeyDown = SearchKeyDown
            , onSubmitText = SubmitTextSearch
            , id_name = "searchBarMembers"
            , placeholder_txt = T.searchMembers
            }
    in
    div [ class "columns is-centered" ]
        [ div [ class "column is-12 is-11-desktop is-9-fullhd" ]
            [ div [ class "columns is-centered" ]
                [ div [ class "column is-four-fifth" ]
                    [ viewSearchBar opSearch model.pattern_init model.pattern ]
                , if isAdmin then
                    div [ class "column is-one-fifth is-flex is-align-self-flex-start" ]
                        [ div
                            [ class "button is-primary is-pushed-right"
                            , onClick (JoinOrgaMsg (JoinOrga.OnOpen model.node_focus.rootnameid JoinOrga.InviteOne))
                            ]
                            [ A.icon1 "icon-user-plus" T.inviteMembers ]
                        ]

                  else
                    text ""
                ]
            , div [ class "columns is-centered" ]
                [ div [ class "column is-four-fifth" ]
                    [ div [ class "columns mb-6 px-3" ]
                        [ Lazy.lazy4 viewMembers model.conf model.members_sub model.node_focus isPanelOpen ]
                    , div [ class "columns mb-6 px-3" ]
                        [ if isRoot then
                            div [ class "column is-5 pl-0" ] [ Lazy.lazy4 viewGuest model.conf model.members_top model.node_focus isPanelOpen ]

                          else
                            text ""
                        ]
                    ]
                , div [ class "column is-one-fifth is-flex is-align-self-flex-start" ]
                    [ if isRoot then
                        let
                            rtid =
                                tidFromPath model.path_data |> withDefault ""
                        in
                        div [ class "is-pushed-right" ]
                            [ viewPending model.conf model.members_top model.node_focus model.pending_hover model.pending_hover_i rtid ]

                      else
                        text ""
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
                    [ h2 [ class "subtitle has-text-weight-semibold" ] [ text T.members, goToParent ]
                    , div [ class "table-container" ]
                        [ div [ class "table is-fullwidth" ]
                            [ thead []
                                [ tr []
                                    [ th [] [ text T.user ]
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
                            [ th [] [ text T.user ]
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
                    [ class "button is-small is-primary ml-3"
                    , classList [ ( "is-invisible", not pending_hover ) ]
                    , href <| toHref <| Route.Tension_Dynamic_Dynamic_Contract { param1 = "", param2 = tid }
                    ]
                    [ text T.goContracts ]
                ]
            , div [ class "table-container", style "min-width" "375px" ]
                [ div [ class "table is-fullwidth" ]
                    [ thead []
                        [ tr []
                            [ th [] [ text T.user ]
                            ]
                        ]
                    , tbody [] <|
                        List.indexedMap
                            (\i m ->
                                tr [ onMouseEnter (OnPendingRowHover (Just i)), onMouseLeave (OnPendingRowHover Nothing) ]
                                    [ td [] [ viewUserFull 1 True False m ]
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
        [ td [] [ viewUserFull 1 True False m ]
        , td []
            [ case roles_ of
                [] ->
                    text "--"

                _ ->
                    viewMemberRoles conf OverviewBaseUri roles_ isPanelOpen
            ]
        , td []
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
        [ td [] [ viewUserFull 1 True False m ]
        , td [] [ viewMemberRoles conf OverviewBaseUri m.roles isPanelOpen ]
        ]


viewMemberRoles : Conf -> FractalBaseRoute -> List UserRoleExtended -> Bool -> Html Msg
viewMemberRoles conf baseUri roles isPanelOpen =
    div [ class "buttons" ] <|
        List.map
            (\r ->
                viewRole "" True False (Just ( conf, r.createdAt )) Nothing (ternary isPanelOpen (\_ _ _ -> NoMsg) OpenActionPanel) r
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
