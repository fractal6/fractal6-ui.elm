{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2024 Fractale Co

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

import Assets as A
import Auth exposing (ErrState(..), hasLazyAdminRole)
import Browser.Events as Events
import Browser.Navigation as Nav
import Bulk exposing (..)
import Bulk.Bulma as B
import Bulk.Codecs exposing (ActionType(..), DocType(..), Flags_, FractalBaseRoute(..), NodeFocus, contractIdCodec, focusFromNameid, focusState, isOwner, nameidFromFlags, nearestCircleid, nid2rootid, toLink)
import Bulk.Error exposing (viewGqlErrors)
import Bulk.View exposing (role2icon, roleColor, viewRole, viewUserFull)
import Components.ActionPanel as ActionPanel
import Components.AuthModal as AuthModal
import Components.ConfirmOwner as ConfirmOwner
import Components.HelperBar as HelperBar
import Components.JoinOrga as JoinOrga
import Components.OrgaMenu as OrgaMenu
import Components.SearchBar exposing (viewSearchBar)
import Components.TreeMenu as TreeMenu
import Dict
import Dom
import Extra exposing (colorAttr, showIf, space_, ternary, unwrap, unwrap2, upH)
import Extra.Date exposing (formatDate)
import Extra.Url exposing (queryBuilder, queryParser)
import Form.Help as Help
import Form.NewTension as NTF
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Generated.Route as Route exposing (toHref)
import Global exposing (Msg(..), getConf, send, sendNow, sendSleep)
import Html exposing (Html, a, div, h2, hr, i, input, span, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (attribute, class, classList, href, id, style, type_)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Html.Lazy as Lazy
import Json.Decode as JD
import List.Extra as LE
import Loading exposing (GqlData, RequestResult(..), RestData, withDefaultData, withMapData, withMapDataRest, withMaybeData)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.QueryContract exposing (getContractId, queryOpenInvitation)
import Query.QueryNode exposing (queryLocalGraph, queryMembersLocal)
import Query.QueryUser exposing (queryUserRoles)
import RemoteData
import Requests exposing (fetchMembersSub)
import Session exposing (Conf, GlobalCmd(..), isMobile)
import String.Format as Format
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

                    DoPushSystemNotif a ->
                        ( [], send (OnPushSystemNotif a) )

                    -- Component
                    DoCreateTension a ntm d ->
                        case ntm of
                            Nothing ->
                                ( [ Cmd.map NewTensionMsg <| send (NTF.OnOpen (FromNameid a) d) ], Cmd.none )

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
                        ( [ Cmd.map TreeMenuMsg <| sendSleep (TreeMenu.FetchNewNode nameid False) 333, sendSleep DoLoad 333 ], Cmd.none )

                    DoAddNodes nodes ->
                        ( [ Cmd.map TreeMenuMsg <| send (TreeMenu.AddNodes nodes), send DoLoad ], Cmd.none )

                    --DoUpdateNode nameid fun ->
                    --    ( Cmd.map TreeMenuMsg <| send (TreeMenu.UpdateNode nameid fun), Cmd.none )
                    DoDelNodes nameids ->
                        ( [ Cmd.map TreeMenuMsg <| send (TreeMenu.DelNodes nameids), send DoLoad ], Cmd.none )

                    DoMoveNode a b c ->
                        ( [ Cmd.map TreeMenuMsg <| send (TreeMenu.MoveNode a b c), send DoLoad ], Cmd.none )

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
    , members_top : GqlData (List Member)
    , members_sub : GqlData (List Member)
    , open_invitations : GqlData (List ContractLight)
    , pending_hover : Bool
    , pending_hover_i : Maybe Int
    , pattern : String
    , pattern_init : String
    , row_hover : Ellipsis

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
    , confirmOwner : ConfirmOwner.State
    }


type alias Ellipsis =
    { hover : Maybe String
    , isOpen : Bool
    }


resetEllipsis : Ellipsis
resetEllipsis =
    { hover = Nothing, isOpen = False }



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
            getConf global

        -- Query parameters
        query =
            queryParser global.url

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
            , open_invitations = Loading
            , pending_hover = False
            , pending_hover_i = Nothing
            , pattern = Dict.get "q" query |> withDefault [] |> List.head |> withDefault ""
            , pattern_init = Dict.get "q" query |> withDefault [] |> List.head |> withDefault ""
            , row_hover = resetEllipsis

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
            , treeMenu = TreeMenu.init MembersBaseUri global.url.query newFocus global.session.user global.session.tree_menu global.session.tree_data
            , actionPanel = ActionPanel.init global.session.user global.session.screen
            , confirmOwner = ConfirmOwner.init global.session.user newFocus
            }

        cmds =
            [ ternary fs.focusChange (queryLocalGraph apis newFocus.nameid True (GotPath True)) Cmd.none
            , send DoLoad
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



--
-- Msg
--


type Msg
    = -- Loading
      PassedSlowLoadTreshold -- timer
    | Submit (Time.Posix -> Msg) -- Get Current Time
    | DoLoad
    | GotPath Bool (GqlData LocalGraph)
    | GotMembers (GqlData (List Member))
      --| GotUserRoles (GqlData (List Member))
    | GotMembersSub (GqlData (List Member))
    | GotOpenContracts (GqlData (List ContractLight))
      -- Page
    | OnPendingHover Bool
    | OnPendingRowHover (Maybe Int)
    | OnGoToContract String
    | OnGoContractAck (GqlData IdPayload)
    | OnRowHover (Maybe String)
    | OnRowEdit Bool
      -- Search
    | ChangePattern String
    | SearchKeyDown Int
    | ResetData
    | SubmitSearch
    | SubmitTextSearch String
    | SubmitSearchReset
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
    | ConfirmOwnerMsg ConfirmOwner.Msg


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
            ( model, sendNow nextMsg, Cmd.none )

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
                                    List.head path.path |> Maybe.map .nameid |> withDefault ""
                            in
                            ( { model | path_data = Success newPath }, queryLocalGraph apis nameid False (GotPath False), Cmd.none )

                _ ->
                    ( { model | path_data = result }, Cmd.none, Cmd.none )

        DoLoad ->
            let
                pattern_m =
                    case model.pattern of
                        "" ->
                            Nothing

                        a ->
                            Just a
            in
            ( model
            , Cmd.batch
                [ queryMembersLocal apis model.node_focus.rootnameid pattern_m GotMembers

                -- @deprecated : we use gql queryUserRoles now (allow to search users too..to be tested)
                , fetchMembersSub apis model.node_focus.nameid GotMembersSub
                , queryOpenInvitation apis model.node_focus.nameid GotOpenContracts
                ]
            , Cmd.none
            )

        GotMembers result ->
            let
                newModel =
                    { model | members_top = result }
            in
            case result of
                Success m ->
                    let
                        users =
                            List.map .username m

                        pattern_m =
                            case model.pattern of
                                "" ->
                                    Nothing

                                a ->
                                    Just a
                    in
                    --( newModel, queryUserRoles apis model.node_focus.rootnameid users pattern_m GotUserRoles, Cmd.none )
                    ( newModel, Cmd.none, Cmd.none )

                _ ->
                    ( newModel, Cmd.none, Cmd.none )

        --GotUserRoles result ->
        --    case result of
        --        Success data ->
        --            let
        --                path =
        --                    -- path from nameid (not included) to root node
        --                    withMapData .path model.path_data |> withDefaultData [] |> List.map .nameid |> List.filter (\x -> x /= model.node_focus.nameid)
        --                ur =
        --                    -- Filter roles in above circles
        --                    List.map
        --                        (\y -> { y | roles = List.filter (\z -> not (List.member (nearestCircleid z.nameid) path)) y.roles })
        --                        data
        --            in
        --            ( { model | members_sub = Success ur }, Cmd.none, Cmd.none )
        --        _ ->
        --            ( { model | members_sub = result }, Cmd.none, Cmd.none )
        GotMembersSub result ->
            case result of
                Success mbs ->
                    let
                        reorderByMembership roles =
                            roles
                                |> List.foldl
                                    (\role ( membershipRoles, otherRoles ) ->
                                        if role.role_type == RoleType.Owner then
                                            ( role :: membershipRoles, otherRoles )

                                        else
                                            ( membershipRoles, role :: otherRoles )
                                    )
                                    ( [], [] )
                                |> (\( a, b ) -> a ++ b)
                    in
                    ( { model
                        | members_sub =
                            List.filterMap
                                (\m ->
                                    case memberRolesFilter m.roles of
                                        [] ->
                                            Nothing

                                        roles ->
                                            Just { m | roles = reorderByMembership roles }
                                )
                                mbs
                                |> Success
                      }
                    , Cmd.none
                    , Cmd.none
                    )

                _ ->
                    ( { model | members_sub = result }, Cmd.none, Cmd.none )

        GotOpenContracts result ->
            ( { model | open_invitations = result }, Cmd.none, Cmd.none )

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

        OnRowHover id_m ->
            if model.row_hover.isOpen then
                ( model, Cmd.none, Cmd.none )

            else
                let
                    row =
                        model.row_hover
                in
                ( { model | row_hover = { row | hover = id_m } }, Cmd.none, Cmd.none )

        OnRowEdit edit ->
            let
                row =
                    model.row_hover
            in
            ( { model | row_hover = { row | isOpen = edit } }, Cmd.none, Cmd.none )

        -- Search
        ChangePattern value ->
            ( { model | pattern = value }, Cmd.none, Cmd.none )

        SearchKeyDown key ->
            case key of
                13 ->
                    --ENTER
                    ( model, send (SubmitTextSearch model.pattern), Cmd.none )

                27 ->
                    --ESC
                    ( model, send (ChangePattern ""), Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        SubmitSearch ->
            let
                query =
                    queryBuilder
                        [ ( "q", model.pattern |> String.trim )
                        ]
                        |> (\q -> ternary (q == "") "" ("?" ++ q))
            in
            ( model, Nav.pushUrl global.key (toLink MembersBaseUri model.node_focus.nameid [] ++ query), Cmd.none )

        SubmitTextSearch pattern ->
            if (pattern |> String.trim) == model.pattern_init then
                ( model, Cmd.none, Cmd.none )

            else
                ( { model | pattern = pattern }, send SubmitSearchReset, Cmd.none )

        SubmitSearchReset ->
            -- Send search and reset the other results
            ( model, Cmd.batch [ send SubmitSearch, send ResetData ], Cmd.none )

        ResetData ->
            ( { model | members_top = Loading, members_sub = Loading, path_data = Loading }, Cmd.none, Cmd.none )

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
            ( model, Cmd.none, send (NavigateRaw (toLink MembersBaseUri model.node_focus.rootnameid [] ++ query)) )

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

        ConfirmOwnerMsg msg ->
            let
                ( data, out ) =
                    ConfirmOwner.update apis msg model.confirmOwner

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | confirmOwner = data }, out.cmds |> List.map (\m -> Cmd.map ConfirmOwnerMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions _ model =
    []
        ++ (if model.row_hover.isOpen then
                [ Events.onMouseUp (JD.succeed (OnRowEdit False))
                , Events.onKeyUp (Dom.key "Escape" (OnRowEdit False))
                ]

            else
                []
           )
        ++ (HelperBar.subscriptions |> List.map (\s -> Sub.map HelperBarMsg s))
        ++ (Help.subscriptions |> List.map (\s -> Sub.map HelpMsg s))
        ++ (NTF.subscriptions model.tensionForm |> List.map (\s -> Sub.map NewTensionMsg s))
        ++ (JoinOrga.subscriptions model.joinOrga |> List.map (\s -> Sub.map JoinOrgaMsg s))
        ++ (AuthModal.subscriptions |> List.map (\s -> Sub.map AuthModalMsg s))
        ++ (OrgaMenu.subscriptions |> List.map (\s -> Sub.map OrgaMenuMsg s))
        ++ (TreeMenu.subscriptions |> List.map (\s -> Sub.map TreeMenuMsg s))
        ++ (ActionPanel.subscriptions model.actionPanel |> List.map (\s -> Sub.map ActionPanelMsg s))
        ++ (ConfirmOwner.subscriptions model.confirmOwner |> List.map (\s -> Sub.map ConfirmOwnerMsg s))
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
        , Lazy.lazy2 Help.view model.empty model.help |> Html.map HelpMsg
        , Lazy.lazy3 NTF.view (TreeMenu.getOrgaData_ model.treeMenu) model.path_data model.tensionForm |> Html.map NewTensionMsg
        , Lazy.lazy2 JoinOrga.view model.empty model.joinOrga |> Html.map JoinOrgaMsg
        , Lazy.lazy2 AuthModal.view model.empty model.authModal |> Html.map AuthModalMsg
        , Lazy.lazy2 OrgaMenu.view model.empty model.orgaMenu |> Html.map OrgaMenuMsg
        , Lazy.lazy2 TreeMenu.view model.empty model.treeMenu |> Html.map TreeMenuMsg
        , ActionPanel.view panelData model.actionPanel |> Html.map ActionPanelMsg
        , ConfirmOwner.view model.empty model.confirmOwner |> Html.map ConfirmOwnerMsg
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

        row_hover =
            -- Note: For admin user, row_hover change at every hover, so the lazy function will be defeat.
            if isAdmin then
                model.row_hover

            else
                resetEllipsis

        ( guests, pendings ) =
            model.members_top
                |> withDefaultData []
                |> (\x ->
                        ( List.filter
                            (\u ->
                                u.roles
                                    |> List.map .role_type
                                    |> List.member RoleType.Guest
                            )
                            x
                        , List.filter
                            (\u ->
                                u.roles
                                    |> List.map .role_type
                                    |> List.member RoleType.Pending
                            )
                            x
                        )
                   )
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
            , div [ class "columns mb-6" ]
                [ div [ class "column is-four-fifth", onMouseLeave (OnRowHover Nothing) ]
                    [ Lazy.lazy6 viewMembers model.conf model.members_sub model.open_invitations model.node_focus isPanelOpen row_hover
                    ]
                ]
            , if isRoot then
                let
                    rtid =
                        tidFromPath model.path_data |> withDefault ""
                in
                div [ class "columns mb-6" ]
                    [ showIf (List.length guests > 0) <|
                        div [ class "column is-5", onMouseLeave (OnRowHover Nothing) ]
                            [ Lazy.lazy6 viewGuest model.conf guests model.open_invitations model.node_focus isPanelOpen row_hover
                            ]
                    , showIf (List.length pendings > 0) <|
                        div [ class "column is-2 is-flex is-align-self-flex-start", classList [ ( "is-offset-2", List.length guests > 0 ) ] ]
                            [ viewPending model.conf pendings model.node_focus model.pending_hover model.pending_hover_i rtid
                            ]
                    ]

              else
                text ""
            ]
        ]


viewMembers : Conf -> GqlData (List Member) -> GqlData (List ContractLight) -> NodeFocus -> Bool -> Ellipsis -> Html Msg
viewMembers conf members_d invitations_d focus isPanelOpen ell =
    let
        goToParent =
            if focus.nameid /= focus.rootnameid then
                span [ class "help-label button-light is-h is-discrete", onClick OnGoRoot ] [ A.icon "arrow-up", text T.goRoot ]

            else
                text ""
    in
    case members_d of
        Success members ->
            if List.length members == 0 then
                div [] [ text T.noMemberYet, goToParent ]

            else
                let
                    invitations =
                        withDefaultData [] invitations_d
                            |> List.filter
                                (\c ->
                                    List.any (\u -> List.member u (List.map .username members)) (List.map .username c.candidates)
                                )

                    hasInvitation =
                        invitations /= []
                in
                div []
                    [ h2 [ class "subtitle has-text-weight-semibold" ] [ text T.members, goToParent ]
                    , div [ class "table-container" ]
                        [ div [ class "table is-fullwidth" ]
                            [ thead []
                                [ tr []
                                    [ th [] [ text T.user ]
                                    , th [] [ text T.rolesHere ]
                                    , th [] [ text T.rolesSub ]
                                    , showIf (invitations /= []) <| th [] [ text T.pendingRoles ]
                                    , th [] [] -- for the ellipsis
                                    ]
                                ]
                            , tbody [ class "pr-5" ] <|
                                List.map
                                    (\m ->
                                        Lazy.lazy7 viewMemberRow conf focus m invitations_d hasInvitation isPanelOpen ell
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


viewGuest : Conf -> List Member -> GqlData (List ContractLight) -> NodeFocus -> Bool -> Ellipsis -> Html Msg
viewGuest conf guests invitations_d focus isPanelOpen ell =
    let
        invitations =
            withDefaultData [] invitations_d
                |> List.filter
                    (\c ->
                        List.any (\u -> List.member u (List.map .username guests)) (List.map .username c.candidates)
                    )

        hasInvitation =
            invitations /= []
    in
    div []
        [ h2 [ class "subtitle has-text-weight-semibold" ] [ text T.guests ]
        , div [ class "table-container" ]
            [ div [ class "table is-fullwidth" ]
                [ thead []
                    [ tr []
                        [ th [] [ text T.user ]
                        , th [] [ text T.roles ]
                        , showIf (invitations /= []) <| th [] [ text T.pendingRoles ]
                        , th [] [] -- for the ellipsis
                        ]
                    ]
                , tbody [] <|
                    List.indexedMap
                        (\i m ->
                            Lazy.lazy7 viewGuestRow conf focus m invitations_d hasInvitation isPanelOpen ell
                        )
                        guests
                ]
            ]
        ]


viewPending : Conf -> List Member -> NodeFocus -> Bool -> Maybe Int -> String -> Html Msg
viewPending _ pendings focus pending_hover pending_hover_i tid =
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
                        pendings
                ]
            ]
        ]


viewMemberRow : Conf -> NodeFocus -> Member -> GqlData (List ContractLight) -> Bool -> Bool -> Ellipsis -> Html Msg
viewMemberRow conf focus m invitations_d hasInvitation isPanelOpen ell =
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

        user_invitations =
            withDefaultData [] invitations_d
                |> List.filter (\i -> List.member m.username (List.map .username i.candidates))
    in
    tr [ onMouseEnter (OnRowHover (Just ("member" ++ m.username))) ]
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
        , if not hasInvitation then
            text ""

          else if user_invitations == [] then
            td [] [ text "--" ]

          else
            td [] [ viewPendingRoles conf user_invitations ]
        , td []
            [ showIf (ell.hover == Just ("member" ++ m.username) || isMobile conf.screen) <|
                viewUserEllipsis conf focus m roles_ ell
            ]
        ]


viewGuestRow : Conf -> NodeFocus -> Member -> GqlData (List ContractLight) -> Bool -> Bool -> Ellipsis -> Html Msg
viewGuestRow conf focus m invitations_d hasInvitation isPanelOpen ell =
    let
        user_invitations =
            withDefaultData [] invitations_d
                |> List.filter (\i -> List.member m.username (List.map .username i.candidates))
    in
    tr [ onMouseEnter (OnRowHover (Just ("guest" ++ m.username))) ]
        [ td [] [ viewUserFull 1 True False m ]
        , td [] [ viewMemberRoles conf OverviewBaseUri m.roles isPanelOpen ]
        , if not hasInvitation then
            text ""

          else if user_invitations == [] then
            td [] [ text "--" ]

          else
            td [] [ viewPendingRoles conf user_invitations ]
        , td []
            [ showIf (ell.hover == Just ("guest" ++ m.username) || isMobile conf.screen) <|
                viewUserEllipsis conf focus m m.roles ell
            ]
        ]


viewMemberRoles : Conf -> FractalBaseRoute -> List UserRoleExtended -> Bool -> Html Msg
viewMemberRoles conf baseUri roles isPanelOpen =
    div [ class "buttons is-inline" ] <|
        List.map
            (\r ->
                viewRole "" True False (Just ( conf, r.createdAt )) Nothing (ternary isPanelOpen (\_ _ _ -> NoMsg) OpenActionPanel) r
            )
            roles


viewUserEllipsis : Conf -> NodeFocus -> Member -> List UserRoleExtended -> Ellipsis -> Html Msg
viewUserEllipsis conf focus m roles ell =
    let
        isOwner_ =
            isOwner (uctxFromUser conf.user) focus.nameid

        isMobile_ =
            isMobile conf.screen

        isOpen_ =
            ell.isOpen && String.endsWith m.username (withDefault "" ell.hover)
    in
    span [ class "is-pulled-right" ]
        [ span [ class "outside-table", classList [ ( "is-mobile", isMobile_ ) ] ]
            [ B.dropdownLight
                { dropdown_id = "row-ellipsis"
                , isOpen = isOpen_
                , dropdown_cls = ternary isMobile_ "is-right" ""
                , button_cls = ""
                , button_html = A.icon "icon-more-vertical is-h icon-1half"
                , msg = OnRowEdit (ternary ell.isOpen False True)
                , menu_cls = ""
                , content_cls = "p-0 has-border-light"
                , content_html =
                    div []
                        ([ div [ class "dropdown-item button-light", onClick (NewTensionMsg (NTF.OnOpenRoleUser (FromNameid focus.nameid) m.username)) ]
                            [ A.icon1 "icon-leaf" T.addUserRole ]
                         ]
                            ++ (if isOwner_ && not (List.any (\x -> x.role_type == RoleType.Owner) roles) then
                                    [ hr [ class "dropdown-divider" ] []
                                    , div [ class "dropdown-item button-light is-warning", onClick (ConfirmOwnerMsg (ConfirmOwner.OnOpen m.username)) ]
                                        [ A.icon1 "icon-queen" T.makeOwner ]
                                    ]

                                else
                                    []
                               )
                        )
                }
            ]
        ]


viewPendingRoles : Conf -> List ContractLight -> Html Msg
viewPendingRoles conf invitations =
    div [ class "buttons is-inline" ] <|
        List.map (\c -> viewPendingRole conf c) invitations


viewPendingRole : Conf -> ContractLight -> Html Msg
viewPendingRole conf c =
    let
        tooltip_cls =
            String.split " " "tooltip has-tooltip-arrow is-multiline has-tooltip-text-left"

        since =
            T.createdThe ++ " " ++ formatDate conf.lang conf.now c.createdAt

        role =
            { name = c.tension.node |> unwrap2 "" .name
            , role_type = c.tension.node |> unwrap2 RoleType.Pending .role_type
            , nameid = c.tension.receiverid
            }

        link =
            Route.Tension_Dynamic_Dynamic_Contract_Dynamic { param1 = nid2rootid role.nameid, param2 = c.tension.id, param3 = c.id } |> toHref
    in
    a
        [ class "button buttonRole is-small pending-border"
        , classList (List.map (\x -> ( x, True )) tooltip_cls)
        , attribute "data-tooltip"
            (T.theyPlay
                |> Format.namedValue "role" (upH role.name)
                |> Format.namedValue "circle" (getParentFragmentFromRole role)
                |> Format.namedValue "since" since
            )
        , href link
        , colorAttr (roleColor role.role_type)
        , style "opacity" "0.75"
        ]
        [ A.icon1 (role2icon role) (upH role.name) ]



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
