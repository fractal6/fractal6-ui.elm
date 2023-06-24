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
import Bulk.Codecs exposing (ActionType(..), DocType(..), Flags_, FractalBaseRoute(..), NodeFocus, contractIdCodec, focusFromNameid, focusState, id3Changed, nameidFromFlags, nearestCircleid, toLink)
import Bulk.Error exposing (viewGqlErrors, viewGqlErrorsLight)
import Bulk.View exposing (viewRole, viewUserFull)
import Components.ActionPanel as ActionPanel
import Components.AuthModal as AuthModal
import Components.Board as Board
import Components.CardPanel as CardPanel
import Components.HelperBar as HelperBar
import Components.JoinOrga as JoinOrga
import Components.LinkTensionPanel as LinkTensionPanel exposing (ColTarget)
import Components.OrgaMenu as OrgaMenu
import Components.ProjectColumnModal as ProjectColumnModal exposing (ModalType(..))
import Components.SearchBar exposing (viewSearchBar)
import Components.TreeMenu as TreeMenu
import Dict
import Extra exposing (insertAt, ternary, unwrap)
import Extra.Url exposing (queryBuilder, queryParser)
import Fifo exposing (Fifo)
import Form.Help as Help
import Form.NewTension as NTF
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.ProjectColumnType as ProjectColumnType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Generated.Route as Route exposing (toHref)
import Global exposing (Msg(..), send, sendNow, sendSleep)
import Html exposing (Html, a, button, div, h2, hr, i, input, span, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (attribute, class, classList, href, id, style, type_)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Html.Lazy as Lazy
import List.Extra as LE
import Loading exposing (GqlData, RequestResult(..), fromMaybeData, isFailure, isLoading, withDefaultData, withMapData, withMaybeData, withMaybeMapData)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.QueryNode exposing (queryLocalGraph)
import Query.QueryProject exposing (getProject)
import Session exposing (Conf, GlobalCmd(..))
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

                    DoUpdateScreen a ->
                        ( [], send (UpdateSessionScreen a) )

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

                    DoOpenLinkTensionPanel a ->
                        ( [ send <| OpenTensionPane a ], Cmd.none )

                    DoOpenCardPanel a ->
                        ( [ Cmd.map CardPanelMsg (send (CardPanel.OnOpen a)) ], Cmd.none )

                    DoToggleTreeMenu ->
                        ( [ Cmd.map TreeMenuMsg <| send TreeMenu.OnToggle ], Cmd.none )

                    DoFetchNode nameid ->
                        ( [ Cmd.map TreeMenuMsg <| send (TreeMenu.FetchNewNode nameid False) ], Cmd.none )

                    DoAddNodes nodes ->
                        ( [ Cmd.map TreeMenuMsg <| send (TreeMenu.AddNodes nodes) ], Cmd.none )

                    DoUpdateNode nameid fun ->
                        ( [ Cmd.map TreeMenuMsg <| send (TreeMenu.UpdateNode nameid fun) ], Cmd.none )

                    DoDelNodes nameids ->
                        ( [ Cmd.map TreeMenuMsg <| send (TreeMenu.DelNodes nameids) ], Cmd.none )

                    DoMoveNode a b c ->
                        ( [ Cmd.map TreeMenuMsg <| send (TreeMenu.MoveNode a b c) ], Cmd.none )

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
    , linkTensionPanel : LinkTensionPanel.State
    , cardPanel : CardPanel.State

    -- Common
    , conf : Conf
    , refresh_trial : Int
    , board : Board.State
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
            , linkTensionPanel = LinkTensionPanel.init projectid global.session.user
            , cardPanel = CardPanel.init newFocus global.session.user
            , board = Board.init projectid newFocus global.session.user

            -- Common
            , conf = conf
            , refresh_trial = 0
            , empty = {}
            , tensionForm = NTF.init global.session.user conf
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
            , Ports.hide "footBar"
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
    | OpenTensionPane (Maybe ColTarget)
    | OnClearBoardResult
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
    | BoardMsg Board.Msg
    | LinkTensionPanelMsg LinkTensionPanel.Msg
    | CardPanelMsg CardPanel.Msg


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global message model =
    let
        apis =
            global.session.apis
    in
    case message of
        PassedSlowLoadTreshold ->
            let
                project_data =
                    ternary (model.project_data == Loading) LoadingSlowly model.project_data
            in
            ( { model | project_data = project_data }, Cmd.none, Cmd.none )

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
            ( model, Cmd.batch [ getProject apis model.projectid GotProject ], Cmd.none )

        GotProject result ->
            case result of
                Success data ->
                    -- save space
                    ( { model | project_data = Success { data | columns = [] } }
                    , Cmd.map BoardMsg (send (Board.OnLoad data))
                    , Cmd.none
                    )

                _ ->
                    ( { model | project_data = result }, Cmd.none, Cmd.none )

        OpenTensionPane colTarget ->
            ( model
            , Cmd.batch
                [ Cmd.map LinkTensionPanelMsg (send (LinkTensionPanel.OnOpen colTarget))
                , Cmd.map TreeMenuMsg (send TreeMenu.OnRequireData)
                ]
            , Cmd.none
            )

        OnClearBoardResult ->
            ( model, Cmd.map BoardMsg (send Board.OnClearBoardResult), Cmd.none )

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
            ( model, Cmd.none, send (NavigateRaw (toLink ProjectsBaseUri model.node_focus.rootnameid [] ++ query)) )

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

                convert_draft_cmd =
                    case out.result of
                        Just ( t, d ) ->
                            case d of
                                Just draft ->
                                    Cmd.map BoardMsg (send (Board.OnConvertDraftAck draft t))

                                Nothing ->
                                    send NoMsg

                        Nothing ->
                            send NoMsg

                ( cmds_, gcmds ) =
                    mapGlobalOutcmds out.gcmds

                cmds =
                    convert_draft_cmd :: cmds_
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

                extra_cmd =
                    if
                        ((out.result == Just ( True, True ))
                            || (out.result == Just ( True, False ))
                        )
                            && not (LinkTensionPanel.hasTargets_ model.linkTensionPanel)
                    then
                        send (LinkTensionPanelMsg (LinkTensionPanel.SetTargets model.path_data <| TreeMenu.getList_ model.node_focus.nameid data))

                    else
                        Cmd.none
            in
            ( { model | treeMenu = data }, out.cmds |> List.map (\m -> Cmd.map TreeMenuMsg m) |> List.append (extra_cmd :: cmds) |> Cmd.batch, Cmd.batch gcmds )

        ActionPanelMsg msg ->
            let
                ( data, out ) =
                    ActionPanel.update apis msg model.actionPanel

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | actionPanel = data }
            , out.cmds
                |> List.map (\m -> Cmd.map ActionPanelMsg m)
                |> List.append cmds
                |> Cmd.batch
            , Cmd.batch gcmds
            )

        LinkTensionPanelMsg msg ->
            let
                ( data, out ) =
                    LinkTensionPanel.update apis msg model.linkTensionPanel

                cmd =
                    case out.result of
                        Just x ->
                            -- @TODO : link tension in other views. Data shallow copy accross views !?
                            Cmd.map BoardMsg (send <| Board.OnLinkTension x)

                        Nothing ->
                            send NoMsg

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | linkTensionPanel = data }
            , out.cmds |> List.map (\m -> Cmd.map LinkTensionPanelMsg m) |> List.append (cmd :: cmds) |> Cmd.batch
            , Cmd.batch gcmds
            )

        BoardMsg msg ->
            let
                ( data, out ) =
                    Board.update apis msg model.board

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | board = data }, out.cmds |> List.map (\m -> Cmd.map BoardMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

        CardPanelMsg msg ->
            let
                ( data, out ) =
                    CardPanel.update apis msg model.cardPanel

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | cardPanel = data }, out.cmds |> List.map (\m -> Cmd.map CardPanelMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )


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
        ++ (LinkTensionPanel.subscriptions model.linkTensionPanel |> List.map (\s -> Sub.map LinkTensionPanelMsg s))
        ++ (Board.subscriptions model.board |> List.map (\s -> Sub.map BoardMsg s))
        ++ (CardPanel.subscriptions model.cardPanel |> List.map (\s -> Sub.map CardPanelMsg s))
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

        tree_data =
            TreeMenu.getOrgaData_ model.treeMenu
    in
    { title =
        case model.project_data of
            Success p ->
                T.project ++ " · " ++ p.name

            _ ->
                "Loading..."
    , body =
        [ div [ class "orgPane" ]
            [ HelperBar.view helperData model.helperBar |> Html.map HelperBarMsg
            , div [ id "mainPane" ]
                [ view_ global model
                , case model.project_data of
                    Success data ->
                        Board.view model.empty model.board |> Html.map BoardMsg

                    Failure err ->
                        viewGqlErrors err

                    _ ->
                        div [ class "spinner" ] []
                ]
            ]
        , Help.view model.empty model.help |> Html.map HelpMsg
        , NTF.view { tree_data = tree_data, path_data = model.path_data } model.tensionForm |> Html.map NewTensionMsg
        , JoinOrga.view model.empty model.joinOrga |> Html.map JoinOrgaMsg
        , AuthModal.view model.empty model.authModal |> Html.map AuthModalMsg
        , OrgaMenu.view model.empty model.orgaMenu |> Html.map OrgaMenuMsg
        , TreeMenu.view model.empty model.treeMenu |> Html.map TreeMenuMsg
        , ActionPanel.view panelData model.actionPanel |> Html.map ActionPanelMsg
        , LinkTensionPanel.view { tree_data = tree_data, path_data = model.path_data } model.linkTensionPanel |> Html.map LinkTensionPanelMsg
        , CardPanel.view { conf = model.conf, path_data = model.path_data } model.cardPanel |> Html.map CardPanelMsg
        ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    let
        isAdmin =
            case global.session.user of
                LoggedIn uctx ->
                    --hasAdminRole uctx (withMaybeData model.path_data)
                    hasLazyAdminRole uctx Nothing model.node_focus.rootnameid

                LoggedOut ->
                    False
    in
    div [ class "columns is-centered" ]
        [ div [ class "column is-12 is-11-desktop is-10-fullhd pb-0" ]
            [ div [ class "columns is-centered mb-0" ]
                [ div [ class "column is-tree-quarter pb-1" ]
                    [ case model.project_data of
                        Success p ->
                            viewSearchBar p model

                        _ ->
                            text ""
                    ]
                , div [ class "column is-one-quarter is-flex is-align-self-flex-start pt-0 pb-1" ]
                    [ div [ class "button is-small is-pushed-right", onClick (OpenTensionPane Nothing) ]
                        [ A.icon1 "icon-plus" "Add tensions to project" ]
                    ]
                ]

            -- User notification
            , case Board.board_result model.board of
                Failure err ->
                    div [ class "f6-notification notification is-danger is-light" ]
                        [ button [ class "delete", onClick OnClearBoardResult ] []
                        , viewGqlErrorsLight err
                        ]

                _ ->
                    text ""
            ]
        ]


viewSearchBar : ProjectData -> Model -> Html Msg
viewSearchBar project model =
    div [ id "searchBarProject", class "searchBar" ]
        [ h2 [ class "subtitle is-strong" ] [ text project.name ]

        --div [ class "columns mb-0" ] [
        --  h2 [] [text title]
        --, div [ class "column is-5" ]
        --  [ div [ class "field has-addons" ]
        --      [ div [ class "control is-expanded" ]
        --          [ input
        --              [ class "is-rounded input is-small pr-6"
        --              , type_ "search"
        --              , autocomplete False
        --              , autofocus False
        --              , placeholder T.searchTensions
        --              , value model.pattern
        --              , onInput ChangePattern
        --              , onKeydown SearchKeyDown
        --              ]
        --              []
        --          , span [ class "icon-input-flex-right" ]
        --              [ if model.pattern_init /= "" then
        --                  span [ class "delete is-hidden-mobile", onClick (SubmitTextSearch "") ] []
        --                else
        --                  text ""
        --              , span [ class "vbar has-border-color" ] []
        --              , span [ class "button-light is-w px-1", onClick (SearchKeyDown 13) ]
        --                  [ A.icon "icon-search" ]
        --              ]
        --          ]
        --      ]
        --  ]
        ]


viewProject : ProjectData -> Model -> Html Msg
viewProject data model =
    --viewBoard op viewHeader columns
    text ""
