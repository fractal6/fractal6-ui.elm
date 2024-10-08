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


module Components.TreeMenu exposing (Msg(..), State, getList, getList_, getOrgaData_, init, subscriptions, update, view, viewSelectorTree)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Bulk exposing (UserState(..), getNode, getParentId, hotNodeInsert, hotNodePull, hotNodePush, localGraphFromOrga, uctxFromUser)
import Bulk.Codecs exposing (DocType(..), FractalBaseRoute(..), NodeFocus, getRootids, isRole, nearestCircleid, toLink)
import Bulk.Error exposing (viewGqlErrors)
import Bulk.View exposing (action2icon, counter)
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Dict exposing (Dict)
import Dict.Extra as DE
import Extra exposing (showIf, showMaybe, space_, ternary, unwrap)
import Extra.Events exposing (onClickPD, onClickSafe)
import Fractal.Enum.RoleType as RoleType
import Global exposing (send, sendSleep)
import Html exposing (Html, a, div, i, li, span, text, ul)
import Html.Attributes exposing (attribute, class, classList, id, selected, target)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Html.Lazy as Lazy
import Loading exposing (GqlData, ModalData, RequestResult(..), isSuccess, withMaybeData, withMaybeMapData)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Ports
import Query.QueryNode exposing (queryNodesSub, queryOrgaTree)
import Schemas.TreeMenu exposing (ExpandedLines, PersistentModel, toPersistant)
import Scroll
import Session exposing (Apis, GlobalCmd(..))
import String
import Text as T


type State
    = State Model


type alias Model =
    { user : UserState
    , isActive : Bool
    , isActive2 : Bool
    , isHover : Bool
    , focus : NodeFocus
    , next_focus : Maybe String
    , tree_result : GqlData NodesDict
    , tree : Tree Node
    , hover : Maybe String
    , expanded_lines : ExpandedLines

    -- Common
    , refresh_trial : Int -- use to refresh user token
    , modal_confirm : ModalConfirm Msg
    , baseUri : FractalBaseRoute
    , uriQuery : Maybe String
    }


type Tree n
    = Tree
        { node : n
        , children : List (Tree n)
        }


prefixId : String -> String
prefixId did =
    "treeMenu_" ++ did


initModel : FractalBaseRoute -> Maybe String -> NodeFocus -> UserState -> Maybe PersistentModel -> Maybe NodesDict -> Model
initModel baseUri uriQuery focus user persistent tree =
    let
        m =
            persistent
                |> withDefault
                    { isActive = False
                    , hover = Nothing
                    , expanded_lines = Dict.empty
                    }

        expanded_lines =
            if isRole focus.nameid then
                let
                    nid =
                        nearestCircleid focus.nameid |> (++) "-/§roles§/"
                in
                Dict.insert nid False m.expanded_lines

            else
                m.expanded_lines
    in
    { user = user
    , isActive = m.isActive
    , isActive2 = m.isActive
    , isHover = False
    , focus = focus
    , next_focus = Nothing
    , tree_result =
        case tree of
            Just o ->
                Success o

            Nothing ->
                case user of
                    LoggedIn _ ->
                        LoadingSlowly

                    LoggedOut ->
                        NotAsked
    , tree = Tree { node = initNode, children = [] }
    , hover = m.hover
    , expanded_lines = expanded_lines

    -- Common
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg
    , baseUri = baseUri
    , uriQuery = uriQuery
    }
        |> setTree


init : FractalBaseRoute -> Maybe String -> NodeFocus -> UserState -> Maybe PersistentModel -> Maybe NodesDict -> State
init baseUri uriQuery focus user persistent data =
    initModel baseUri uriQuery focus user persistent data |> State


setTree : Model -> Model
setTree model =
    case model.tree_result of
        Success orga ->
            { model | tree = buildTree_ Nothing orga }

        _ ->
            model


buildTree_ : Maybe String -> NodesDict -> Tree Node
buildTree_ rid_m orga =
    case rid_m of
        Just rid ->
            case Dict.get rid orga of
                Just node ->
                    Tree { node = node, children = buildChildren_ rid orga }

                Nothing ->
                    Tree { node = initNode, children = [] }

        Nothing ->
            -- Root
            case
                DE.find (\k v -> v.parent == Nothing) orga
            of
                Just ( rid, root ) ->
                    Tree
                        { node = root
                        , children = buildChildren_ rid orga
                        }

                Nothing ->
                    Tree { node = initNode, children = [] }


buildChildren_ : String -> NodesDict -> List (Tree Node)
buildChildren_ rid orga =
    orga
        |> Dict.filter
            (\k v ->
                (v.parent |> Maybe.map .nameid) == Just rid
            )
        |> Dict.keys
        |> List.map (\nid -> buildTree_ (Just nid) orga)



-- Global methods


getOrgaData_ : State -> GqlData NodesDict
getOrgaData_ (State model) =
    model.tree_result


getList : String -> GqlData NodesDict -> List String
getList nameid tree_result =
    -- Get the ordered list of nodes
    -- starting from (or below to) the given nameid
    case tree_result of
        Success data ->
            next_ (Just nameid) (buildTree_ Nothing data)

        _ ->
            []


getList_ : String -> State -> List String
getList_ nameid (State model) =
    if isSuccess model.tree_result then
        next_ (Just nameid) model.tree

    else
        []


next_ : Maybe String -> Tree Node -> List String
next_ nameid_m (Tree { node, children }) =
    let
        dive =
            if nameid_m == Nothing || nameid_m == Just node.nameid then
                Nothing

            else
                nameid_m
    in
    ternary (dive == Nothing) [ node.nameid ] []
        ++ List.concatMap
            (\(Tree c) ->
                case children of
                    [] ->
                        []

                    _ ->
                        next_ dive (Tree c)
            )
            children



--- State Controls


reset : Model -> Model
reset model =
    initModel model.baseUri Nothing model.focus model.user (Just (toPersistant model)) (withMaybeData model.tree_result)


setDataResult : GqlData NodesDict -> Model -> Model
setDataResult result model =
    { model | tree_result = result }



-- utils
-- ...
-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = -- Data
      OnLoad
    | OnReload UserCtx
    | OnRequireData
    | OnDataAck (GqlData NodesDict)
    | OnSetTree NodesDict
    | OnToggle
    | OnToggleHover Bool
    | SetIsActive2 Bool
    | OnNodeHover (Maybe String)
    | OnToggleDropdowLine String
      --
    | OnUpdateFocus NodeFocus
      -- Tree Data Edit
    | FetchNewNode String Bool
    | NewNodesAck (GqlData (List Node))
    | AddNodes (List Node)
    | UpdateNode String (Node -> Node)
    | DelNodes (List String)
    | MoveNode String String String
      -- Confirm Modal
    | DoModalConfirmOpen Msg TextMessage
    | DoModalConfirmClose ModalData
    | DoModalConfirmSend
      -- Common
    | NoMsg
    | LogErr String
    | Navigate String
    | NavigateNode Node
    | Do (List GlobalCmd)
    | ScrollToElement String


type alias Out =
    { cmds : List (Cmd Msg)
    , gcmds : List GlobalCmd
    , result : Maybe ( Bool, Bool ) -- define what data is to be returned
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


update_ apis message model =
    case message of
        -- Data
        OnLoad ->
            if model.focus.nameid == "" then
                -- Happens for exemple when a tension page is visited without the nameid name (which is optional) in the url
                ( model, noOut )

            else if model.isActive2 && (not (isSuccess model.tree_result) || (withMaybeMapData (\d -> Dict.member model.focus.rootnameid d) model.tree_result == Just False)) then
                ( setDataResult LoadingSlowly model
                  -- openTreeMenu is needed here, because .has-tree-orga is lost on #helperBar and #mainPane when navigating from non orgs pages.
                , out0 [ queryOrgaTree apis model.focus.rootnameid OnDataAck, ternary model.isHover Cmd.none Ports.openTreeMenu ]
                )

            else
                -- openTreeMenu is needed here, because .has-tree-orga is lost on #helperBar and #mainPane when navigating from non orgs pages.
                ( model
                , out0 [ sendSleep (ScrollToElement model.focus.nameid) 333, ternary (model.isActive2 && not model.isHover) Ports.openTreeMenu Cmd.none ]
                )

        OnReload uctx ->
            if not (isSuccess model.tree_result) || List.length (getRootids uctx.roles) /= List.length (getRootids (uctxFromUser model.user).roles) then
                ( { model | tree_result = LoadingSlowly, user = LoggedIn uctx }, out0 [ send OnLoad ] )

            else
                ( model, noOut )

        OnRequireData ->
            if not (isSuccess model.tree_result) then
                ( setDataResult LoadingSlowly model
                , out0 [ queryOrgaTree apis model.focus.rootnameid OnDataAck ]
                )

            else
                ( model, Out [] [] (Just ( True, False )) )

        OnDataAck result ->
            let
                data =
                    setDataResult result model
            in
            case parseErr result data.refresh_trial of
                Authenticate ->
                    ( setDataResult NotAsked model
                    , out0 [ Ports.raiseAuthModal (uctxFromUser model.user) ]
                    )

                RefreshToken i ->
                    ( { data | refresh_trial = i }, out2 [ sendSleep OnLoad 500 ] [ DoUpdateToken ] )

                OkAuth d ->
                    ( setTree data, Out [ send (ScrollToElement model.focus.nameid) ] [ DoUpdateTree (Just d) ] (Just ( True, True )) )

                _ ->
                    ( data, noOut )

        OnSetTree data ->
            ( setDataResult (Success data) model |> setTree, ternary model.isActive (out0 [ send (ScrollToElement model.focus.nameid), Ports.openTreeMenu ]) noOut )

        OnToggle ->
            if model.isActive then
                ( { model | isActive = False }
                , out0 [ Ports.saveMenuTree (toPersistant { model | isActive = False }), Ports.closeTreeMenu, sendSleep (SetIsActive2 False) 500 ]
                )

            else
                ( { model | isActive2 = True }
                , out0 [ Ports.saveMenuTree (toPersistant { model | isActive = True }), send OnLoad, sendSleep (SetIsActive2 True) 10 ]
                )

        OnToggleHover v ->
            if model.isHover then
                ( { model | isHover = False }, out0 [ sendSleep (SetIsActive2 False) 500 ] )

            else
                ( { model | isActive2 = True, isHover = True }, out0 [ send OnLoad ] )

        SetIsActive2 v ->
            -- Prevent elm from computing the VDOM
            if v then
                ( { model | isActive = model.isActive2 }, out0 [ Ports.openTreeMenu ] )

            else
                ( { model | isActive2 = model.isActive }, noOut )

        OnNodeHover v ->
            let
                newModel =
                    { model | hover = v }
            in
            ( newModel, out0 [ Ports.saveMenuTree (toPersistant newModel) ] )

        OnToggleDropdowLine nid ->
            let
                newModel =
                    if Dict.member nid model.expanded_lines then
                        { model | expanded_lines = Dict.remove nid model.expanded_lines }

                    else
                        { model | expanded_lines = Dict.insert nid False model.expanded_lines }
            in
            ( newModel, out0 [ Ports.saveMenuTree (toPersistant newModel) ] )

        OnUpdateFocus focus ->
            if isSuccess model.tree_result && model.focus.rootnameid == focus.rootnameid then
                ( { model | focus = focus }, noOut )

            else
                let
                    expanded_lines =
                        if isRole model.focus.nameid then
                            let
                                nid =
                                    nearestCircleid model.focus.nameid |> (++) "-/§roles§/"
                            in
                            Dict.insert nid False model.expanded_lines

                        else
                            model.expanded_lines
                in
                ( { model | focus = focus, expanded_lines = expanded_lines }, out0 [ send OnLoad ] )

        -- Data Tree Edit
        FetchNewNode nameid focus ->
            ( if focus then
                { model | next_focus = Just nameid }

              else
                model
            , out0
                [ queryNodesSub apis nameid NewNodesAck ]
            )

        NewNodesAck result ->
            case result of
                Success nodes ->
                    ( model, out0 [ send (AddNodes nodes) ] )

                _ ->
                    ( model, noOut )

        AddNodes nodes ->
            let
                data =
                    hotNodePush nodes model.tree_result
            in
            ( { model | tree_result = Success data, next_focus = Nothing } |> setTree
            , out2
                [--Ports.addQuickSerchNodes nodes
                 --List.map (\n -> n.first_link) nodes |> List.filterMap identity |> Ports.addQuickSearchUsers
                ]
                ([ DoUpdateToken, DoUpdateTree (Just data) ]
                    ++ (model.next_focus
                            |> Maybe.map (\nid -> [ DoFocus nid ])
                            |> withDefault []
                       )
                )
            )

        UpdateNode nameid fun ->
            let
                node_m =
                    getNode nameid model.tree_result |> Maybe.map fun
            in
            case node_m of
                Just n ->
                    let
                        data =
                            hotNodeInsert n model.tree_result
                    in
                    ( { model | tree_result = Success data } |> setTree
                    , out1 [ DoUpdateToken, DoUpdateTree (Just data) ]
                    )

                Nothing ->
                    -- For exemple, when Guest leave an organisation
                    ( model, out1 [ DoUpdateToken ] )

        DelNodes nameids ->
            let
                ( data, _ ) =
                    hotNodePull nameids model.tree_result
            in
            ( { model | tree_result = Success data } |> setTree
              --, Cmd.batch [ Ports.addQuickSearchNodes nodes, nodes |> List.map (\n -> n.first_link) |> List.filterMap identity |> Ports.addQuickSearchUsers ]
            , out1
                ([ DoUpdateToken, DoUpdateTree (Just data) ]
                    ++ (if List.member model.focus.nameid nameids then
                            let
                                newFocus =
                                    nameids
                                        |> List.head
                                        |> Maybe.map (\nid -> getParentId nid model.tree_result)
                                        |> withDefault Nothing
                                        |> withDefault model.focus.rootnameid
                            in
                            [ DoFocus newFocus ]

                        else
                            []
                       )
                )
            )

        MoveNode nameid_old parentid_new nameid_new ->
            let
                ( data, _ ) =
                    hotNodePull [ nameid_old ] model.tree_result
            in
            ( { model | tree_result = Success data }
              --, Cmd.batch [ Ports.addQuickSearchNodes nodes, nodes |> List.map (\n -> n.first_link) |> List.filterMap identity |> Ports.addQuickSearchUsers ]
            , out0 [ send (FetchNewNode nameid_new False) ]
            )

        -- Confirm Modal
        DoModalConfirmOpen msg mess ->
            ( { model | modal_confirm = ModalConfirm.open msg mess model.modal_confirm }, noOut )

        DoModalConfirmClose _ ->
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }, noOut )

        DoModalConfirmSend ->
            ( { model | modal_confirm = ModalConfirm.close model.modal_confirm }, out0 [ send model.modal_confirm.msg ] )

        -- Common
        NoMsg ->
            ( model, noOut )

        LogErr err ->
            ( model, out0 [ Ports.logErr err ] )

        Navigate link ->
            ( model, out1 [ DoNavigate link ] )

        NavigateNode n ->
            let
                q =
                    model.uriQuery |> Maybe.map (\uq -> "?" ++ uq) |> Maybe.withDefault ""
            in
            ( model
            , out2 [ Ports.send_if_mobile "triggerMenuTreeFromJs" ]
                [ DoUpdatePath (localGraphFromOrga n.nameid model.tree_result)
                , DoNavigate (toLink model.baseUri n.nameid [ getSourceTid n ] ++ q)
                ]
            )

        Do gcmds ->
            ( model, out1 gcmds )

        ScrollToElement nid ->
            ( model, out0 [ Scroll.scrollToSubElement "tree-menu" (prefixId nid) NoMsg ] )


subscriptions : List (Sub Msg)
subscriptions =
    [ Ports.triggerMenuTreeFromJs (always OnToggle)

    --, Ports.uctxPD Ports.loadUserCtxFromJs LogErr OnReload
    , Ports.requireTreeDataFromJs (always OnRequireData)
    , Ports.mcPD Ports.closeModalConfirmFromJs LogErr DoModalConfirmClose
    ]



-- ------------------------------
-- V I E W
-- ------------------------------


type alias Op =
    {}


view : Op -> State -> Html Msg
view op (State model) =
    if model.isActive2 then
        let
            isActive =
                model.isActive || model.isHover
        in
        div
            [ id "tree-menu"
            , class "is-hidden-embed"
            , classList [ ( "off", not isActive ) ]
            , onMouseLeave (ternary model.isHover (OnToggleHover False) NoMsg)
            ]
            [ viewTreeMenu model
            , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
            , div
                [ class "button is-small bottom-button"
                , classList [ ( "is-invisible", not isActive ) ]
                , onClick OnToggle
                ]
                [ if model.isHover then
                    A.icon1 "icon-chevrons-right" T.lockMenu

                  else
                    A.icon1 "icon-chevrons-left" T.close
                ]
            , div [ class "pb-6 is-invisible" ] [ text "nop" ]
            ]

    else
        div [ id "tree-hinter", class "is-hidden-mobile", onMouseEnter (OnToggleHover True) ]
            --[ div [ class "hinter is-hidden-mobile", onClick OnToggle ] [] ]
            [ div [] [] ]


viewTreeMenu : Model -> Html Msg
viewTreeMenu model =
    div [ class "menu", onMouseLeave (OnNodeHover Nothing) ]
        [ case model.tree_result of
            Success data ->
                viewSubTree 0 model.hover model.focus model.tree model.expanded_lines

            LoadingSlowly ->
                ul [ class "menu-list" ]
                    [ li [] [ a [] [ div [ class "ph-line is-block" ] [] ] ]
                    , li [] [ a [] [ div [ class "ph-line is-block" ] [] ] ]
                    , li [] [ a [] [ div [ class "ph-line is-block" ] [] ] ]
                    ]

            Failure err ->
                case err of
                    [] ->
                        text ""

                    f :: _ ->
                        if f |> String.toLower |> String.startsWith "no data returned" then
                            ul [ class "menu-list" ]
                                [ li [] [ a [] [ div [ class "ph-line is-block" ] [] ] ]
                                , li [] [ a [] [ div [ class "ph-line is-block" ] [] ] ]
                                , li [] [ a [] [ div [ class "ph-line is-block" ] [] ] ]
                                ]

                        else
                            viewGqlErrors err

            _ ->
                text ""
        ]


viewSubTree : Int -> Maybe String -> NodeFocus -> Tree Node -> ExpandedLines -> Html Msg
viewSubTree depth hover focus (Tree { node, children }) expanded_lines =
    let
        roles =
            children |> List.filter (\(Tree c) -> List.member c.node.role_type [ Just RoleType.Coordinator, Just RoleType.Peer, Just RoleType.Bot ]) |> List.map (\(Tree c) -> c.node)

        nid =
            node.nameid |> (++) "-/§roles§/"

        isExpanded =
            Dict.member nid expanded_lines
    in
    ul ([ class "menu-list" ] ++ ternary (depth == 0) [ onMouseLeave (OnNodeHover Nothing) ] [])
        [ li []
            (Lazy.lazy3 viewCircleLine hover focus node
                :: List.map
                    (\(Tree c) ->
                        showIf (c.node.role_type == Nothing) <|
                            viewSubTree (depth + 1) hover focus (Tree c) expanded_lines
                    )
                    children
            )
        , ul [ class "menu-list pl-0" ]
            [ div [ class "is-boxed" ] <|
                ([ showIf (List.length roles > 0) <|
                    li [] [ viewRolesLine "roles" hover roles nid expanded_lines ]
                 ]
                    ++ (if isExpanded then
                            List.map (\n -> li [] [ Lazy.lazy3 viewCircleLine hover focus n ]) roles

                        else
                            []
                       )
                )
            ]
            |> showIf (List.length roles > 0)
        ]


viewCircleLine : Maybe String -> NodeFocus -> Node -> Html Msg
viewCircleLine hover focus node =
    a
        [ class "treeMenu"
        , id (prefixId node.nameid)
        , classList [ ( "is-active", focus.nameid == node.nameid ) ]
        , onMouseEnter (OnNodeHover (Just node.nameid))
        , onClickPD (NavigateNode node)
        , target "_blank"
        ]
        [ div [ class "level is-mobile" ]
            [ div [ class "level-left", attribute "style" "width:82%;" ]
                [ A.icon1_sm (action2icon { doc_type = NODE node.type_ }) node.name
                , showMaybe node.first_link
                    (\f -> span [ class "is-username is-size-7" ] [ text (space_ ++ "@" ++ f.username) ])
                , case node.n_open_tensions of
                    0 ->
                        text ""

                    i ->
                        counter i
                , case node.n_open_contracts of
                    0 ->
                        text ""

                    i ->
                        span [ class "is-contract-badge-bg" ] [ counter i ]
                ]
            , if hover == Just node.nameid then
                div [ class "level-right here" ]
                    [ span [ class "tag is-rounded has-border", onClickSafe (Do [ DoCreateTension node.nameid Nothing Nothing ]) ] [ A.icon "icon-plus" ] ]

              else
                text ""
            ]
        ]


viewRolesLine : String -> Maybe String -> List Node -> String -> ExpandedLines -> Html Msg
viewRolesLine type_txt hover roles nid expanded_lines =
    let
        isExpanded =
            Dict.member nid expanded_lines
    in
    a
        [ class "treeMenu pt-0 pr-1"
        , id nid
        , onMouseEnter (OnNodeHover (Just nid))
        , onClickPD (OnToggleDropdowLine nid)
        , target "_blank"
        ]
        [ div [ class "level is-mobile" ]
            [ div [ class "level-left", attribute "style" "width:82%;" ]
                [ span [ class "tag has-background-tag has-text-text" ]
                    [ text "+", text (String.fromInt (List.length roles)), text (" " ++ type_txt) ]
                , case List.sum <| List.map .n_open_tensions roles of
                    0 ->
                        text ""

                    i ->
                        showIf (not isExpanded) (counter i)
                , case List.sum <| List.map .n_open_contracts roles of
                    0 ->
                        text ""

                    i ->
                        showIf (not isExpanded)
                            (span [ class "is-contract-badge-bg" ] [ counter i ])
                ]
            , if isExpanded then
                A.icon "icon-chevron-up"

              else if hover == Just nid then
                A.icon "icon-chevron-down"

              else
                text ""
            ]
        ]



--
-- External
--


viewSelectorTree : (Node -> msg) -> (String -> msg) -> List String -> ExpandedLines -> GqlData NodesDict -> Html msg
viewSelectorTree onTargetClick onDropdownClick selected expanded_lines odata =
    case odata of
        Success data ->
            let
                tree =
                    buildTree_ Nothing data
            in
            div [ id "tree-selector", class "menu" ] [ viewSubTree2 onTargetClick onDropdownClick 0 selected tree expanded_lines ]

        _ ->
            div [ class "spinner" ] []


viewSubTree2 : (Node -> msg) -> (String -> msg) -> Int -> List String -> Tree Node -> ExpandedLines -> Html msg
viewSubTree2 onTargetClick onDropdownClick depth selected (Tree { node, children }) expanded_lines =
    let
        roles =
            children |> List.filter (\(Tree c) -> List.member c.node.role_type [ Just RoleType.Coordinator, Just RoleType.Peer, Just RoleType.Bot ]) |> List.map (\(Tree c) -> c.node)

        nid =
            node.nameid |> (++) "-/§roles§/"

        isExpanded =
            Dict.member nid expanded_lines
    in
    ul ([ class "menu-list" ] ++ ternary (depth == 0) [] [])
        [ li []
            (Lazy.lazy3 viewNodeLine onTargetClick selected node
                :: List.map
                    (\(Tree c) ->
                        showIf (c.node.role_type == Nothing) <|
                            viewSubTree2 onTargetClick onDropdownClick (depth + 1) selected (Tree c) expanded_lines
                    )
                    children
            )
        , ul [ class "menu-list pl-0" ]
            [ div [ class "is-boxed" ] <|
                ([ showIf (List.length roles > 0) <|
                    li [] [ viewRolesLine2 onDropdownClick "roles" (Just nid) roles nid expanded_lines ]
                 ]
                    ++ (if isExpanded then
                            List.map (\n -> li [] [ Lazy.lazy3 viewNodeLine onTargetClick selected n ]) roles

                        else
                            []
                       )
                )
            ]
            |> showIf (List.length roles > 0)
        ]


viewNodeLine : (Node -> msg) -> List String -> Node -> Html msg
viewNodeLine onTargetClick selected node =
    let
        isActive =
            List.member node.nameid selected
    in
    a
        ([ class "treeMenu"
         , id (prefixId node.nameid)
         , classList [ ( "is-active", isActive ) ]
         , target "_blank"
         ]
            ++ (if not isActive then
                    [ onClick (onTargetClick node) ]

                else
                    []
               )
        )
        [ A.icon1_sm (action2icon { doc_type = NODE node.type_ }) node.name
        , showMaybe node.first_link
            (\f -> span [ class "is-username is-size-7" ] [ text (space_ ++ "@" ++ f.username) ])
        ]


{-| This is a copy (just a few changes) of viewRoleLines just with msg instead of MSG
@CODEFACTOR: how to factorize those functions in elm ?
-}
viewRolesLine2 : (String -> msg) -> String -> Maybe String -> List Node -> String -> ExpandedLines -> Html msg
viewRolesLine2 onDropdownClick type_txt hover roles nid expanded_lines =
    let
        isExpanded =
            Dict.member nid expanded_lines
    in
    a
        [ class "treeMenu pt-0 pr-1"
        , id nid
        , onClickPD (onDropdownClick nid)
        , target "_blank"
        ]
        [ div [ class "level is-mobile" ]
            [ div [ class "level-left", attribute "style" "width:82%;" ]
                [ span [ class "tag is-smaller2 has-background-tag has-text-text" ]
                    [ text "+", text (String.fromInt (List.length roles)), text (" " ++ type_txt) ]
                ]
            , if isExpanded then
                A.icon "icon-chevron-up"

              else if hover == Just nid then
                A.icon "icon-chevron-down"

              else
                text ""
            ]
        ]
