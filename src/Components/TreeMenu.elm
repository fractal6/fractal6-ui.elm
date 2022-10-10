module Components.TreeMenu exposing (Msg(..), State, getList_, getOrgaData_, init, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Dict
import Dict.Extra as DE
import Extra exposing (ternary, textH, upH)
import Extra.Events exposing (onClickPD, onClickPD2)
import Form exposing (isPostEmpty)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Generated.Route as Route exposing (Route, toHref)
import Global exposing (send, sendNow, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, label, li, nav, option, p, pre, section, select, span, text, textarea, ul)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, for, href, id, list, name, placeholder, required, rows, selected, target, title, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter, onMouseLeave)
import Html.Lazy as Lazy
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Loading exposing (GqlData, ModalData, RequestResult(..), isFailure, isSuccess, viewGqlErrors, withMaybeData, withMaybeDataMap)
import Maybe exposing (withDefault)
import ModelCommon exposing (UserState(..), getNode, getParentId, hotNodeInsert, hotNodePull, hotNodePush, uctxFromUser)
import ModelCommon.Codecs exposing (DocType(..), FractalBaseRoute(..), NodeFocus, getRootids, uriFromNameid)
import ModelCommon.View exposing (action2icon, viewOrga0)
import ModelSchema exposing (..)
import Ports
import Query.QueryNode exposing (queryNodesSub, queryOrgaTree)
import Session exposing (Apis, GlobalCmd(..))
import String
import Text as T
import Time


type State
    = State Model


type alias Model =
    { user : UserState
    , isActive : Bool
    , isActive2 : Bool
    , focus : NodeFocus
    , tree_result : GqlData NodesDict
    , tree : Tree Node
    , hover : Maybe String
    , next_focus : Maybe String

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


initModel : FractalBaseRoute -> Maybe String -> NodeFocus -> Maybe Bool -> Maybe NodesDict -> UserState -> Model
initModel baseUri uriQuery focus isActive tree user =
    { user = user
    , isActive = withDefault False isActive
    , isActive2 = withDefault False isActive
    , focus = focus
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
    , hover = Nothing
    , next_focus = Nothing

    -- Common
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg
    , baseUri = baseUri
    , uriQuery = uriQuery
    }
        |> setTree


init : FractalBaseRoute -> Maybe String -> NodeFocus -> Maybe Bool -> Maybe NodesDict -> UserState -> State
init baseUri uriQuery focus isActive tree user =
    initModel baseUri uriQuery focus isActive tree user |> State


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


getList_ : String -> State -> List String
getList_ nameid (State model) =
    -- Get the ordered list of nodes
    -- starting from the given nameid
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
        ++ (List.map
                (\(Tree c) ->
                    case children of
                        [] ->
                            []

                        _ ->
                            next_ dive (Tree c)
                )
                children
                |> List.concat
           )



--- State Controls


reset : Model -> Model
reset model =
    initModel model.baseUri Nothing model.focus (Just model.isActive) (withMaybeData model.tree_result) model.user


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
    | SetIsActive2 Bool
    | OnOrgHover (Maybe String)
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

            else if model.isActive2 && (not (isSuccess model.tree_result) || (withMaybeDataMap (\d -> Dict.member model.focus.rootnameid d) model.tree_result == Just False)) then
                ( setDataResult LoadingSlowly model
                  -- openTreeMenu is needed here, because .has-tree-orga is lost on #helperBar and #mainPane
                  -- when navigating from non orgs pages.
                , out0 [ queryOrgaTree apis model.focus.rootnameid OnDataAck, Ports.openTreeMenu ]
                )

            else
                ( model, noOut )

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
                ( model, Out [] [] (Just ( True, True )) )

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
                    ( setTree data, Out [] [ DoUpdateTree (Just d) ] (Just ( True, True )) )

                _ ->
                    ( data, noOut )

        OnSetTree data ->
            ( setDataResult (Success data) model |> setTree, ternary model.isActive (out0 [ Ports.openTreeMenu ]) noOut )

        OnToggle ->
            if model.isActive then
                ( { model | isActive = False }
                , out0 [ Ports.saveMenuTree False, Ports.closeTreeMenu, sendSleep (SetIsActive2 False) 500 ]
                )

            else
                ( { model | isActive2 = True }
                , out0 [ Ports.saveMenuTree True, send OnLoad, sendSleep (SetIsActive2 True) 10 ]
                )

        SetIsActive2 v ->
            -- Prevent elm from computing the VDOM
            if v then
                ( { model | isActive = model.isActive2 }, out0 [ Ports.openTreeMenu ] )

            else
                ( { model | isActive2 = model.isActive }, noOut )

        OnOrgHover v ->
            ( { model | hover = v }, noOut )

        OnUpdateFocus focus ->
            if isSuccess model.tree_result then
                ( { model | focus = focus }, noOut )

            else
                ( { model | focus = focus }, out0 [ send OnLoad ] )

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
            ( model, out1 [ DoNavigate (uriFromNameid model.baseUri n.nameid [ getSourceTid n ] ++ q) ] )

        Do gcmds ->
            ( model, out1 gcmds )


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
        div
            [ id "tree-menu"
            , class "is-hidden-touch"
            , classList [ ( "off", not model.isActive ) ]
            ]
            [ viewTreeMenu model
            , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
            , div
                [ class "button is-small bottom-button"
                , classList [ ( "is-invisible", not model.isActive ) ]
                , onClick OnToggle
                ]
                [ A.icon1 "icon-chevrons-left" T.collapse ]
            , div [ class "pb-6 is-invisible" ] [ text "nop" ]
            ]

    else
        div [ id "tree-hinter", class "is-hidden-touch", onClick OnToggle ]
            [ div [ class "hinter" ] [] ]


viewTreeMenu : Model -> Html Msg
viewTreeMenu model =
    div [ class "menu", onMouseLeave (OnOrgHover Nothing) ]
        [ case model.tree_result of
            Success data ->
                Lazy.lazy4 viewSubTree 0 model.hover model.focus model.tree

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


viewSubTree : Int -> Maybe String -> NodeFocus -> Tree Node -> Html Msg
viewSubTree depth hover focus (Tree { node, children }) =
    ul ([ class "menu-list" ] ++ ternary (depth == 0) [ onMouseLeave (OnOrgHover Nothing) ] [])
        [ li []
            ([ Lazy.lazy3 viewCircleLine hover focus node ]
                ++ List.map
                    (\(Tree c) ->
                        if c.node.role_type == Nothing then
                            viewSubTree (depth + 1) hover focus (Tree c)

                        else
                            text ""
                    )
                    children
            )
        , ul [ class "menu-list pl-0" ]
            [ li []
                -- @TODO : add tag (3 roles) that is clickable and will open the roles list.
                [ viewRoleLine focus (List.filter (\(Tree c) -> List.member c.node.role_type [ Just RoleType.Peer, Just RoleType.Coordinator ]) children |> List.map (\(Tree c) -> c.node))
                ]
            ]
        ]


viewCircleLine : Maybe String -> NodeFocus -> Node -> Html Msg
viewCircleLine hover focus node =
    a
        [ class "treeMenu"
        , classList [ ( "is-active", focus.nameid == node.nameid ) ]
        , onMouseEnter (OnOrgHover (Just node.nameid))
        , onClickPD (NavigateNode node)
        , target "_blank"
        ]
        [ div [ class "level is-mobile" ]
            [ div [ class "level-left", attribute "style" "width:82%;" ]
                [ case node.role_type of
                    Just RoleType.Bot ->
                        A.icon1 "icon-radio" node.name

                    Just _ ->
                        A.icon1 (action2icon { doc_type = NODE node.type_ }) node.name

                    Nothing ->
                        text node.name
                ]
            , if hover == Just node.nameid then
                div [ class "level-right here" ]
                    [ span [ class "tag is-rounded has-border", onClickPD2 (Do [ DoCreateTension node.nameid ]) ] [ A.icon "icon-plus" ] ]

              else
                text ""
            ]
        ]


viewRoleLine : NodeFocus -> List Node -> Html Msg
viewRoleLine focus roles =
    if List.length roles > 0 then
        div
            [ class "treeMenu"
            , classList [ ( "is-active", List.member focus.nameid (List.map .nameid roles) ) ]

            --, onMouseEnter (OnOrgHover (Just node.nameid))
            --, onClickPD (NavigateNode node)
            --, target "_blank"
            ]
            [ div [ class "level is-mobile" ]
                [ div [ class "level-left", attribute "style" "width:82%;" ]
                    [ span [ class "tag is-rounded is-small has-background-border-light" ]
                        [ text "+", text (String.fromInt (List.length roles)), text " roles" ]
                    ]
                ]
            ]

    else
        text ""
