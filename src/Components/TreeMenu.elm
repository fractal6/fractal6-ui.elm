module Components.TreeMenu exposing (Msg(..), State, getOrgaData_, init, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Components.Loading as Loading exposing (GqlData, ModalData, RequestResult(..), isFailure, isSuccess, viewGqlErrors, withMaybeData)
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Dict
import Dict.Extra as DE
import Extra exposing (ternary)
import Extra.Events exposing (onClickPD)
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
import Maybe exposing (withDefault)
import ModelCommon exposing (UserState(..), uctxFromUser)
import ModelCommon.Codecs exposing (DocType(..), FractalBaseRoute(..), NodeFocus, getRootids, uriFromNameid)
import ModelCommon.View exposing (action2icon, viewOrga0)
import ModelSchema exposing (..)
import Ports
import Query.QueryNode exposing (queryOrgaTree)
import Session exposing (Apis, GlobalCmd(..))
import Text as T exposing (textH, textT, upH)
import Time


type State
    = State Model


type alias Model =
    { user : UserState
    , isActive : Bool
    , focus : NodeFocus
    , tree_result : GqlData NodesDict
    , tree : Tree Node
    , hover : Maybe String

    -- Common
    , refresh_trial : Int -- use to refresh user token
    , modal_confirm : ModalConfirm Msg
    }


type Tree n
    = Tree
        { node : n
        , children : List (Tree n)
        }


initModel : NodeFocus -> Maybe Bool -> Maybe NodesDict -> UserState -> Model
initModel focus isActive tree user =
    { user = user
    , isActive = withDefault False isActive
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

    -- Common
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg
    }
        |> setTree


init : NodeFocus -> Maybe Bool -> Maybe NodesDict -> UserState -> State
init focus isActive tree user =
    initModel focus isActive tree user |> State


setTree : Model -> Model
setTree model =
    case model.tree_result of
        Success orga ->
            { model | tree = buildTree Nothing orga }

        _ ->
            model


buildTree : Maybe String -> NodesDict -> Tree Node
buildTree rid_m orga =
    case rid_m of
        Just rid ->
            case Dict.get rid orga of
                Just node ->
                    Tree { node = node, children = buildChildren rid orga }

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
                        , children = buildChildren rid orga
                        }

                Nothing ->
                    Tree { node = initNode, children = [] }


buildChildren : String -> NodesDict -> List (Tree Node)
buildChildren rid orga =
    orga
        |> Dict.filter
            (\k v ->
                (v.parent |> Maybe.map .nameid) == Just rid
            )
        |> Dict.keys
        |> List.map (\nid -> buildTree (Just nid) orga)



-- Global methods


getOrgaData_ : State -> GqlData NodesDict
getOrgaData_ (State model) =
    model.tree_result



--- State Controls


reset : Model -> Model
reset model =
    initModel model.focus (Just model.isActive) (withMaybeData model.tree_result) model.user


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
    | OnOrgHover (Maybe String)
      -- Confirm Modal
    | DoModalConfirmOpen Msg TextMessage
    | DoModalConfirmClose ModalData
    | DoModalConfirmSend
      -- Common
    | NoMsg
    | LogErr String
    | Navigate String
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
            if model.isActive && not (isSuccess model.tree_result) then
                ( setDataResult LoadingSlowly model
                , out0 [ queryOrgaTree apis model.focus.rootnameid OnDataAck ]
                )

            else
                ( setTree model, noOut )

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
                ( model, noOut )

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
                    ( data |> setTree, Out [] [ DoUpdateTree (Just d) ] (Just ( True, True )) )

                _ ->
                    ( data, noOut )

        OnSetTree data ->
            ( setDataResult (Success data) model |> setTree, noOut )

        OnToggle ->
            ( { model | isActive = not model.isActive }, out0 [ Ports.saveMenuTree (not model.isActive), send OnLoad, Ports.toggleTreeMenu ] )

        OnOrgHover v ->
            ( { model | hover = v }, noOut )

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

        Do gcmds ->
            ( model, out1 gcmds )


subscriptions : List (Sub Msg)
subscriptions =
    [ Ports.triggerMenuTreeFromJs (always OnToggle)

    --, Ports.uctxPD Ports.loadUserCtxFromJs LogErr OnReload
    , Ports.mcPD Ports.closeModalConfirmFromJs LogErr DoModalConfirmClose
    ]



-- ------------------------------
-- V I E W
-- ------------------------------


type alias Op =
    { baseUri : FractalBaseRoute
    , uriQuery : Maybe String
    }


view : Op -> State -> Html Msg
view op (State model) =
    div
        [ id "tree-menu"
        , class "is-hidden-touch"
        , classList [ ( "off", not model.isActive ) ]
        ]
        [ Lazy.lazy5 viewOrgas op model.hover model.focus model.tree model.tree_result
        , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
        , div
            [ class "button is-small bottom-button"
            , classList [ ( "is-invisible", not model.isActive ) ]
            , onClick OnToggle
            ]
            [ A.icon1 "icon-chevrons-left" "Collapse" ]
        , div [ class "pb-6 is-invisible" ] [ text "nop" ]
        ]


viewOrgas : Op -> Maybe String -> NodeFocus -> Tree Node -> GqlData NodesDict -> Html Msg
viewOrgas op hover focus (Tree b) tree_result =
    let
        q =
            op.uriQuery |> Maybe.map (\uq -> "?" ++ uq) |> Maybe.withDefault ""
    in
    div [ class "menu" ]
        [ case tree_result of
            Success data ->
                viewSubTree op.baseUri q hover focus b.node b.children

            LoadingSlowly ->
                ul [ class "menu-list" ]
                    [ li [] [ a [] [ div [ class "ph-line is-block" ] [] ] ]
                    , li [] [ a [] [ div [ class "ph-line is-block" ] [] ] ]
                    , li [] [ a [] [ div [ class "ph-line is-block" ] [] ] ]
                    ]

            Failure err ->
                --viewGqlErrors err
                span [ class "m-3 has-text-danger" ] [ text "Error" ]

            _ ->
                text ""
        ]


viewSubTree : FractalBaseRoute -> String -> Maybe String -> NodeFocus -> Node -> List (Tree Node) -> Html Msg
viewSubTree baseUri q hover focus x children =
    ul [ class "menu-list" ]
        [ li []
            ([ a
                [ class "treeMenu"
                , classList [ ( "is-active", focus.nameid == x.nameid ) ]
                , onMouseEnter (OnOrgHover (Just x.nameid))
                , onMouseLeave (OnOrgHover Nothing)
                , onClickPD NoMsg
                , target "_blank"
                ]
                [ span [ onClick (Navigate (uriFromNameid baseUri x.nameid ++ q)) ]
                    [ case x.role_type of
                        Just RoleType.Bot ->
                            A.icon1 "icon-radio" x.name

                        Just _ ->
                            A.icon1 (action2icon { doc_type = NODE x.type_ }) x.name

                        Nothing ->
                            if List.length children > 0 then
                                --A.icon1 (action2icon { doc_type = NODE x.type_ }) x.name
                                text x.name

                            else
                                text x.name
                    ]
                , if hover == Just x.nameid then
                    div [ class "here tag is-rounded has-border is-pulled-right", onClick (Do [ DoCreateTension x.nameid ]) ] [ A.icon "icon-plus" ]

                  else
                    text ""
                ]
             ]
                ++ (children
                        |> List.map
                            (\(Tree c) ->
                                if List.member c.node.role_type [ Just RoleType.Owner, Just RoleType.Pending, Just RoleType.Peer, Just RoleType.Coordinator ] then
                                    -- @TODO : add tag (3 roles) that is clickable and will open the roles list.
                                    text ""

                                else
                                    viewSubTree baseUri q hover focus c.node c.children
                            )
                   )
            )
        ]
