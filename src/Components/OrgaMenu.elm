module Components.OrgaMenu exposing (Msg(..), State, init, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Dict exposing (Dict)
import Extra exposing (ternary, textH, upH)
import Form exposing (isPostEmpty)
import Generated.Route as Route exposing (Route, toHref)
import Global exposing (send, sendNow, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, label, li, nav, option, p, pre, section, select, span, text, textarea, ul)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, for, href, id, list, name, placeholder, required, rows, selected, target, title, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter, onMouseLeave)
import Html.Lazy as Lazy
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Loading exposing (GqlData, ModalData, RequestResult(..), isFailure, isSuccess, viewGqlErrors, withMaybeData)
import Maybe exposing (withDefault)
import ModelCommon exposing (UserState(..), uctxFromUser)
import ModelCommon.Codecs exposing (NodeFocus, getRootids)
import ModelCommon.View exposing (viewOrga0)
import ModelSchema exposing (..)
import Ports
import Query.QueryNode exposing (queryOrgaNode)
import Session exposing (Apis, GlobalCmd(..))
import Text as T
import Time


type State
    = State Model


type alias Model =
    { user : UserState
    , isActive : Bool
    , isActive2 : Bool
    , focus : NodeFocus
    , orgs_result : GqlData (List OrgaNode)
    , hover : Maybe String

    -- Common
    , refresh_trial : Int -- use to refresh user token
    , modal_confirm : ModalConfirm Msg
    }


initModel : NodeFocus -> Maybe Bool -> Maybe (List OrgaNode) -> UserState -> Model
initModel focus isActive orgs user =
    { user = user
    , isActive = withDefault False isActive
    , isActive2 = withDefault False isActive
    , focus = focus
    , orgs_result =
        case orgs of
            Just o ->
                Success o

            Nothing ->
                case user of
                    LoggedIn _ ->
                        LoadingSlowly

                    LoggedOut ->
                        NotAsked
    , hover = Nothing

    -- Common
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg
    }


init : NodeFocus -> Maybe Bool -> Maybe (List OrgaNode) -> UserState -> State
init focus isActive orgs user =
    initModel focus isActive orgs user |> State



-- Global methods
--isOpen_ : State -> Bool
--isOpen_ (State model) =
--    model.isOpen
--- State Controls


reset : Model -> Model
reset model =
    initModel model.focus (Just model.isActive) (withMaybeData model.orgs_result) model.user


setDataResult : GqlData (List OrgaNode) -> Model -> Model
setDataResult result model =
    { model | orgs_result = result }



-- utils
-- ...
-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = -- Data
      OnLoad
    | OnReload UserCtx
    | OnDataAck (GqlData (List OrgaNode))
    | OnToggle
    | SetIsActive2 Bool
    | OnOrgHover (Maybe String)
      -- Confirm Modal
    | DoModalConfirmOpen Msg TextMessage
    | DoModalConfirmClose ModalData
    | DoModalConfirmSend
      -- Common
    | NoMsg
    | LogErr String


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
            case model.user of
                LoggedIn uctx ->
                    case getRootids uctx.roles of
                        [] ->
                            ( setDataResult (Success []) model, noOut )

                        rootids ->
                            if model.isActive2 && not (isSuccess model.orgs_result) then
                                ( setDataResult LoadingSlowly model
                                , out0 [ queryOrgaNode apis rootids OnDataAck ]
                                )

                            else
                                ( model, noOut )

                LoggedOut ->
                    ( model, noOut )

        OnReload uctx ->
            if not (isSuccess model.orgs_result) || List.length (getRootids uctx.roles) /= List.length (getRootids (uctxFromUser model.user).roles) then
                ( { model | orgs_result = LoadingSlowly, user = LoggedIn uctx }, out0 [ send OnLoad ] )

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
                    ( data, Out [] [ DoUpdateOrgs (Just d) ] (Just ( True, True )) )

                _ ->
                    ( data, noOut )

        OnToggle ->
            if model.isActive then
                ( { model | isActive = False }, out0 [ Ports.saveMenuOrga False, Ports.closeOrgaMenu, sendSleep (SetIsActive2 False) 500 ] )

            else
                ( { model | isActive2 = True }, out0 [ Ports.saveMenuOrga True, send OnLoad, sendSleep (SetIsActive2 True) 10 ] )

        SetIsActive2 v ->
            -- Prevent elm from computing the VDOM
            if v then
                ( { model | isActive = model.isActive2 }, out0 [ Ports.openOrgaMenu ] )

            else
                ( { model | isActive2 = model.isActive }, noOut )

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


subscriptions : List (Sub Msg)
subscriptions =
    [ Ports.triggerMenuOrgaFromJs (always OnToggle)
    , Ports.uctxPD Ports.loadUserCtxFromJs LogErr OnReload
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
            [ id "orga-menu"
            , class "is-hidden-touch"
            , classList [ ( "off", not model.isActive ) ]
            , onMouseLeave (OnOrgHover Nothing)
            ]
            [ Lazy.lazy4 viewOrgas model.hover model.focus model.orgs_result op
            , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
            ]

    else
        text ""


viewOrgas : Maybe String -> NodeFocus -> GqlData (List OrgaNode) -> Op -> Html Msg
viewOrgas hover focus orgs_result op =
    div []
        ((case orgs_result of
            Success data ->
                List.map (\x -> Lazy.lazy3 viewOrga hover focus x) data

            LoadingSlowly ->
                [ div [ class "m-3 image circleBase circle1 ph-circle" ] []
                , div [ class "m-3 image circleBase circle1 ph-circle" ] []
                , div [ class "m-3 image circleBase circle1 ph-circle" ] []
                ]

            Failure err ->
                --viewGqlErrors err
                [ span [ class "m-3 has-text-danger" ] [ text "Error" ] ]

            _ ->
                []
         )
            ++ [ div [ class "m-5 pb-6" ]
                    [ a
                        [ class "is-discrete-2"
                        , href (toHref Route.New_Orga)
                        , title "Create new organizatinon"
                        ]
                        [ A.icon "icon-plus icon-lg" ]
                    ]
               ]
        )


viewOrga : Maybe String -> NodeFocus -> OrgaNode -> Html Msg
viewOrga hover focus x =
    div
        [ class "orgaMenu"
        , classList [ ( "is-active", focus.rootnameid == x.nameid ) ]
        , onMouseEnter (OnOrgHover (Just x.nameid))
        ]
        [ if hover == Just x.nameid then
            div [ class "here box" ] [ text x.name ]

          else
            text ""
        , viewOrga0 True x.nameid
        ]
