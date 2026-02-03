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


module Components.OrgaMenu exposing (Msg(..), State, init, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Browser.Dom as Dom
import Bulk exposing (UserState(..), uctxFromUser)
import Bulk.Codecs exposing (NodeFocus, getRootids)
import Bulk.View exposing (viewOrga0)
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Generated.Route as Route exposing (toHref)
import Global exposing (send, sendSleep)
import Html exposing (Html, a, div, i, span, text)
import Html.Attributes exposing (class, classList, href, id, style, title)
import Html.Events exposing (onMouseEnter, onMouseLeave)
import Html.Lazy as Lazy
import Loading exposing (GqlData, ModalData, RequestResult(..), isSuccess, withMaybeData)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Ports
import Query.QueryNode exposing (queryOrgaNode)
import Session exposing (Apis, GlobalCmd(..), SessionCommon)
import Task


type State
    = State Model


type alias TooltipInfo =
    { name : String
    , top : Float
    }


type alias Model =
    { isActive : Bool
    , isActive2 : Bool
    , focus : NodeFocus
    , orgs_result : GqlData (List OrgaNode)
    , tooltip : Maybe TooltipInfo

    -- Common
    , session : SessionCommon
    , refresh_trial : Int -- use to refresh user token
    , modal_confirm : ModalConfirm Msg
    }


initModel : NodeFocus -> Maybe Bool -> Maybe (List OrgaNode) -> SessionCommon -> Model
initModel focus isActive orgs session =
    { isActive = withDefault False isActive
    , isActive2 = withDefault False isActive
    , focus = focus
    , orgs_result =
        case orgs of
            Just o ->
                Success o

            Nothing ->
                case session.user of
                    LoggedIn _ ->
                        LoadingSlowly

                    LoggedOut ->
                        NotAsked
    , tooltip = Nothing

    -- Common
    , session = session
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg
    }


init : NodeFocus -> Maybe Bool -> Maybe (List OrgaNode) -> SessionCommon -> State
init focus isActive orgs session =
    initModel focus isActive orgs session |> State



-- State Controls


clearTooltip : Model -> Model
clearTooltip model =
    { model | tooltip = Nothing }


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
    | OnOrgHover (Maybe String) String -- nameid and orgName
    | OnOrgHoverResult (Result Dom.Error TooltipInfo)
      --
    | OnUpdateFocus NodeFocus
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
            case model.session.user of
                LoggedIn uctx ->
                    case getRootids uctx.roles of
                        [] ->
                            ( setDataResult (Success []) model, noOut )

                        rootids ->
                            if model.focus.nameid == "" then
                                -- Happens for exemple when a tension page is visited without the nameid name (which is optional) in the url
                                ( model, noOut )

                            else if model.isActive2 && not (isSuccess model.orgs_result) then
                                ( setDataResult LoadingSlowly model
                                , out0 [ queryOrgaNode apis rootids OnDataAck ]
                                )

                            else
                                ( model, noOut )

                LoggedOut ->
                    ( model, noOut )

        OnReload uctx ->
            if not (isSuccess model.orgs_result) || List.length (getRootids uctx.roles) /= List.length (getRootids (uctxFromUser model.session.user).roles) then
                let
                    session =
                        model.session
                in
                ( { model | orgs_result = LoadingSlowly, session = { session | user = LoggedIn uctx } }, out0 [ send OnLoad ] )

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
                    , out0 [ Ports.raiseAuthModal (uctxFromUser model.session.user) ]
                    )

                RefreshToken i ->
                    ( { data | refresh_trial = i }, out2 [ sendSleep OnLoad 500 ] [ DoUpdateToken ] )

                OkAuth d ->
                    ( data, Out [] [ DoUpdateOrgs (Just d) ] (Just ( True, True )) )

                _ ->
                    ( data, noOut )

        OnToggle ->
            if model.isActive then
                ( { model | isActive = False }
                , out0 [ Ports.saveMenuOrga False, Ports.closeOrgaMenu, sendSleep (SetIsActive2 False) 500 ]
                )

            else
                ( { model | isActive2 = True }
                , out0 [ Ports.saveMenuOrga True, send OnLoad, sendSleep (SetIsActive2 True) 10 ]
                )

        SetIsActive2 v ->
            -- Prevent elm from computing the VDOM
            if v then
                ( { model | isActive = model.isActive2 }, out0 [ Ports.openOrgaMenu ] )

            else
                ( { model | isActive2 = model.isActive }, noOut )

        OnOrgHover maybeNameid orgName ->
            case maybeNameid of
                Just nameid ->
                    -- Calculate position relative to #orga-menu container (which is fixed)
                    -- This ensures main window scroll doesn't affect tooltip position
                    ( model
                    , out0
                        [ Task.map2
                            (\elementInfo containerInfo ->
                                let
                                    relativeY =
                                        elementInfo.element.y - containerInfo.element.y
                                in
                                { name = orgName
                                , top = relativeY + elementInfo.element.height / 2
                                }
                            )
                            (Dom.getElement ("orga-" ++ nameid))
                            (Dom.getElement "orga-menu")
                            |> Task.attempt OnOrgHoverResult
                        ]
                    )

                Nothing ->
                    ( clearTooltip model, noOut )

        OnOrgHoverResult result ->
            case result of
                Ok info ->
                    ( { model | tooltip = Just info }, noOut )

                Err _ ->
                    ( clearTooltip model, noOut )

        OnUpdateFocus focus ->
            if isSuccess model.orgs_result then
                ( { model | focus = focus }, noOut )

            else
                ( { model | focus = focus }, out0 [ send OnLoad ] )

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
            , onMouseLeave (OnOrgHover Nothing "")
            ]
            [ viewOrgaMenu model.focus model.orgs_result op
            , viewTooltip model.tooltip
            , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
            ]

    else
        text ""


viewTooltip : Maybe TooltipInfo -> Html msg
viewTooltip maybeTooltip =
    case maybeTooltip of
        Just { name, top } ->
            div
                [ class "here box"
                , style "position" "absolute"
                , style "top" (String.fromFloat top ++ "px")
                , style "transform" "translateY(-50%)"
                , style "left" "calc(64px + 0.5rem)"
                , style "z-index" "100"
                , style "white-space" "nowrap"
                , style "padding" "10px"
                ]
                [ text name ]

        Nothing ->
            text ""


viewOrgaMenu : NodeFocus -> GqlData (List OrgaNode) -> Op -> Html Msg
viewOrgaMenu focus orgs_result op =
    div []
        ((case orgs_result of
            Success data ->
                List.map (\x -> Lazy.lazy2 viewOrga focus x) data

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
                        [ class "discrete-link"
                        , href (toHref Route.New_Orga)
                        , title "Create new organizatinon"
                        ]
                        [ A.icon "icon-plus icon-lg" ]
                    ]
               ]
        )


viewOrga : NodeFocus -> OrgaNode -> Html Msg
viewOrga focus x =
    div
        [ id ("orga-" ++ x.nameid)
        , class "orgaMenu"
        , classList [ ( "is-active", focus.rootnameid == x.nameid ) ]
        , onMouseEnter (OnOrgHover (Just x.nameid) x.name)
        ]
        [ viewOrga0 True x.nameid
        ]
