module Components.UserInput exposing (Msg(..), State, init, subscriptions, update, view)

import Auth exposing (ErrState(..), parseErr)
import Codecs exposing (LookupResult)
import Components.Loading as Loading exposing (GqlData, ModalData, RequestResult(..), loadingSpinRight, viewGqlErrors, withMaybeData)
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Dict exposing (Dict)
import Extra exposing (ternary)
import Extra.Date exposing (diffTime)
import Extra.Events exposing (onClickPD)
import Form exposing (isPostEmpty, isUsersSendable)
import Global exposing (send, sendNow, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, label, li, nav, option, p, pre, section, select, span, text, textarea, ul)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, for, href, id, list, name, placeholder, required, rows, selected, target, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Icon as I
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (UserForm, UserState(..), initUserForm, orgaToUsers, uctxFromUser)
import ModelCommon.View exposing (viewUserFull)
import ModelSchema exposing (..)
import Ports
import Query.QueryUser exposing (queryUser)
import Session exposing (Apis, GlobalCmd(..))
import Text as T exposing (textH, textT, upH)
import Time


type State
    = State Model


type alias Model =
    { user : UserState
    , users_result : GqlData (List User)
    , form : UserForm
    , lookup : List User
    , users_orga : List User -- base user from orga
    , lastPattern : String -- last pattern used to fetch data
    , lastTime : Time.Posix -- last time data were fetch
    , isOpen : Bool -- state of the selectors panel

    -- Common
    , refresh_trial : Int -- use to refresh user token
    , modal_confirm : ModalConfirm Msg
    }


initModel : UserState -> Model
initModel user =
    { user = user
    , users_result = NotAsked
    , form = initUserForm
    , lookup = []
    , users_orga = []
    , lastPattern = ""
    , lastTime = Time.millisToPosix 0
    , isOpen = False

    -- Common
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg
    }


init : UserState -> State
init user =
    initModel user |> State



-- Global methods
--isOpen_ : State -> Bool
--isOpen_ (State model) =
--    model.isOpen
--- State Controls


reset : Model -> Model
reset model =
    initModel model.user


open : Model -> Model
open model =
    { model | isOpen = True }


close : Model -> Model
close model =
    { model | isOpen = False }


clickUser : User -> Model -> Model
clickUser user model =
    let
        form =
            model.form
    in
    { model | form = { form | username = user.username, name = user.name, pattern = "" } }


clickEmail : String -> Model -> Model
clickEmail email model =
    let
        form =
            model.form
    in
    { model | form = { form | email = email, pattern = "" } }


unselect : Model -> Model
unselect model =
    let
        form =
            model.form
    in
    { model | form = initUserForm }


setPattern : String -> Model -> Model
setPattern value model =
    let
        form =
            model.form
    in
    { model | form = { form | pattern = value } }


setDataResult : GqlData (List User) -> Model -> Model
setDataResult result model =
    { model | users_result = result }



-- utils
--
--
--
--
-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = -- Data
      OnLoad (GqlData NodesDict)
    | OnReset
    | OnInput Bool String
    | OnClickUser User
    | OnClickEmail String
    | OnUnselect
    | DoQueryUser
    | OnUsersAck (GqlData (List User))
      -- Lookup
    | ChangeUserLookup (LookupResult User)
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
    , result : Maybe ( Bool, List UserForm ) -- define what data is to be returned
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
        OnLoad orga_data ->
            let
                users_data =
                    withMaybeData orga_data |> withDefault Dict.empty |> orgaToUsers
            in
            ( { model | users_orga = users_data }
            , out0 [ Ports.focusOn "userInput" ]
            )

        --Ports.inheritWith "usersSearchPanel"  @need it ?
        OnReset ->
            ( reset model, noOut )

        OnInput seemsEmail value ->
            if seemsEmail then
                ( setPattern value model, noOut )

            else
                ( setPattern value model, out0 [ send DoQueryUser ] )

        OnClickUser user ->
            let
                data =
                    clickUser user model
            in
            ( data, Out [] [] (Just ( True, [ data.form ] )) )

        OnClickEmail email ->
            let
                data =
                    clickEmail email model
            in
            ( data, Out [] [] (Just ( True, [ data.form ] )) )

        OnUnselect ->
            ( unselect model, Out [] [] (Just ( False, [] )) )

        DoQueryUser ->
            ( setDataResult LoadingSlowly model
            , out0 [ queryUser apis.gql model.form.pattern OnUsersAck ]
            )

        OnUsersAck result ->
            let
                data =
                    setDataResult result model
            in
            case parseErr result data.refresh_trial of
                Authenticate ->
                    ( setDataResult NotAsked model
                    , out1 [ DoAuth (uctxFromUser data.user) ]
                    )

                RefreshToken i ->
                    ( { data | refresh_trial = i }, out2 [ sendSleep DoQueryUser 500 ] [ DoUpdateToken ] )

                OkAuth users ->
                    ( data, out0 [ Ports.initUserSearchSeek (users ++ model.users_orga) model.form.pattern ] )

                _ ->
                    ( data, noOut )

        ChangeUserLookup data ->
            case data of
                Ok d ->
                    ( { model | lookup = LE.uniqueBy (\u -> u.username) d }, noOut )

                Err err ->
                    ( model, out0 [ Ports.logErr err ] )

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
    [ Ports.mcPD Ports.closeModalConfirmFromJs LogErr DoModalConfirmClose
    , Ports.lookupUserFromJs ChangeUserLookup
    ]



-- ------------------------------
-- V I E W
-- ------------------------------


type alias Op =
    {}


view : Op -> State -> Html Msg
view op (State model) =
    div []
        [ viewInput op model
        , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
        ]


viewInput : Op -> Model -> Html Msg
viewInput op model =
    let
        hasSelected =
            isUsersSendable [ model.form ]

        selectedUser =
            if hasSelected then
                let
                    user =
                        User model.form.username model.form.name
                in
                div [ class "tagsinput tags has-addons" ]
                    [ span [ class "tag is-rounded" ]
                        [ if model.form.email /= "" then
                            viewEmail model.form.email

                          else
                            viewUserFull 0 False False user
                        ]
                    , span [ class "tag is-delete is-rounded", onClick OnUnselect ] []
                    ]

            else
                text ""

        seemsEmail =
            model.form.pattern
                |> String.split "@"
                |> (\l ->
                        case l of
                            [ a, b ] ->
                                if a /= "" && b /= "" then
                                    True

                                else
                                    False

                            _ ->
                                False
                   )
    in
    div []
        [ div [ class "field mb-5" ]
            [ label [ class "label" ]
                [ text "Invite someone:" ]
            , div [ class "control" ]
                [ selectedUser
                , input
                    [ id "userInput"
                    , class "input is-rounded"
                    , type_ "text"
                    , placeholder <| ternary hasSelected "" "Username or email address"
                    , value model.form.pattern
                    , ternary hasSelected
                        (onClick NoMsg)
                        (onInput (OnInput seemsEmail))

                    --, disabled hasSelected
                    ]
                    []
                ]
            , if model.form.pattern == "" then
                text ""

              else if seemsEmail then
                viewEmailSelector op model

              else
                viewUserSelectors op model
            ]
        , case model.users_result of
            Failure err ->
                viewGqlErrors err

            _ ->
                text ""
        ]


viewUserSelectors : Op -> Model -> Html Msg
viewUserSelectors op model =
    div [ class "panel sidePanel" ]
        [ loadingSpinRight (model.users_result == LoadingSlowly)
        , div [ class "selectors" ] <|
            if model.lookup == [] then
                [ p [ class "panel-block" ] [ textH T.noResultsFound ] ]

            else
                model.lookup
                    |> List.map
                        (\u ->
                            p
                                [ class "panel-block pt-1 pb-1"
                                , onClick (OnClickUser u)
                                ]
                                [ viewUserFull 1 False False u ]
                        )
        ]


viewEmailSelector : Op -> Model -> Html Msg
viewEmailSelector op model =
    div [ class "panel sidePanel" ]
        [ div [ class "selectors has-background-grey" ]
            [ p
                [ class "panel-block"
                , onClick (OnClickEmail model.form.pattern)
                ]
                [ I.icon1 "icon-mail" T.invite
                , text T.space_
                , span [ class "is-italic" ] [ text "\"", text model.form.pattern, text "\"" ]
                ]
            ]
        ]


viewEmail : String -> Html Msg
viewEmail email =
    span []
        [ span [ class "mr-2" ]
            [ I.icon "icon-mail"
            ]
        , span [ attribute "style" "position:relative;top:-2px;" ]
            [ span [ class "is-email" ] [ text email ] ]
        ]
