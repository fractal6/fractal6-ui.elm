module Components.UserSearchPanel exposing (..)

import Auth exposing (ErrState(..), parseErr)
import Browser.Events as Events
import Codecs exposing (LookupResult)
import Components.Loading as Loading exposing (GqlData, RequestResult(..), loadingSpin, viewGqlErrors, withMapData, withMaybeData, withMaybeDataMap)
import Dict exposing (Dict)
import Dom
import Extra exposing (ternary)
import Fractal.Enum.TensionEvent as TensionEvent
import Global exposing (Msg(..), send, sendNow, sendSleep)
import Html exposing (Html, a, br, button, canvas, datalist, div, h1, h2, hr, i, input, label, li, nav, option, p, select, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, name, placeholder, required, rows, selected, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Icon as I
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (AssigneeForm, UserState(..), initAssigneeForm)
import ModelCommon.Codecs exposing (nearestCircleid)
import ModelCommon.View exposing (viewUser)
import ModelSchema exposing (..)
import Ports
import Process
import Query.PatchTension exposing (setAssignee)
import Query.QueryNode exposing (queryMembers)
import Session exposing (Apis, GlobalCmd(..), UserSearchPanelOnClickAction(..))
import Task
import Text as T exposing (textH, textT, upH)
import Time


type alias Model =
    Session.UserSearchPanelModel


type alias OnClickAction =
    Session.UserSearchPanelOnClickAction


type State
    = State Model


id_target_name : String
id_target_name =
    "usersPanelContent"


init : String -> OnClickAction -> UserState -> State
init tid action user =
    initModel tid action user |> State


initModel : String -> OnClickAction -> UserState -> Model
initModel tid action user =
    { isOpen = False
    , form = initAssigneeForm tid user
    , click_result = NotAsked
    , action = action

    -- Lookup
    , lookup = []
    , pattern = ""
    , assignees_data = NotAsked

    -- Common
    , refresh_trial = 0
    }


load : Maybe Model -> UserState -> State
load model user =
    case model of
        Just m ->
            State { m | click_result = NotAsked }

        Nothing ->
            init "" SelectUser user


getModel : State -> Model
getModel (State model) =
    model



-- Global methods


isOpen_ : State -> Bool
isOpen_ (State model) =
    model.isOpen


isEmpty_ : State -> Bool
isEmpty_ (State model) =
    (withMaybeData model.assignees_data |> withDefault [] |> List.length) == 0



-- State control


open : List String -> Model -> Model
open targets data =
    let
        form =
            data.form
    in
    { data | isOpen = True, form = { form | targets = targets } }


close : Model -> Model
close data =
    let
        form =
            data.form
    in
    { data | isOpen = False, click_result = NotAsked, pattern = "" }


click : User -> Bool -> Model -> Model
click assignee isNew data =
    let
        form =
            data.form
    in
    { data | form = { form | assignee = assignee, isNew = isNew } }


setClickResult : GqlData IdPayload -> Model -> Model
setClickResult result data =
    { data | click_result = result }



-- Update Form


setEvents : List TensionEvent.TensionEvent -> Model -> Model
setEvents events data =
    let
        f =
            data.form
    in
    { data | form = { f | events_type = Just events } }


updatePost : String -> String -> Model -> Model
updatePost field value data =
    let
        f =
            data.form
    in
    { data | form = { f | post = Dict.insert field value f.post } }


setPattern : String -> Model -> Model
setPattern pattern data =
    let
        form =
            data.form
    in
    { data | pattern = pattern }



-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = OnOpen (List String)
    | OnClose
    | OnClose_
    | OnChangePattern String
    | ChangeAssigneeLookup (LookupResult User)
    | OnAssigneeClick User Bool Time.Posix
    | OnAssigneeAck (GqlData IdPayload)
    | OnSubmit (Time.Posix -> Msg)
    | OnGotAssignees (GqlData (List User))
    | SetAssignee AssigneeForm
    | ResetClickResult


type alias Out =
    { cmds : List (Cmd Msg)
    , gcmds : List GlobalCmd
    , result : Maybe ( Bool, User )
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


update_ : Apis -> Msg -> Model -> ( Model, Out )
update_ apis message model =
    case message of
        OnOpen targets ->
            if model.isOpen == False then
                let
                    ( newModel, cmd ) =
                        ternary (targets /= model.form.targets)
                            ( { model | assignees_data = LoadingSlowly }, [ queryMembers apis.gql targets OnGotAssignees ] )
                            ( model, [] )
                in
                ( open targets newModel
                , out0 <|
                    [ Ports.inheritWith "usersSearchPanel"
                    , Ports.focusOn "userInput"
                    ]
                        ++ cmd
                )

            else
                ( model, noOut )

        OnClose ->
            -- The delay is used to be able to toggle (close) the panel on clicking the button.
            ( model, out0 [ sendSleep OnClose_ 50 ] )

        OnClose_ ->
            ( close model, noOut )

        OnGotAssignees result ->
            ( { model | assignees_data = result }
            , case result of
                Success r ->
                    out0 [ Ports.initUserSearch r ]

                _ ->
                    noOut
            )

        OnChangePattern pattern ->
            ( setPattern pattern model
            , out0 [ Ports.searchUser pattern ]
            )

        ChangeAssigneeLookup data ->
            case data of
                Ok d ->
                    ( { model | lookup = d }, noOut )

                Err err ->
                    ( model, out0 [ Ports.logErr err ] )

        OnAssigneeClick assignee isNew time ->
            if model.click_result == LoadingSlowly then
                -- wait here !
                ( model, noOut )

            else
                let
                    newModel =
                        click assignee isNew model
                in
                case model.action of
                    AssignUser ->
                        let
                            data =
                                newModel
                                    |> updatePost "createdAt" (fromTime time)
                                    |> updatePost "new" assignee.username
                                    |> setEvents [ ternary isNew TensionEvent.AssigneeAdded TensionEvent.AssigneeRemoved ]
                                    |> setClickResult LoadingSlowly
                        in
                        ( data
                        , out0 [ send (SetAssignee data.form) ]
                        )

                    SelectUser ->
                        let
                            data =
                                newModel
                                    |> updatePost "new" assignee.username
                                    |> setClickResult LoadingSlowly

                            users =
                                withMaybeData model.assignees_data
                                    |> withDefault []
                                    |> (\x ->
                                            if isNew then
                                                x ++ [ assignee ]

                                            else
                                                LE.remove assignee x
                                       )
                        in
                        ( { data | assignees_data = Success users }
                        , Out [ sendSleep ResetClickResult 333 ] [] (Just ( data.form.isNew, data.form.assignee ))
                        )

        OnAssigneeAck result ->
            let
                data =
                    setClickResult result model
            in
            case parseErr result data.refresh_trial of
                Authenticate ->
                    ( setClickResult NotAsked model
                    , out1 [ DoAuth data.form.uctx ]
                    )

                RefreshToken i ->
                    ( { data | refresh_trial = i }, out2 [ sendSleep (SetAssignee data.form) 500 ] [ DoUpdateToken ] )

                OkAuth _ ->
                    ( data, Out [] [] (Just ( data.form.isNew, data.form.assignee )) )

                _ ->
                    ( data, noOut )

        OnSubmit next ->
            ( model
            , out0 [ sendNow next ]
            )

        SetAssignee form ->
            ( model
            , out0 [ setAssignee apis.gql form OnAssigneeAck ]
            )

        ResetClickResult ->
            ( setClickResult NotAsked model, noOut )


subscriptions : State -> List (Sub Msg)
subscriptions (State model) =
    [ Ports.lookupUserFromJs ChangeAssigneeLookup
    ]
        ++ (if model.isOpen then
                [ Events.onMouseUp (Dom.outsideClickClose id_target_name OnClose)
                , Events.onKeyUp (Dom.key "Escape" OnClose)
                ]

            else
                []
           )



-- ------------------------------
-- V I E W
-- ------------------------------


type alias Op =
    { selectedAssignees : List User
    , targets : List String
    }


view_ : Op -> State -> Html Msg
view_ op_ (State model) =
    nav [ id "usersSearchPanel", class "panel sidePanel" ]
        [ case model.assignees_data of
            Success assignees_d ->
                let
                    selection =
                        List.map (\x -> x.username) op_.selectedAssignees

                    op =
                        { op_ | selectedAssignees = List.filter (\x -> List.member x.username selection) assignees_d }

                    user =
                        model.form.uctx |> List.singleton |> List.map (\u -> User u.username u.name)

                    users =
                        if model.pattern == "" then
                            op.selectedAssignees
                                ++ user
                                ++ List.take 42 assignees_d
                                |> LE.uniqueBy (\u -> u.username)

                        else
                            LE.uniqueBy (\u -> u.username) model.lookup
                in
                div []
                    [ div [ class "panel-block" ]
                        [ p [ class "control has-icons-left" ]
                            [ input
                                [ id "userInput"
                                , class "input autofocus is-small"
                                , type_ "text"
                                , placeholder (upH T.searchUsers)
                                , value model.pattern
                                , onInput OnChangePattern
                                ]
                                []
                            , span [ class "icon is-left" ] [ i [ attribute "aria-hidden" "true", class "icon-search" ] [] ]
                            ]
                        ]
                    , case model.click_result of
                        Failure err ->
                            viewGqlErrors err

                        _ ->
                            div [] []
                    , viewAssigneeSelectors users op model
                    ]

            Loading ->
                div [ class "spinner" ] [ text "" ]

            LoadingSlowly ->
                div [ class "spinner" ] [ text "" ]

            NotAsked ->
                div [] []

            Failure err ->
                viewGqlErrors err
        ]


viewAssigneeSelectors : List User -> Op -> Model -> Html Msg
viewAssigneeSelectors users op model =
    div [ class "selectors" ] <|
        if users == [] then
            [ p [ class "panel-block" ] [ textH T.noResultsFound ] ]

        else
            users
                |> List.map
                    (\u ->
                        let
                            isActive =
                                List.member u op.selectedAssignees

                            iconCls =
                                ternary isActive "icon-check-square" "icon-square"

                            isLoading =
                                model.click_result == LoadingSlowly && u.username == model.form.assignee.username
                        in
                        p
                            [ class "panel-block"
                            , classList [ ( "is-active", isActive ) ]
                            , onClick (OnSubmit <| OnAssigneeClick u (isActive == False))
                            ]
                            [ span [ class "panel-icon" ] [ I.icon iconCls ]
                            , viewUser False u.username
                            , case u.name of
                                Just name ->
                                    span [ class "has-text-weight-semibold" ] [ text name ]

                                Nothing ->
                                    span [] []
                            , span [ class "is-grey-light help" ] [ text u.username ]
                            , loadingSpin isLoading
                            ]
                    )



--
-- Input View
--


view : Op -> State -> Html Msg
view op (State model) =
    div [ id id_target_name ]
        [ if model.isOpen then
            view_ op (State model)

          else
            text ""
        ]


{-|

     @Debug: put this in User quicksearch module

-}
viewUserSelectors i pattern op =
    div [ class "selectors", classList [ ( "spinner", op.users_data == Loading ) ] ] <|
        case op.users_data of
            Success ud ->
                let
                    users =
                        if pattern == "" then
                            -- linked users
                            op.targets
                                |> List.foldl
                                    (\a b ->
                                        List.append (Dict.get a ud |> withDefault []) b
                                    )
                                    []
                                |> LE.uniqueBy (\u -> u.username)

                        else
                            op.lookup
                in
                if users == [] then
                    [ p [ class "panel-block" ] [ textH T.noResultsFound ] ]

                else
                    users
                        |> List.map
                            (\u ->
                                p
                                    [ class "panel-block"
                                    , onClick (op.onSelectUser i u.username)
                                    ]
                                    [ viewUser False u.username
                                    , case u.name of
                                        Just name ->
                                            span [ class "has-text-weight-semibold" ] [ text name ]

                                        Nothing ->
                                            span [] []
                                    , span [ class "is-grey-light help" ] [ text u.username ]
                                    ]
                            )

            _ ->
                []
