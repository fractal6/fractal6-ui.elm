module Components.AssigneeSearchPanel exposing (Msg, State, init, subscriptions, update, view, viewUserSelectors)

import Auth exposing (AuthState(..), doRefreshToken, refreshAuthModal)
import Codecs exposing (LookupResult)
import Icon as I
import Components.Loading as Loading exposing (GqlData, RequestResult(..), loadingSpin, viewGqlErrors, withMapData, withMaybeData, withMaybeDataMap)
import Dict exposing (Dict)
import Extra exposing (ternary)
import Fractal.Enum.TensionEvent as TensionEvent
import Global exposing (send, sendNow, sendSleep)
import Html exposing (Html, a, br, button, canvas, datalist, div, h1, h2, hr, i, input, label, li, nav, option, p, select, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, name, placeholder, required, rows, selected, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (Apis, AssigneeForm, GlobalCmd(..), UserState(..), initAssigneeForm)
import ModelCommon.Codecs exposing (nearestCircleid)
import ModelCommon.View exposing (viewUser)
import ModelSchema exposing (..)
import Ports
import Query.PatchTension exposing (setAssignee)
import Query.QueryNode exposing (queryMembers)
import Task
import Text as T exposing (textH, textT, upH)
import Time


type State
    = State Model


type alias Model =
    { isOpen : Bool
    , form : AssigneeForm
    , click_result : GqlData IdPayload

    -- Lookup
    , lookup : List User
    , pattern : String -- search pattern
    , assignees_data : GqlData (List User)

    --, init_lookup : List User -> Cmd Msg
    --, search_lookup : String -> Cmd Msg
    -- Common
    , refresh_trial : Int
    }


init : String -> UserState -> State
init tid user =
    initModel tid user |> State


initModel : String -> UserState -> Model
initModel tid user =
    { isOpen = False
    , form = initAssigneeForm tid user
    , click_result = NotAsked

    -- Lookup
    , lookup = []
    , pattern = ""
    , assignees_data = NotAsked

    -- Common
    , refresh_trial = 0
    }



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


post : String -> String -> Model -> Model
post field value data =
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
    | OnChangePattern String
    | ChangeAssigneeLookup (LookupResult User)
    | OnAssigneeClick User Bool Time.Posix
    | OnAssigneeAck (GqlData IdPayload)
    | OnSubmit (Time.Posix -> Msg)
    | OnGotAssignees (GqlData (List User))
    | SetAssignee AssigneeForm


type alias Out =
    { cmds : List (Cmd Msg)
    , gcmds : List GlobalCmd
    , result : Maybe ( Bool, User )
    }


noOut : Out
noOut =
    Out [] [] Nothing


out1 : List (Cmd Msg) -> Out
out1 cmds =
    Out cmds [] Nothing


out2 : List GlobalCmd -> Out
out2 cmds =
    Out [] cmds Nothing


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
                    cmd =
                        ternary (targets /= model.form.targets)
                            [ queryMembers apis.gql targets OnGotAssignees ]
                            []
                in
                ( open targets model
                , out1 <|
                    [ Ports.outsideClickClose "cancelAssigneesFromJs" "assigneesPanelContent"
                    , Ports.inheritWith "assigneeSearchPanel"
                    , Ports.focusOn "userInput"
                    ]
                        ++ cmd
                )

            else
                ( model, noOut )

        OnClose ->
            ( close model, noOut )

        OnGotAssignees result ->
            ( { model | assignees_data = result }
            , out1 <|
                case result of
                    Success r ->
                        [ Ports.initUserSearch r ]

                    _ ->
                        []
            )

        OnChangePattern pattern ->
            ( setPattern pattern model
            , out1 [ Ports.searchUser pattern ]
            )

        ChangeAssigneeLookup data ->
            case data of
                Ok d ->
                    ( { model | lookup = d }, noOut )

                Err err ->
                    ( model, out1 [ Ports.logErr err ] )

        OnAssigneeClick assignee isNew time ->
            let
                data =
                    click assignee isNew model
                        |> post "createdAt" (fromTime time)
                        |> post "new" assignee.username
                        |> setEvents [ ternary isNew TensionEvent.AssigneeAdded TensionEvent.AssigneeRemoved ]
                        |> setClickResult LoadingSlowly
            in
            ( data
            , out1 [ send (SetAssignee data.form) ]
            )

        OnAssigneeAck result ->
            let
                data =
                    setClickResult result model
            in
            case doRefreshToken result data.refresh_trial of
                Authenticate ->
                    ( setClickResult NotAsked model
                    , out2 [ DoAuth data.form.uctx ]
                    )

                RefreshToken i ->
                    ( { data | refresh_trial = i }, Out [ sendSleep (SetAssignee data.form) 500 ] [ DoUpdateToken ] Nothing )

                OkAuth _ ->
                    ( data, Out [] [] (Just ( data.form.isNew, data.form.assignee )) )

                NoAuth ->
                    ( data, noOut )

        OnSubmit next ->
            ( model
            , out1 [ sendNow next ]
            )

        SetAssignee form ->
            ( model
            , out1 [ setAssignee apis.gql form OnAssigneeAck ]
            )


subscriptions =
    [ Ports.cancelAssigneesFromJs (always OnClose)
    , Ports.lookupUserFromJs ChangeAssigneeLookup
    ]



-- ------------------------------
-- V I E W
-- ------------------------------


type alias Op =
    { selectedAssignees : List User
    , targets : List String
    , isAdmin : Bool
    }


view_ : Op -> State -> Html Msg
view_ op (State model) =
    nav [ id "assigneeSearchPanel", class "panel sidePanel" ]
        [ case model.assignees_data of
            Success assignees_d ->
                let
                    user =
                        model.form.uctx |> List.singleton |> List.map (\u -> User u.username u.name)

                    users =
                        if model.pattern == "" then
                            op.selectedAssignees
                                ++ user
                                ++ List.take 20 assignees_d
                                |> LE.uniqueBy (\u -> u.username)

                        else
                            LE.uniqueBy (\u -> u.username) model.lookup
                in
                div []
                    [ div [ class "panel-block" ]
                        [ p [ class "control has-icons-left" ]
                            [ input
                                [ id "userInput"
                                , class "input autofocus"
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
    div []
        [ h2
            [ class "subtitle"
            , classList [ ( "is-w", op.isAdmin ) ]
            , onClick (OnOpen op.targets)
            ]
            [ textH T.assignees
            , if model.isOpen then
                I.icon "icon-x is-pulled-right"

              else if op.isAdmin then
                I.icon "icon-settings is-pulled-right"

              else
                text ""
            ]
        , div [ id "assigneesPanelContent" ]
            [ if model.isOpen then
                view_ op (State model)

              else
                text ""
            ]
        ]


{-|

     @Debug: put this in User quicsearch module

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
