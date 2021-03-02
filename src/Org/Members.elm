module Org.Members exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Array
import Auth exposing (AuthState(..), doRefreshToken, refreshAuthModal)
import Browser.Events exposing (onKeyDown)
import Browser.Navigation as Nav
import Codecs exposing (QuickDoc)
import Components.HelperBar as HelperBar exposing (HelperBar)
import Components.Loading as Loading exposing (GqlData, ModalData, RequestResult(..), WebData, viewAuthNeeded, viewGqlErrors, withDefaultData, withMaybeData)
import Date exposing (formatTime)
import Dict exposing (Dict)
import Extra exposing (ternary)
import Extra.Events exposing (onClickPD, onEnter, onKeydown, onTab)
import Form exposing (isPostSendable)
import Form.Help as Help
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, br, button, datalist, div, h1, h2, hr, i, input, li, nav, option, p, span, table, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, placeholder, rows, target, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Icon as I
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Codecs exposing (Flags_, FractalBaseRoute(..), NodeFocus, basePathChanged, focusFromNameid, focusState, nameidFromFlags, uriFromNameid, uriFromUsername)
import ModelCommon.Requests exposing (fetchMembers, getQuickDoc, login)
import ModelCommon.View exposing (mediaTension, roleColor)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.AddTension exposing (addOneTension)
import Query.PatchTension exposing (actionRequest)
import Query.QueryNode exposing (fetchNode, queryLocalGraph, queryMembersTop)
import RemoteData exposing (RemoteData)
import Task
import Text as T exposing (textH, textT, upH)
import Time


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
                    DoNavigate link ->
                        ( send (Navigate link), Cmd.none )

                    DoAuth uctx ->
                        ( send (DoOpenAuthModal uctx), Cmd.none )

                    DoUpdateToken ->
                        ( Cmd.none, send UpdateUserToken )

                    _ ->
                        ( Cmd.none, Cmd.none )
            )
        |> List.unzip



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

    -- Common
    , node_action : ActionState
    , isModalActive : Bool -- Only use by JoinOrga for now. (other actions rely on Bulma drivers)
    , modalAuth : ModalAuth
    , helperBar : HelperBar
    , refresh_trial : Int
    , help : Help.State
    }



--
-- Msg
--


type Msg
    = PassedSlowLoadTreshold -- timer
    | PushTension TensionForm (GqlData Tension -> Msg)
    | PushGuest ActionForm
    | Submit (Time.Posix -> Msg) -- Get Current Time
      -- Data Queries
    | GotPath (GqlData LocalGraph) -- GraphQL
    | GotPath2 (GqlData LocalGraph) -- GraphQL
      -- Page
    | GotMembersTop (GqlData (List Member)) -- GraphQL
    | GotMembersSub (GqlData (List Member)) -- Rest
      -- JoinOrga Action
    | DoJoinOrga String
    | DoJoinOrga2 (GqlData Node)
    | DoJoinOrga3 Node Time.Posix
    | JoinAck (GqlData ActionResult)
      -- Help
    | HelpMsg Help.Msg
      -- Token refresh
    | DoOpenAuthModal UserCtx
    | DoCloseAuthModal String
    | ChangeAuthPost String String
    | SubmitUser UserAuthForm
    | GotSignin (WebData UserCtx)
    | SubmitKeyDown Int -- Detect Enter (for form sending)
      -- Common
    | NoMsg
    | LogErr String
    | Navigate String
    | DoOpenModal
    | DoCloseModal ModalData
    | ExpandRoles
    | CollapseRoles



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

        -- Focus
        newFocus =
            flags
                |> nameidFromFlags
                |> focusFromNameid

        -- What has changed
        fs =
            focusState MembersBaseUri global.session.referer global.session.node_focus newFocus

        model =
            { node_focus = newFocus
            , path_data =
                global.session.path_data
                    |> Maybe.map (\x -> Success x)
                    |> withDefault Loading
            , members_top = Loading
            , members_sub = Loading

            -- Common
            , node_action = NoOp
            , isModalActive = False
            , modalAuth = Inactive
            , helperBar = HelperBar.create
            , refresh_trial = 0
            , help = Help.init global.session.user
            }

        cmds =
            [ ternary fs.focusChange (queryLocalGraph apis.gql newFocus.nameid GotPath) Cmd.none
            , queryMembersTop apis.gql newFocus.nameid GotMembersTop
            , fetchMembers apis.rest newFocus.nameid GotMembersSub
            , sendSleep PassedSlowLoadTreshold 500
            ]
    in
    ( model
    , Cmd.batch cmds
    , send (UpdateSessionFocus (Just newFocus))
    )


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global message model =
    let
        apis =
            global.session.apis
    in
    case message of
        PushTension form ack ->
            ( model, addOneTension apis.gql form ack, Cmd.none )

        PushGuest form ->
            ( model, actionRequest apis.gql form JoinAck, Cmd.none )

        PassedSlowLoadTreshold ->
            let
                members_top =
                    ternary (model.members_top == Loading) LoadingSlowly model.members_top

                members_sub =
                    ternary (model.members_sub == Loading) LoadingSlowly model.members_sub
            in
            ( { model | members_top = members_top, members_sub = members_sub }, Cmd.none, Cmd.none )

        Submit nextMsg ->
            ( model, Task.perform nextMsg Time.now, Cmd.none )

        -- Data queries
        GotPath result ->
            let
                newModel =
                    { model | path_data = result }
            in
            case result of
                Success path ->
                    case path.root of
                        Just root ->
                            ( newModel, Cmd.none, send (UpdateSessionPath (Just path)) )

                        Nothing ->
                            let
                                nameid =
                                    List.head path.path |> Maybe.map (\p -> p.nameid) |> withDefault ""
                            in
                            ( newModel, queryLocalGraph apis.gql nameid GotPath2, Cmd.none )

                _ ->
                    ( newModel, Cmd.none, Cmd.none )

        GotPath2 result ->
            case model.path_data of
                Success prevPath ->
                    case result of
                        Success path ->
                            case path.root of
                                Just root ->
                                    let
                                        newPath =
                                            { prevPath | root = Just root, path = path.path ++ (List.tail prevPath.path |> withDefault []) }
                                    in
                                    ( { model | path_data = Success newPath }, Cmd.none, send (UpdateSessionPath (Just newPath)) )

                                Nothing ->
                                    let
                                        nameid =
                                            List.head path.path |> Maybe.map (\p -> p.nameid) |> withDefault ""

                                        newPath =
                                            { prevPath | path = path.path ++ (List.tail prevPath.path |> withDefault []) }
                                    in
                                    ( { model | path_data = Success newPath }, queryLocalGraph apis.gql nameid GotPath2, Cmd.none )

                        _ ->
                            ( model, Cmd.none, Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        GotMembersTop result ->
            let
                newModel =
                    { model | members_top = result }
            in
            ( newModel, Cmd.none, Cmd.none )

        GotMembersSub result ->
            let
                newModel =
                    { model | members_sub = result }
            in
            ( newModel, Cmd.none, Cmd.none )

        -- Join
        DoJoinOrga rootnameid ->
            case global.session.user of
                LoggedOut ->
                    ( { model | node_action = ActionAuthNeeded }
                    , send DoOpenModal
                    , Cmd.none
                    )

                LoggedIn uctx ->
                    ( { model | node_action = JoinOrga (JoinInit LoadingSlowly) }
                    , Cmd.batch [ fetchNode apis.gql rootnameid DoJoinOrga2, send DoOpenModal ]
                    , Cmd.none
                    )

        DoJoinOrga2 result ->
            case result of
                Success n ->
                    ( { model | node_action = JoinOrga (JoinInit LoadingSlowly) }
                    , send (Submit <| DoJoinOrga3 n)
                    , Cmd.none
                    )

                other ->
                    ( { model | node_action = JoinOrga (JoinInit result) }, Cmd.none, Cmd.none )

        DoJoinOrga3 node time ->
            let
                ( tid, bid ) =
                    node.source
                        |> Maybe.map (\b -> ( b.tension.id, b.id ))
                        |> withDefault ( "", "" )

                f =
                    initActionForm tid global.session.user

                form =
                    { f
                        | bid = "" -- do no set bid to pass the backend
                        , events_type = Just [ TensionEvent.UserJoin ]
                        , post =
                            Dict.fromList
                                [ ( "createdAt", fromTime time )
                                , ( "old", f.uctx.username )
                                , ( "new", node.nameid )
                                ]
                        , node = node
                    }
            in
            ( { model | node_action = JoinOrga (JoinValidation form LoadingSlowly) }
            , Cmd.batch [ send (PushGuest form), send DoOpenModal ]
            , Cmd.none
            )

        JoinAck result ->
            case model.node_action of
                JoinOrga (JoinValidation form _) ->
                    case doRefreshToken result model.refresh_trial of
                        Authenticate ->
                            ( model, send (DoOpenAuthModal form.uctx), Cmd.none )

                        RefreshToken i ->
                            ( { model | refresh_trial = i }, sendSleep (PushGuest form) 500, send UpdateUserToken )

                        OkAuth n ->
                            ( { model | node_action = JoinOrga (JoinValidation form result) }
                            , Cmd.none
                            , send UpdateUserToken
                            )

                        NoAuth ->
                            ( { model | node_action = JoinOrga (JoinValidation form result) }, Cmd.none, Cmd.none )

                default ->
                    ( model, Cmd.none, Cmd.none )

        -- Token Refresh
        DoOpenAuthModal uctx ->
            ( { model
                | modalAuth =
                    Active
                        { post = Dict.fromList [ ( "username", uctx.username ) ]
                        , result = RemoteData.NotAsked
                        }
              }
            , Cmd.none
            , Ports.open_auth_modal
            )

        DoCloseAuthModal link ->
            let
                cmd =
                    ternary (link /= "") (send (Navigate link)) Cmd.none
            in
            case model.node_action of
                JoinOrga _ ->
                    ( { model | modalAuth = Inactive }, Cmd.batch [ cmd, send (DoCloseModal { reset = True, link = "" }) ], Ports.close_auth_modal )

                _ ->
                    ( { model | modalAuth = Inactive }, cmd, Ports.close_auth_modal )

        ChangeAuthPost field value ->
            case model.modalAuth of
                Active form ->
                    let
                        newForm =
                            { form | post = Dict.insert field value form.post }
                    in
                    ( { model | modalAuth = Active newForm }, Cmd.none, Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        SubmitUser form ->
            ( model, login apis.auth form.post GotSignin, Cmd.none )

        GotSignin result ->
            case result of
                RemoteData.Success uctx ->
                    let
                        cmd =
                            case model.modalAuth of
                                Active f ->
                                    case Dict.get "msg" f.post of
                                        Just "GotOrga" ->
                                            sendSleep (Navigate (uriFromNameid OverviewBaseUri model.node_focus.rootnameid)) 500

                                        _ ->
                                            Cmd.none

                                _ ->
                                    Cmd.none
                    in
                    ( { model | modalAuth = Inactive }
                    , Cmd.batch [ send (DoCloseAuthModal ""), cmd ]
                    , send (UpdateUserSession uctx)
                    )

                other ->
                    case model.modalAuth of
                        Active form ->
                            ( { model | modalAuth = Active { form | result = result } }, Cmd.none, Cmd.none )

                        Inactive ->
                            ( model, Cmd.none, Cmd.none )

        SubmitKeyDown key ->
            case key of
                13 ->
                    let
                        form =
                            case model.modalAuth of
                                Active f ->
                                    f

                                Inactive ->
                                    UserAuthForm Dict.empty RemoteData.NotAsked
                    in
                    --ENTER
                    if isPostSendable [ "password" ] form.post then
                        ( model, send (SubmitUser form), Cmd.none )

                    else
                        ( model, Cmd.none, Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        -- Modal
        DoOpenModal ->
            ( { model | isModalActive = True }, Ports.open_modal "actionModal", Cmd.none )

        DoCloseModal data ->
            let
                gcmd =
                    if data.link /= "" then
                        send (Navigate data.link)

                    else
                        Cmd.none
            in
            ( { model | isModalActive = False }, gcmd, Ports.close_modal )

        -- Common
        NoMsg ->
            ( model, Cmd.none, Cmd.none )

        LogErr err ->
            ( model, Ports.logErr err, Cmd.none )

        Navigate url ->
            ( model, Cmd.none, Nav.pushUrl global.key url )

        ExpandRoles ->
            ( { model | helperBar = HelperBar.expand model.helperBar }, Cmd.none, Cmd.none )

        CollapseRoles ->
            ( { model | helperBar = HelperBar.collapse model.helperBar }, Cmd.none, Cmd.none )

        -- Help
        HelpMsg msg ->
            let
                ( help, out ) =
                    Help.update apis msg model.help

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | help = help }, out.cmds |> List.map (\m -> Cmd.map HelpMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions global model =
    [ Ports.mcPD Ports.closeModalFromJs LogErr DoCloseModal
    ]
        ++ (Help.subscriptions |> List.map (\s -> Sub.map HelpMsg s))
        |> Sub.batch



---- VIEW ----


view : Global.Model -> Model -> Document Msg
view global model =
    { title = upH T.members ++ " · " ++ (String.join "/" <| LE.unique [ model.node_focus.rootnameid, model.node_focus.nameid |> String.split "#" |> List.reverse |> List.head |> withDefault "" ])
    , body =
        [ view_ global model
        , Help.view {} model.help |> Html.map HelpMsg
        , refreshAuthModal model.modalAuth { closeModal = DoCloseAuthModal, changePost = ChangeAuthPost, submit = SubmitUser, submitEnter = SubmitKeyDown }
        ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    let
        helperData =
            { onJoin = DoJoinOrga model.node_focus.rootnameid
            , onExpand = ExpandRoles
            , onCollapse = CollapseRoles
            , user = global.session.user
            , path_data = global.session.path_data
            , baseUri = MembersBaseUri
            , data = model.helperBar
            }
    in
    div [ id "mainPane" ]
        [ HelperBar.view helperData
        , div [ class "columns is-centered" ]
            [ div [ class "column is-10-desktop is-10-widescreen is-9-fullhd" ]
                [ div [ class "columns" ]
                    [ viewMembers model.members_top model.node_focus ]
                , div [ class "columns" ]
                    [ viewMembersSub model.members_sub model.node_focus ]
                , div [ class "columns" ]
                    [ viewGuest model.members_top T.guest model.node_focus ]
                ]
            ]
        , setupActionModal model.isModalActive model.node_action
        ]


viewMembers : GqlData (List Member) -> NodeFocus -> Html Msg
viewMembers data focus =
    case data of
        Success members_ ->
            let
                members =
                    members_
                        |> List.map
                            (\m ->
                                case memberRolesFilter focus m.roles of
                                    [] ->
                                        Nothing

                                    r ->
                                        Just { m | roles = r }
                            )
                        |> List.filterMap identity
            in
            case members of
                [] ->
                    div [ class "section" ] [ [ "No", T.member, "yet." ] |> String.join " " |> text ]

                mbs ->
                    div [ id "membersTable", class "section" ]
                        [ h2 [ class "subtitle has-text-weight-semibold" ] [ textH T.directMembers ]
                        , div [ class "table is-fullwidth" ]
                            [ thead []
                                [ tr []
                                    [ th [] [ textH T.username ]
                                    , th [] [ textH T.name ]
                                    , th [ class "" ] [ textH T.roles ]
                                    ]
                                ]
                            , tbody [] <|
                                List.indexedMap
                                    (\i m ->
                                        tr []
                                            [ td [] [ a [ href (uriFromUsername UsersBaseUri m.username) ] [ "@" ++ m.username |> text ] ]
                                            , td [] [ m.name |> withDefault "--" |> text ]
                                            , td [] [ viewMemberRoles OverviewBaseUri m.roles ]
                                            ]
                                    )
                                    mbs
                            ]
                        ]

        Failure err ->
            viewGqlErrors err

        LoadingSlowly ->
            div [ class "spinner" ] []

        other ->
            text ""


viewMembersSub : GqlData (List Member) -> NodeFocus -> Html Msg
viewMembersSub data focus =
    case data of
        Success members_ ->
            let
                members =
                    members_
                        |> List.map
                            (\m ->
                                case memberRolesFilter focus m.roles of
                                    [] ->
                                        Nothing

                                    r ->
                                        Just { m | roles = r }
                            )
                        |> List.filterMap identity
            in
            case members of
                [] ->
                    div [ class "section" ] [ [ "No sub-circle", T.member, "yet." ] |> String.join " " |> text ]

                mbs ->
                    div [ id "membersTable", class "section" ]
                        [ h2 [ class "subtitle has-text-weight-semibold" ] [ textH T.subMembers ]
                        , div [ class "table is-fullwidth" ]
                            [ thead []
                                [ tr []
                                    [ th [] [ textH T.username ]
                                    , th [] [ textH T.name ]
                                    , th [ class "" ] [ textH T.roles ]
                                    ]
                                ]
                            , tbody [] <|
                                List.indexedMap
                                    (\i m ->
                                        tr []
                                            [ td [] [ a [ href (uriFromUsername UsersBaseUri m.username) ] [ "@" ++ m.username |> text ] ]
                                            , td [] [ m.name |> withDefault "--" |> text ]
                                            , td [] [ viewMemberRoles OverviewBaseUri m.roles ]
                                            ]
                                    )
                                    mbs
                            ]
                        ]

        Failure err ->
            viewGqlErrors err

        LoadingSlowly ->
            div [ class "spinner" ] []

        other ->
            text ""


viewGuest : GqlData (List Member) -> String -> NodeFocus -> Html Msg
viewGuest members_d title focus =
    let
        guests =
            members_d
                |> withDefaultData []
                |> List.filter
                    (\u ->
                        u.roles
                            |> List.map (\r -> r.role_type)
                            |> List.member RoleType.Guest
                    )
    in
    if List.length guests > 0 then
        div [ id "membersTable", class "section" ]
            [ h2 [ class "subtitle has-text-weight-semibold" ] [ text title ]
            , div [ class "table is-fullwidth" ]
                [ thead []
                    [ tr []
                        [ th [] [ textH T.username ]
                        , th [] [ textH T.name ]
                        ]
                    ]
                , tbody [] <|
                    List.indexedMap
                        (\i m ->
                            tr []
                                [ td [] [ a [ href (uriFromUsername UsersBaseUri m.username) ] [ "@" ++ m.username |> text ] ]
                                , td [] [ m.name |> withDefault "--" |> text ]
                                ]
                        )
                        guests
                ]
            ]

    else
        div [] []


memberRolesFilter : NodeFocus -> List UserRoleExtended -> List UserRoleExtended
memberRolesFilter focus roles =
    roles
        |> List.map
            (\r ->
                if r.role_type == RoleType.Guest then
                    -- Filter Guest roles
                    []

                else if r.role_type == RoleType.Member && List.length roles > 1 then
                    -- Filter Member with roles
                    []

                else if focus.nameid == (r.parent |> Maybe.map (\p -> p.nameid) |> withDefault "") then
                    -- Dont include top level member for sub circle member (which contains all member)
                    -- Note: .parentid not defined in the top member query
                    []

                else
                    [ r ]
            )
        |> List.concat


viewMemberRoles : FractalBaseRoute -> List UserRoleExtended -> Html msg
viewMemberRoles baseUri roles =
    div [ class "buttons" ] <|
        List.map
            (\r ->
                a
                    [ class ("button buttonRole is-small tooltip has-tooltip-bottom is-" ++ roleColor r.role_type)
                    , attribute "data-tooltip" ([ r.name, "of", getParentFragmentFromRole r, "since the", formatTime r.createdAt ] |> String.join " ")
                    , href <| uriFromNameid baseUri r.nameid
                    ]
                    [ if r.role_type == RoleType.Guest then
                        textH T.guest

                      else if r.role_type == RoleType.Member then
                        textH T.member

                      else if r.role_type == RoleType.Owner then
                        textH T.owner

                      else
                        -- Peer
                        text r.name
                    ]
            )
            roles



-- Actions


setupActionModal : Bool -> ActionState -> Html Msg
setupActionModal isModalActive action =
    div
        [ id "actionModal"
        , class "modal modal-fx-fadeIn"
        , classList [ ( "is-active", isModalActive ) ]
        , attribute "data-modal-close" "closeModalFromJs"
        ]
        [ div
            [ class "modal-background modal-escape"
            , attribute "data-modal" "actionModal"
            , onClick (DoCloseModal { reset = True, link = "" })
            ]
            []
        , div [ class "modal-content" ]
            [ case action of
                JoinOrga step ->
                    viewJoinOrgaStep step

                NoOp ->
                    text ""

                AskErr err ->
                    viewGqlErrors [ err ]

                ActionAuthNeeded ->
                    viewAuthNeeded DoCloseModal
            ]
        , button [ class "modal-close is-large", onClick (DoCloseModal { reset = True, link = "" }) ] []
        ]


viewJoinOrgaStep : JoinStep ActionForm -> Html Msg
viewJoinOrgaStep step =
    case step of
        JoinInit _ ->
            div [ class "box spinner" ] [ text "" ]

        JoinNotAuthorized errMsg ->
            viewGqlErrors errMsg

        JoinValidation form result ->
            case result of
                Success _ ->
                    div [ class "box is-light", onClick (DoCloseModal { reset = True, link = "" }) ]
                        [ I.icon1 "icon-check icon-2x has-text-success" " "
                        , textH T.welcomIn
                        , text " "
                        , span [ class "has-font-weight-semibold" ] [ text form.node.name ]
                        ]

                Failure err ->
                    viewGqlErrors err

                default ->
                    div [ class "box spinner" ] [ text "" ]
