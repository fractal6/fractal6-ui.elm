module Org.Members exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Array
import Assets as A
import Auth exposing (ErrState(..), parseErr, refreshAuthModal)
import Browser.Navigation as Nav
import Codecs exposing (QuickDoc)
import Components.HelperBar as HelperBar exposing (HelperBar)
import Components.JoinOrga as JoinOrga
import Components.Loading as Loading exposing (GqlData, ModalData, RequestResult(..), WebData, fromMaybeData, viewAuthNeeded, viewGqlErrors, withDefaultData, withMaybeData)
import Dict exposing (Dict)
import Extra exposing (ternary)
import Extra.Date exposing (formatDate)
import Extra.Events exposing (onClickPD, onEnter, onKeydown, onTab)
import Form exposing (isPostSendable)
import Form.Help as Help
import Form.NewTension as NTF exposing (TensionTab(..))
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
import Html.Lazy as Lazy
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Codecs exposing (Flags_, FractalBaseRoute(..), NodeFocus, basePathChanged, focusFromNameid, focusState, nameidFromFlags, uriFromNameid, uriFromUsername)
import ModelCommon.Requests exposing (fetchMembersSub, getQuickDoc, login)
import ModelCommon.View exposing (roleColor, viewMemberRole, viewUser)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.PatchTension exposing (actionRequest)
import Query.QueryNode exposing (fetchNode, queryLocalGraph, queryMembersLocal)
import RemoteData exposing (RemoteData)
import Session exposing (GlobalCmd(..))
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
    , modalAuth : ModalAuth
    , helperBar : HelperBar
    , refresh_trial : Int
    , now : Time.Posix

    -- Components
    , help : Help.State
    , tensionForm : NTF.State
    , joinOrga : JoinOrga.State
    }



--
-- Msg
--


type Msg
    = PassedSlowLoadTreshold -- timer
    | Submit (Time.Posix -> Msg) -- Get Current Time
      -- Data Queries
    | GotPath Bool (GqlData LocalGraph) -- GraphQL
      -- Page
    | GotMembers (GqlData (List Member)) -- GraphQL
    | GotMembersSub (GqlData (List Member)) -- Rest
      -- New Tension
    | DoCreateTension LocalGraph
      -- Token refresh
    | DoOpenAuthModal UserCtx
    | DoCloseAuthModal String
    | ChangeAuthPost String String
    | SubmitUser UserAuthForm
    | GotSignin (WebData UserCtx)
    | SubmitKeyDown Int -- Detect Enter (for form sending)
      -- Common
    | NoMsg
    | InitModals
    | LogErr String
    | Navigate String
    | ExpandRoles
    | CollapseRoles
      -- Components
    | HelpMsg Help.Msg
    | NewTensionMsg NTF.Msg
    | JoinOrgaMsg JoinOrga.Msg



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
            focusState MembersBaseUri global.session.referer global.url global.session.node_focus newFocus

        model =
            { node_focus = newFocus
            , path_data =
                global.session.path_data
                    |> Maybe.map (\x -> Success x)
                    |> withDefault Loading
            , members_top = Loading
            , members_sub = Loading

            -- Common
            , modalAuth = Inactive
            , helperBar = HelperBar.create
            , help = Help.init global.session.user
            , tensionForm = NTF.init global.session.user
            , refresh_trial = 0
            , now = global.now
            , joinOrga = JoinOrga.init newFocus.nameid global.session.user
            }

        cmds =
            [ ternary fs.focusChange (queryLocalGraph apis newFocus.nameid (GotPath True)) Cmd.none
            , queryMembersLocal apis newFocus.nameid GotMembers
            , fetchMembersSub apis newFocus.nameid GotMembersSub
            , sendSleep PassedSlowLoadTreshold 500
            , sendSleep InitModals 400
            ]
    in
    ( model
    , Cmd.batch cmds
    , if fs.refresh then
        send (UpdateSessionFocus (Just newFocus))

      else
        Cmd.none
    )


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global message model =
    let
        apis =
            global.session.apis
    in
    case message of
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
                                    List.head path.path |> Maybe.map (\p -> p.nameid) |> withDefault ""
                            in
                            ( { model | path_data = Success newPath }, queryLocalGraph apis nameid (GotPath False), Cmd.none )

                _ ->
                    ( { model | path_data = result }, Cmd.none, Cmd.none )

        GotMembers result ->
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

        -- New tension
        DoCreateTension lg ->
            let
                tf =
                    model.tensionForm
                        |> NTF.setUser_ global.session.user
                        |> NTF.setPath_ lg
            in
            ( { model | tensionForm = tf }, Cmd.map NewTensionMsg (send NTF.OnOpen), Cmd.none )

        NewTensionMsg msg ->
            let
                ( tf, out ) =
                    NTF.update apis msg model.tensionForm

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | tensionForm = tf }, out.cmds |> List.map (\m -> Cmd.map NewTensionMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )

        -- Token Refresh
        DoOpenAuthModal uctx ->
            ( { model
                | modalAuth =
                    Active { post = Dict.fromList [ ( "username", uctx.username ) ] } RemoteData.NotAsked
              }
            , Cmd.none
            , Ports.open_auth_modal
            )

        DoCloseAuthModal link ->
            ( { model | modalAuth = Inactive }
            , ternary (link /= "") (send (Navigate link)) Cmd.none
            , Ports.close_auth_modal
            )

        ChangeAuthPost field value ->
            case model.modalAuth of
                Active f r ->
                    let
                        form =
                            { f | post = Dict.insert field value f.post }
                    in
                    ( { model | modalAuth = Active form r }, Cmd.none, Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        SubmitUser form ->
            ( model, login apis form.post GotSignin, Cmd.none )

        GotSignin result ->
            case result of
                RemoteData.Success uctx ->
                    let
                        cmd =
                            case model.modalAuth of
                                Active f _ ->
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

                _ ->
                    case model.modalAuth of
                        Active form _ ->
                            ( { model | modalAuth = Active form result }, Cmd.none, Cmd.none )

                        Inactive ->
                            ( model, Cmd.none, Cmd.none )

        SubmitKeyDown key ->
            case key of
                13 ->
                    let
                        form =
                            case model.modalAuth of
                                Active f _ ->
                                    f

                                Inactive ->
                                    UserAuthForm Dict.empty
                    in
                    --ENTER
                    if isPostSendable [ "password" ] form.post then
                        ( model, send (SubmitUser form), Cmd.none )

                    else
                        ( model, Cmd.none, Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )

        -- Common
        NoMsg ->
            ( model, Cmd.none, Cmd.none )

        InitModals ->
            ( { model | tensionForm = NTF.fixGlitch_ model.tensionForm }, Cmd.none, Cmd.none )

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

        JoinOrgaMsg msg ->
            let
                ( data, out ) =
                    JoinOrga.update apis msg model.joinOrga

                ( cmds, gcmds ) =
                    mapGlobalOutcmds out.gcmds
            in
            ( { model | joinOrga = data }, out.cmds |> List.map (\m -> Cmd.map JoinOrgaMsg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions _ model =
    []
        ++ (Help.subscriptions |> List.map (\s -> Sub.map HelpMsg s))
        ++ (NTF.subscriptions model.tensionForm |> List.map (\s -> Sub.map NewTensionMsg s))
        ++ (JoinOrga.subscriptions model.joinOrga |> List.map (\s -> Sub.map JoinOrgaMsg s))
        |> Sub.batch



---- VIEW ----


view : Global.Model -> Model -> Document Msg
view global model =
    let
        helperData =
            { user = global.session.user
            , uriQuery = global.url.query
            , path_data = global.session.path_data
            , baseUri = MembersBaseUri
            , data = model.helperBar
            , onExpand = ExpandRoles
            , onCollapse = CollapseRoles
            , onCreateTension = DoCreateTension
            }
    in
    { title = upH T.members ++ " · " ++ (String.join "/" <| LE.unique [ model.node_focus.rootnameid, model.node_focus.nameid |> String.split "#" |> List.reverse |> List.head |> withDefault "" ])
    , body =
        [ Lazy.lazy HelperBar.view helperData
        , div [ id "mainPane" ] [ view_ model ]
        , Lazy.lazy2 refreshAuthModal model.modalAuth { closeModal = DoCloseAuthModal, changePost = ChangeAuthPost, submit = SubmitUser, submitEnter = SubmitKeyDown }
        , Help.view {} model.help |> Html.map HelpMsg
        , NTF.view { users_data = fromMaybeData global.session.users_data NotAsked } model.tensionForm |> Html.map NewTensionMsg
        , JoinOrga.view {} model.joinOrga |> Html.map JoinOrgaMsg
        ]
    }


view_ : Model -> Html Msg
view_ model =
    div [ class "columns is-centered" ]
        [ div [ class "column is-11 is-11-desktop is-9-fullhd" ]
            [ div [ class "columns" ]
                [ viewMembers model.now model.members_top model.node_focus ]
            , div [ class "columns" ]
                [ viewMembersSub model.now model.members_sub model.node_focus ]
            , div [ class "columns" ]
                [ viewGuest model.now model.members_top T.guest model.node_focus ]
            ]
        , div [ class "column is-1 section" ]
            [ div [ class "button is-primary", onClick (JoinOrgaMsg (JoinOrga.OnOpen model.node_focus.rootnameid JoinOrga.InviteOne)) ]
                [ A.icon1 "icon-user-plus" (upH T.inviteMember) ]
            ]
        ]


viewMembers : Time.Posix -> GqlData (List Member) -> NodeFocus -> Html Msg
viewMembers now data focus =
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
                    div [ class "section" ]
                        [ h2 [ class "subtitle has-text-weight-semibold" ] [ textH T.directMembers ]
                        , div [ class "table-containe" ]
                            -- @DEBUG: table-container with width=100%, do not work !
                            [ div [ class "table is-fullwidth" ]
                                [ thead []
                                    [ tr [ class "has-background-header" ]
                                        [ th [] []
                                        , th [] [ textH T.username ]
                                        , th [] [ textH T.name ]
                                        , th [ class "" ] [ textH T.roles ]
                                        ]
                                    ]
                                , tbody [] <|
                                    List.indexedMap
                                        (\i m ->
                                            tr []
                                                [ td [ class "pr-0" ] [ viewUser True m.username ]
                                                , td [ class "pt-3" ] [ a [ href (uriFromUsername UsersBaseUri m.username) ] [ "@" ++ m.username |> text ] ]
                                                , td [ class "pt-3" ] [ m.name |> withDefault "--" |> text ]
                                                , td [ class "pt-3" ] [ viewMemberRoles now OverviewBaseUri m.roles ]
                                                ]
                                        )
                                        mbs
                                ]
                            ]
                        ]

        Failure err ->
            viewGqlErrors err

        LoadingSlowly ->
            div [ class "spinner" ] []

        _ ->
            text ""


viewMembersSub : Time.Posix -> GqlData (List Member) -> NodeFocus -> Html Msg
viewMembersSub now data focus =
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
                    div [ class "section" ]
                        [ h2 [ class "subtitle has-text-weight-semibold" ] [ textH T.subMembers ]
                        , div [ class "table is-fullwidth" ]
                            [ thead []
                                [ tr [ class "has-background-header" ]
                                    [ th [] []
                                    , th [] [ textH T.username ]
                                    , th [] [ textH T.name ]
                                    , th [ class "" ] [ textH T.roles ]
                                    ]
                                ]
                            , tbody [] <|
                                List.indexedMap
                                    (\i m ->
                                        tr []
                                            [ td [ class "pr-0" ] [ viewUser True m.username ]
                                            , td [ class "pt-3" ] [ a [ href (uriFromUsername UsersBaseUri m.username) ] [ "@" ++ m.username |> text ] ]
                                            , td [ class "pt-3" ] [ m.name |> withDefault "--" |> text ]
                                            , td [ class "pt-3" ] [ viewMemberRoles now OverviewBaseUri m.roles ]
                                            ]
                                    )
                                    mbs
                            ]
                        ]

        Failure err ->
            viewGqlErrors err

        LoadingSlowly ->
            div [ class "spinner" ] []

        _ ->
            text ""


viewGuest : Time.Posix -> GqlData (List Member) -> String -> NodeFocus -> Html Msg
viewGuest now members_d title focus =
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
        div [ class "section" ]
            [ h2 [ class "subtitle has-text-weight-semibold" ] [ text title ]
            , div [ class "table is-fullwidth" ]
                [ thead []
                    [ tr [ class "has-background-header" ]
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


viewMemberRoles : Time.Posix -> FractalBaseRoute -> List UserRoleExtended -> Html Msg
viewMemberRoles now baseUri roles =
    div [ class "buttons" ] <|
        List.map
            (\r -> viewMemberRole now baseUri r)
            roles
