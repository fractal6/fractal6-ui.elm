module Components.Org.Members exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Array
import Browser.Events exposing (onKeyDown)
import Browser.Navigation as Nav
import Components.Fa as Fa
import Components.HelperBar as HelperBar
import Components.Loading as Loading exposing (WebData, viewAuthNeeded, viewGqlErrors, viewWarnings)
import Components.Text as Text exposing (..)
import Date exposing (formatTime)
import Dict exposing (Dict)
import Extra exposing (ternary, withDefaultData, withMaybeData)
import Extra.Events exposing (onClickPD, onEnter, onKeydown, onTab)
import Form
import Form.NewCircle
import Form.NewTension
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Global exposing (Msg(..))
import Html exposing (Html, a, br, button, datalist, div, h1, h2, hr, i, input, li, nav, option, p, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, placeholder, rows, target, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Requests exposing (fetchMembers)
import ModelCommon.Uri exposing (Flags_, FractalBaseRoute(..), NodeFocus, basePathChanged, focusFromNameid, focusState, nameidFromFlags, uriFromNameid, uriFromUsername)
import ModelCommon.View exposing (mediaTension, roleColor)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.AddNode exposing (addNewMember)
import Query.QueryNode exposing (queryLocalGraph, queryMembers)
import RemoteData exposing (RemoteData)
import Task
import Time


page : Page Flags Model Msg
page =
    Page.component
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



--
-- Model
--


type alias Model =
    { node_focus : NodeFocus
    , path_data : GqlData LocalGraph
    , children : WebData (List NodeId)
    , members_top : GqlData (List Member)
    , members_sub : GqlData (List Member)
    , node_action : ActionState
    , isModalActive : Bool -- Only use by JoinOrga for now. (other actions rely on Bulma drivers)
    }



--
-- Msg
--


type Msg
    = PassedSlowLoadTreshold -- timer
    | Submit (Time.Posix -> Msg) -- Get Current Time
      -- Data Queries
    | GotPath (GqlData LocalGraph) -- GraphQL
    | GotPath2 (GqlData LocalGraph) -- GraphQL
    | GotMembersTop (GqlData (List Member)) -- GraphQL
    | GotMembersSub (GqlData (List Member)) -- GraphQl
      -- Page Action
      -- JoinOrga Action
    | DoJoinOrga String Time.Posix
    | JoinAck (GqlData Node)
      -- JS Interop
    | DoCloseModal String -- ports receive / Close modal
    | DoOpenModal -- ports receive / Open  modal



--
-- INIT
--


type alias Flags =
    Flags_


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        -- Focus
        newFocus =
            flags
                |> nameidFromFlags
                |> focusFromNameid

        -- What has changed
        fs =
            focusState TensionsBaseUri global.session.referer global.session.node_focus newFocus

        model =
            { node_focus = newFocus
            , path_data =
                global.session.path_data
                    |> Maybe.map (\x -> Success x)
                    |> withDefault Loading
            , children = RemoteData.Loading
            , members_top = Loading
            , members_sub = Loading
            , node_action = NoOp
            , isModalActive = False
            }

        cmds =
            [ ternary fs.focusChange (queryLocalGraph newFocus.nameid GotPath) Cmd.none
            , queryMembers newFocus.nameid GotMembersTop
            , fetchMembers newFocus.nameid GotMembersSub
            , Global.sendSleep PassedSlowLoadTreshold 500
            ]
    in
    ( model
    , Cmd.batch cmds
    , Global.send (UpdateSessionFocus (Just newFocus))
    )


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global msg model =
    case msg of
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
                            ( newModel, Cmd.none, Global.send (UpdateSessionPath (Just path)) )

                        Nothing ->
                            let
                                nameid =
                                    List.head path.path |> Maybe.map (\p -> p.nameid) |> withDefault ""
                            in
                            ( newModel, queryLocalGraph nameid GotPath2, Cmd.none )

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
                                    ( { model | path_data = Success newPath }, Cmd.none, Global.send (UpdateSessionPath (Just newPath)) )

                                Nothing ->
                                    let
                                        nameid =
                                            List.head path.path |> Maybe.map (\p -> p.nameid) |> withDefault ""

                                        newPath =
                                            { prevPath | path = path.path ++ (List.tail prevPath.path |> withDefault []) }
                                    in
                                    ( { model | path_data = Success newPath }, queryLocalGraph nameid GotPath2, Cmd.none )

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
        DoJoinOrga rootnameid time ->
            case global.session.user of
                LoggedOut ->
                    ( { model | node_action = ActionAuthNeeded }, Global.send DoOpenModal, Cmd.none )

                LoggedIn uctx ->
                    let
                        form =
                            { uctx = uctx
                            , rootnameid = rootnameid
                            , id = model.path_data |> withMaybeData |> Maybe.map (\pd -> pd.root |> Maybe.map (\r -> r.id) |> withDefault "")
                            , post = Dict.fromList [ ( "createdAt", fromTime time ) ]
                            }

                        newModel =
                            { model | node_action = JoinOrga (JoinInit form) }
                    in
                    ( newModel, Cmd.batch [ addNewMember form JoinAck, Global.send DoOpenModal ], Cmd.none )

        JoinAck result ->
            case model.node_action of
                JoinOrga (JoinInit form) ->
                    case result of
                        Success n ->
                            ( { model | node_action = JoinOrga (JoinValidation form result) }
                            , Cmd.none
                            , Cmd.batch [ Global.send UpdateUserToken ]
                            )

                        other ->
                            ( { model | node_action = JoinOrga (JoinValidation form result) }, Cmd.none, Cmd.none )

                default ->
                    ( model, Cmd.none, Cmd.none )

        -- Modal
        DoOpenModal ->
            ( { model | isModalActive = True }, Cmd.none, Ports.open_modal )

        DoCloseModal _ ->
            ( { model | isModalActive = False }, Cmd.none, Cmd.none )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions global model =
    Sub.batch
        [ Ports.closeModalFromJs DoCloseModal ]



---- VIEW ----


view : Global.Model -> Model -> Document Msg
view global model =
    { title = "Members · " ++ (String.join "/" <| LE.unique [ model.node_focus.rootnameid, model.node_focus.nameid |> String.split "#" |> List.reverse |> List.head |> withDefault "" ])
    , body = [ view_ global model ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    div [ id "mainPane" ]
        [ HelperBar.view MembersBaseUri
            global.session.user
            global.session.path_data
            (Submit <| DoJoinOrga model.node_focus.rootnameid)
        , div [ class "columns is-centered" ]
            [ div [ class "column is-10-desktop is-10-widescreen is-9-fullhd" ]
                [ div [ class "columns" ]
                    [ viewMembers model.members_top "Direct members" model.node_focus ]
                , div [ class "columns" ]
                    [ viewMembers model.members_sub "Sub-Circle members" model.node_focus ]
                , div [ class "columns" ]
                    [ viewGuest model.members_top "Guest" model.node_focus ]
                ]
            ]
        , setupActionModal model.isModalActive model.node_action
        ]


viewMembers : GqlData (List Member) -> String -> NodeFocus -> Html Msg
viewMembers members_d title focus =
    div [ id "membersTable", class "section" ]
        [ h2 [ class "subtitle has-text-weight-semibold" ] [ text title ]
        , div [ class "table is-fullwidth" ]
            [ thead []
                [ tr []
                    [ th [] [ text "Username" ]
                    , th [] [ text "Name" ]
                    , th [ class "" ] [ text "Roles" ]
                    ]
                ]
            , tbody [] <|
                case members_d of
                    Success members ->
                        List.indexedMap
                            (\i m ->
                                let
                                    roles =
                                        memberRolesFilter focus m.roles
                                in
                                if List.length roles == 0 then
                                    tr [] []

                                else
                                    tr []
                                        [ td [] [ a [ href (uriFromUsername UsersBaseUri m.username) ] [ "@" ++ m.username |> text ] ]
                                        , td [] [ m.name |> withDefault "--" |> text ]
                                        , td [] [ viewMemberRoles OverviewBaseUri roles ]
                                        ]
                            )
                            members

                    Failure err ->
                        [ viewGqlErrors err ]

                    LoadingSlowly ->
                        [ div [ class "spinner" ] [] ]

                    other ->
                        []
            ]
        ]


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
                        [ th [] [ text "Username" ]
                        , th [] [ text "Name" ]
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
                if r.role_type == RoleType.Guest || r.role_type == RoleType.Member then
                    []

                else if focus.nameid == (r.parent |> Maybe.map (\p -> p.nameid) |> withDefault "") then
                    -- Dont include top level member for sub circle member (which contains all member)
                    -- Note: .parentis not defined in the top member query
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
                    [ class ("button buttonRole is-small has-text-weight-semiboldtooltip has-tooltip-bottom is-" ++ roleColor r.role_type)
                    , attribute "data-tooltip" ([ r.name, "of", getParentFragmentFromRole r, "since the", formatTime r.createdAt ] |> String.join " ")
                    , href <| uriFromNameid baseUri r.nameid
                    ]
                    [ text r.name ]
            )
            roles



-- Actions


setupActionModal : Bool -> ActionState -> Html Msg
setupActionModal isModalActive action =
    div
        [ id "actionModal"
        , classList
            [ ( "modal", True )
            , ( "modal-fx-fadeIn", True )
            , ( "is-active", isModalActive )
            , ( "protected_", isModalActive )
            ]
        ]
        [ div
            [ classList
                [ ( "modal-background", True )
                , ( "protected_", isModalActive )
                ]
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
                    viewAuthNeeded

                other ->
                    div [] [ text "Action not implemented." ]
            ]
        , button
            [ classList
                [ ( "modal-close", True )
                , ( "is-large", True )
                , ( "protected_", isModalActive )
                ]
            ]
            []
        ]


viewJoinOrgaStep : JoinStep JoinOrgaForm -> Html Msg
viewJoinOrgaStep step =
    case step of
        JoinInit _ ->
            div [ class "box spinner" ] [ text Text.loading ]

        JoinNotAuthorized errMsg ->
            viewGqlErrors errMsg

        JoinValidation form result ->
            case result of
                Success _ ->
                    div [ class "box is-light modalClose", onClick (DoCloseModal "") ]
                        [ Fa.icon0 "fas fa-check fa-2x has-text-success" " "
                        , text "Welcome in "
                        , span [ class "has-font-weight-semibold" ] [ (form.rootnameid |> String.split "#" |> List.head |> withDefault "Unknonwn") |> text ]
                        ]

                Failure err ->
                    viewGqlErrors err

                default ->
                    div [ class "box spinner" ] [ text Text.loading ]
