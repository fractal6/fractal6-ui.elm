module Components.Org.Tension exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Components.Fa as Fa
import Components.HelperBar as HelperBar
import Components.Loading as Loading exposing (viewAuthNeeded, viewGqlErrors, viewHttpErrors, viewWarnings)
import Components.Text as Text exposing (..)
import Date exposing (formatTime)
import Dict exposing (Dict)
import Extra exposing (ternary, withMaybeData)
import Form exposing (isPostSendable)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Global exposing (Msg(..))
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, li, nav, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, placeholder, rows, type_)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Iso8601 exposing (fromTime)
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Uri exposing (FractalBaseRoute(..), NodeFocus, focusState)
import ModelCommon.View exposing (getAvatar, statusColor, tensionTypeColor, tensionTypeSpan, viewTensionArrowB, viewTensionDateAndUser, viewTensionDateAndUserC)
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.AddNode exposing (addNewMember)
import Query.PatchTension exposing (pushTensionComment)
import Query.QueryNodes exposing (queryLocalGraph)
import Query.QueryTension exposing (getTension)
import Task
import Time


type alias Flags =
    { param1 : String
    , param2 : String
    }



--
-- Model
--


type alias Model =
    { -- Focus
      node_focus : NodeFocus
    , path_data : GqlData LocalGraph

    -- Common
    , node_action : ActionState
    , isModalActive : Bool -- Only use by JoinOrga for now. (other actions rely on Bulma drivers)

    -- Page
    , tensionid : String
    , tension_data : GqlData TensionExtended
    , tension_form : TensionPatchForm
    , tension_result : GqlData IdPayload
    }



--
-- Msg
--


type Msg
    = PassedSlowLoadTreshold -- timer
    | Submit (Time.Posix -> Msg) -- Get Current Time
      -- Gql Data Queries
    | GotPath (GqlData LocalGraph) -- GraphQL
    | GotPath2 (GqlData LocalGraph) -- GraphQL
    | GotTension (GqlData TensionExtended)
      -- Page Action
    | ChangeTensionPost String String -- {field value}
    | SubmitComment TensionPatchForm Time.Posix -- Send form
    | CommentAck (GqlData IdPayload)
      -- JoinOrga Action
    | DoJoinOrga String Time.Posix
    | JoinAck (GqlData Node)
      -- JS Interop
    | DoCloseModal String -- ports receive / Close modal
    | DoOpenModal -- ports receive / Open  modal


page : Page Flags Model Msg
page =
    Page.component
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        -- Focus
        newFocus =
            NodeFocus flags.param1 True flags.param1 NodeType.Circle

        -- What has changed
        fs =
            focusState TensionsBaseUri global.session.referer global.session.node_focus newFocus

        -- Init form
        tensionForm =
            { uctx =
                case global.session.user of
                    LoggedIn uctx ->
                        uctx

                    LoggedOut ->
                        UserCtx "" Nothing (UserRights False False) []
            , id = flags.param2
            , status = Nothing
            , emitter = Nothing
            , receiver = Nothing
            , post = Dict.empty
            }

        model =
            { node_focus = newFocus
            , path_data =
                global.session.path_data
                    |> Maybe.map (\x -> Success x)
                    |> withDefault Loading
            , tensionid = flags.param2
            , tension_data = Loading
            , tension_form = tensionForm
            , tension_result = NotAsked
            , node_action = NoOp
            , isModalActive = False
            }

        cmds =
            [ ternary fs.focusChange (queryLocalGraph newFocus.nameid GotPath) Cmd.none
            , getTension model.tensionid GotTension
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
                tension =
                    ternary (model.tension_data == Loading) LoadingSlowly model.tension_data
            in
            ( { model | tension_data = tension }, Cmd.none, Cmd.none )

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

        GotTension result ->
            let
                newModel =
                    { model | tension_data = result }
            in
            ( newModel, Cmd.none, Cmd.none )

        -- Page Action
        ChangeTensionPost field value ->
            let
                form =
                    model.tension_form

                newForm =
                    { form | post = Dict.insert field value form.post }
            in
            ( { model | tension_form = newForm }, Cmd.none, Cmd.none )

        SubmitComment form time ->
            let
                newForm =
                    { form
                        | post = Dict.insert "createdAt" (fromTime time) form.post
                        , emitter = model.tension_data |> withMaybeData |> Maybe.map (\t -> t.emitter)
                        , receiver = model.tension_data |> withMaybeData |> Maybe.map (\t -> t.receiver)
                    }
            in
            ( model, pushTensionComment newForm CommentAck, Cmd.none )

        CommentAck result ->
            case result of
                Success _ ->
                    let
                        newComment =
                            commentFromForm model.tension_form

                        tension_d =
                            case model.tension_data of
                                Success t ->
                                    let
                                        newTension =
                                            { t | comments = Just ((t.comments |> withDefault []) ++ [ newComment ]) }
                                    in
                                    Success newTension

                                other ->
                                    other
                    in
                    ( { model | tension_result = result, tension_data = tension_d }, Cmd.none, Cmd.none )

                other ->
                    ( { model | tension_result = result }, Cmd.none, Cmd.none )

        -- Join
        DoJoinOrga rootnameid time ->
            case global.session.user of
                LoggedOut ->
                    ( { model | node_action = JoinOrga JoinAuthNeeded }, Global.send DoOpenModal, Cmd.none )

                LoggedIn uctx ->
                    let
                        form =
                            { uctx = uctx
                            , rootnameid = rootnameid
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


view : Global.Model -> Model -> Document Msg
view global model =
    { title =
        case model.tension_data of
            Success t ->
                t.title

            _ ->
                "Loading tension"
    , body = [ view_ global model ]
    }


view_ : Global.Model -> Model -> Html Msg
view_ global model =
    div [ id "mainPane" ]
        [ HelperBar.view TensionsBaseUri
            global.session.user
            global.session.path_data
            (Submit <| DoJoinOrga model.node_focus.rootnameid)
        , div [ class "columns is-centered" ]
            [ div [ class "column is-10-desktop is-10-widescreen is-9-fullhd" ]
                [ div [ class "columns" ] <|
                    case model.tension_data of
                        Success t ->
                            [ div [ class "column is-two-thirds" ] [ viewTension global.session.user t model ]
                            , div [ class "column" ] [ viewSidePane t ]
                            ]

                        Failure err ->
                            [ viewGqlErrors err ]

                        LoadingSlowly ->
                            [ div [ class "spinner" ] [] ]

                        other ->
                            []
                ]
            ]
        , setupActionModal model.isModalActive model.node_action
        ]


viewTension : UserState -> TensionExtended -> Model -> Html Msg
viewTension u t model =
    let
        subComments =
            t.comments
                |> withDefault []
                |> List.map (\c -> viewComment c)

        userInput =
            case u of
                LoggedIn uctx ->
                    [ viewCommentInput uctx model.tension_form model.tension_result ]

                LoggedOut ->
                    [ viewJoinNeeded model.node_focus ]
    in
    div [ id "tensionPage" ] <|
        ([ h1 [ class "title tensionTitle" ] [ text t.title ]
         , div [ class "tensionSubtitle" ]
            [ span [ class "tag is-light" ] [ div [ class <| "Circle " ++ tensionTypeColor "text" t.type_ ] [ text "\u{00A0}" ], t.type_ |> TensionType.toString |> text ]
            , span [ class ("tag  is-" ++ statusColor t.status) ]
                [ t.status |> TensionStatus.toString |> text ]
            , viewTensionDateAndUser t.createdAt t.createdBy
            , viewTensionArrowB "has-text-weight-light is-pulled-right" t.emitter t.receiver
            ]
         , hr [ class "has-background-grey-dark" ] []
         , viewComment (Comment t.createdAt t.createdBy (t.message |> withDefault ""))
         ]
            ++ subComments
            ++ [ hr [ class "has-background-grey is-3" ] [] ]
            ++ userInput
        )


viewComment : Comment -> Html Msg
viewComment c =
    div [ class "tensionComment media section is-paddingless" ]
        [ div [ class "media-left" ] [ div [ class "image is-48x48" ] [ getAvatar c.createdBy.username ] ]
        , div [ class "media-content" ]
            [ div [ class "message" ]
                [ div [ class "message-header" ]
                    [ viewTensionDateAndUserC c.createdAt c.createdBy ]
                , div [ class "message-body" ]
                    [ case c.message of
                        "" ->
                            div [ class "is-italic" ] [ text "No description provided." ]

                        message ->
                            div [ class "" ] [ text message ]
                    ]
                ]
            ]
        ]


viewCommentInput : UserCtx -> TensionPatchForm -> GqlData IdPayload -> Html Msg
viewCommentInput uctx form result =
    let
        isSendable =
            isPostSendable [ "message" ] form.post

        isLoading =
            result == LoadingSlowly
    in
    div [ class "tensionComment media section is-paddingless" ]
        [ div [ class "media-left" ] [ div [ class "image is-48x48" ] [ getAvatar uctx.username ] ]
        , div [ class "media-content" ]
            [ div [ class "message" ]
                [ div [ class "message-header" ] [ div [ class "tabs is-boxed is-small" ] [ ul [] [ li [ class "is-active" ] [ a [] [ text "Write" ] ], li [] [ a [] [ text "Preview" ] ] ] ] ]
                , div [ class "message-body" ]
                    [ div [ class "field" ]
                        [ div [ class "control" ]
                            [ textarea
                                [ id "textAreaModal"
                                , class "textarea"
                                , rows 5
                                , placeholder "Leave a comment"
                                , onInput (ChangeTensionPost "message")
                                ]
                                []
                            ]
                        ]
                    , case result of
                        Failure err ->
                            viewGqlErrors err

                        _ ->
                            div [] []
                    , div [ class "field is-grouped is-grouped-right" ]
                        [ div [ class "control" ]
                            [ if isSendable then
                                div [ class "buttons" ]
                                    [ button
                                        [ class "button is-success has-text-weight-semibold"
                                        , classList [ ( "is-loading", isLoading ) ]
                                        , onClick (Submit <| SubmitComment form)
                                        ]
                                        [ text "Submit new tension" ]
                                    ]

                              else
                                div [ class "buttons" ]
                                    [ button [ class "button has-text-weight-semibold", disabled True ]
                                        [ text "Submit new tension" ]
                                    ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewSidePane : TensionExtended -> Html Msg
viewSidePane t =
    div [] []


viewJoinNeeded : NodeFocus -> Html Msg
viewJoinNeeded focus =
    div [ class "box has-background-primary" ]
        [ p []
            [ button
                [ class "button is-small"
                , onClick (Submit <| DoJoinOrga focus.rootnameid)
                ]
                [ text "Join" ]
            , text " this organisation to participate to this conversation."
            ]
        ]



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

        JoinAuthNeeded ->
            viewAuthNeeded

        JoinNotAuthorized errMsg ->
            viewGqlErrors errMsg

        JoinValidation form result ->
            case result of
                Success _ ->
                    div [ class "box has-background-success" ] [ "Welcome in " ++ (form.rootnameid |> String.split "#" |> List.head |> withDefault "Unknonwn") |> text ]

                Failure err ->
                    viewGqlErrors err

                default ->
                    div [ class "box spinner" ] [ text Text.loading ]



--
-- Utils
--


commentFromForm : TensionPatchForm -> Comment
commentFromForm form =
    { createdAt = Dict.get "createdAt" form.post |> withDefault ""
    , createdBy = Username form.uctx.username
    , message = Dict.get "message" form.post |> withDefault ""
    }
