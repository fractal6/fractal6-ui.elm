module Components.Org.Tension exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Components.Fa as Fa
import Components.HelperBar as HelperBar
import Components.Loading as Loading exposing (viewAuthNeeded, viewGqlErrors, viewHttpErrors, viewWarnings)
import Components.Markdown exposing (renderMarkdown)
import Components.Text as Text exposing (..)
import Date exposing (formatTime)
import Dict exposing (Dict)
import Extra exposing (ternary, withMaybeData)
import Extra.Events exposing (onClickPD, onClickPD2)
import Form exposing (isPostSendable)
import Form.NewCircle exposing (NewNodeText)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Global exposing (Msg(..))
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, li, nav, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, placeholder, readonly, rows, target, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Iso8601 exposing (fromTime)
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Uri exposing (FractalBaseRoute(..), NodeFocus, focusState)
import ModelCommon.View
    exposing
        ( getAvatar
        , statusColor
        , tensionTypeColor
        , tensionTypeSpan
        , viewLabels
        , viewTensionArrowB
        , viewTensionDateAndUser
        , viewTensionDateAndUserC
        , viewUsernameLink
        )
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.AddNode exposing (addNewMember)
import Query.PatchTension exposing (pushTensionComment)
import Query.QueryNodes exposing (queryLocalGraph)
import Query.QueryTension exposing (getTension)
import Task
import Time



---- PROGRAM ----


type alias Flags =
    { param1 : String
    , param2 : String
    }


page : Page Flags Model Msg
page =
    Page.component
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



---- MODEL ----


type alias Model =
    { -- Focus
      node_focus : NodeFocus
    , path_data : GqlData LocalGraph

    -- Common
    , node_action : ActionState
    , isModalActive : Bool -- Only use by JoinOrga for now. (other actions rely on Bulma drivers)
    , inputViewMode : InputViewMode

    -- Page
    , tensionid : String
    , tension_data : GqlData TensionExtended
    , tension_form : TensionPatchForm
    , tension_result : GqlData IdPayload
    }



---- MSG ----


type Msg
    = PassedSlowLoadTreshold -- timer
    | Submit (Time.Posix -> Msg) -- Get Current Time
      -- Gql Data Queries
    | GotPath (GqlData LocalGraph) -- GraphQL
    | GotPath2 (GqlData LocalGraph) -- GraphQL
    | GotTension (GqlData TensionExtended)
      -- Page Action
    | ChangeTensionPost String String -- {field value}
    | SubmitTensionPatch TensionPatchForm Time.Posix -- Send form
    | SubmitChangeStatus TensionPatchForm TensionStatus.TensionStatus Time.Posix -- Send form
    | TensionPatchAck (GqlData IdPayload)
    | ChangeInputViewMode InputViewMode
      -- JoinOrga Action
    | DoJoinOrga String Time.Posix
    | JoinAck (GqlData Node)
      -- JS Interop
    | DoCloseModal String -- ports receive / Close modal
    | DoOpenModal -- ports receive / Open  modal



---- INIT ----


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
            initTensionForm flags.param2 global.session.user

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
            , inputViewMode = Write
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



--- UPDATE ----


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

        SubmitTensionPatch form time ->
            let
                newForm =
                    { form
                        | post = Dict.insert "createdAt" (fromTime time) form.post
                        , emitter = model.tension_data |> withMaybeData |> Maybe.map (\t -> t.emitter)
                        , receiver = model.tension_data |> withMaybeData |> Maybe.map (\t -> t.receiver)
                    }
            in
            ( model, pushTensionComment newForm TensionPatchAck, Cmd.none )

        SubmitChangeStatus form status time ->
            let
                newForm =
                    { form
                        | post = Dict.insert "message_action" ("$action$ status " ++ TensionStatus.toString status) form.post
                        , status = Just status
                    }
            in
            ( { model | tension_form = newForm }, Global.send (SubmitTensionPatch newForm time), Cmd.none )

        TensionPatchAck result ->
            case result of
                Success _ ->
                    let
                        newComments =
                            commentsFromForm model.tension_form

                        tension_d =
                            case model.tension_data of
                                Success t ->
                                    let
                                        newTension =
                                            { t
                                                | comments = Just ((t.comments |> withDefault []) ++ newComments)
                                                , status = model.tension_form.status |> withDefault t.status
                                            }
                                    in
                                    Success newTension

                                other ->
                                    other

                        resetForm =
                            initTensionForm model.tensionid global.session.user
                    in
                    ( { model | tension_data = tension_d, tension_form = resetForm, tension_result = result }, Cmd.none, Cmd.none )

                Failure _ ->
                    let
                        f =
                            model.tension_form

                        resetForm =
                            { f
                                | post = Dict.remove "message_action" f.post
                                , status = Nothing
                            }
                    in
                    ( { model | tension_result = result, tension_form = resetForm }, Cmd.none, Cmd.none )

                _ ->
                    ( { model | tension_result = result }, Cmd.none, Cmd.none )

        ChangeInputViewMode viewMode ->
            ( { model | inputViewMode = viewMode }, Cmd.none, Cmd.none )

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
            [ div [ class "column is-11-desktop is-11-widescreen is-10-fullhd is-offset-1" ]
                [ case model.tension_data of
                    Success t ->
                        viewTension global.session.user t model

                    Failure err ->
                        viewGqlErrors err

                    LoadingSlowly ->
                        div [ class "spinner" ] []

                    other ->
                        div [] []
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
                    viewCommentInput uctx t model.tension_form model.tension_result model.inputViewMode

                LoggedOut ->
                    viewJoinNeeded model.node_focus
    in
    div [ id "tensionPage" ]
        [ div [ class "columns" ]
            [ div [ class "column is-8" ]
                [ h1 [ class "title tensionTitle" ] [ text t.title ]
                , div [ class "tensionSubtitle" ]
                    [ span [ class ("tag is-rounded  is-" ++ statusColor t.status) ]
                        [ t.status |> TensionStatus.toString |> text ]
                    , span [ class "tag is-rounded is-light" ] [ div [ class <| "Circle " ++ tensionTypeColor "text" t.type_ ] [ text "\u{00A0}" ], t.type_ |> TensionType.toString |> text ]
                    , viewTensionDateAndUser t.createdAt t.createdBy
                    , viewTensionArrowB "has-text-weight-light is-pulled-right" t.emitter t.receiver
                    ]
                , hr [ class "has-background-grey-dark" ] []
                ]
            ]
        , div [ class "columns" ]
            [ div [ class "column is-8 tensionComments" ]
                [ subComments
                    |> List.append [ viewComment (Comment t.createdAt t.createdBy (t.message |> withDefault "")) ]
                    |> div []
                , hr [ class "has-background-grey is-3" ] []
                , userInput
                ]
            , div [ class "column tensionSidePane" ]
                [ viewSidePane t ]
            ]
        ]


viewComment : Comment -> Html Msg
viewComment c =
    let
        msg =
            if String.left (String.length "$action$") c.message == "$action$" then
                let
                    values =
                        String.split " " c.message
                in
                if List.length values == 3 then
                    if List.member "status" values then
                        values |> List.reverse |> List.head |> Maybe.map (\s -> TensionStatus.fromString s) |> withDefault Nothing

                    else
                        Nothing

                else
                    Nothing

            else
                Nothing
    in
    case msg of
        Just status ->
            -- TensionAction
            --  Assume status for nom
            let
                action =
                    case status of
                        TensionStatus.Open ->
                            "reopened"

                        TensionStatus.Closed ->
                            "closed"
            in
            div [ class "media section is-paddingless actionComment" ]
                [ div [ class "media-left" ] [ Fa.icon0 ("fas fa-ban fa-2x" ++ " has-text-" ++ statusColor status) "" ]
                , div [ class "media-content" ]
                    [ div [ class "is-italic" ] [ viewUsernameLink c.createdBy.username, text " ", text action, text " the ", text (formatTime c.createdAt) ]
                    ]
                ]

        Nothing ->
            div [ class "media section is-paddingless" ]
                [ div [ class "media-left" ] [ div [ class "image is-48x48 circleBase circle1" ] [ getAvatar c.createdBy.username ] ]
                , div [ class "media-content" ]
                    [ div [ class "message" ]
                        [ div [ class "message-header" ]
                            [ viewTensionDateAndUserC c.createdAt c.createdBy ]
                        , div [ class "message-body" ]
                            [ case c.message of
                                "" ->
                                    div [ class "is-italic" ] [ text "No description provided." ]

                                message ->
                                    renderMarkdown message "is-light"
                            ]
                        ]
                    ]
                ]


viewCommentInput : UserCtx -> TensionExtended -> TensionPatchForm -> GqlData IdPayload -> InputViewMode -> Html Msg
viewCommentInput uctx tension form result viewMode =
    let
        isLoading =
            result == LoadingSlowly

        isSendable =
            isPostSendable [ "message" ] form.post

        submitComment =
            ternary isSendable [ onClick (Submit <| SubmitTensionPatch form) ] []

        submitCloseOpenTension =
            case tension.status of
                TensionStatus.Open ->
                    [ onClick (Submit <| SubmitChangeStatus form TensionStatus.Closed) ]

                TensionStatus.Closed ->
                    [ onClick (Submit <| SubmitChangeStatus form TensionStatus.Open) ]

        closeOpenText =
            case tension.status of
                TensionStatus.Open ->
                    if (Dict.get "message" form.post |> withDefault "") /= "" then
                        "Close and comment"

                    else
                        "Close tension"

                TensionStatus.Closed ->
                    if (Dict.get "message" form.post |> withDefault "") /= "" then
                        "Reopen and comment"

                    else
                        "Reopen tension"

        message =
            Dict.get "message" form.post |> withDefault ""
    in
    div [ class "media section is-paddingless tensionCommentInput" ]
        [ div [ class "media-left" ] [ div [ class "image is-48x48 circleBase circle1" ] [ getAvatar uctx.username ] ]
        , div [ class "media-content" ]
            [ div [ class "message" ]
                [ div [ class "message-header" ]
                    [ div [ class "tabs is-boxed is-small" ]
                        [ ul []
                            [ li [ classList [ ( "is-active", viewMode == Write ) ] ] [ a [ onClickPD2 (ChangeInputViewMode Write), target "_blank" ] [ text "Write" ] ]
                            , li [ classList [ ( "is-active", viewMode == Preview ) ] ] [ a [ onClickPD2 (ChangeInputViewMode Preview), target "_blank" ] [ text "Preview" ] ]
                            ]
                        ]
                    ]
                , div [ class "message-body" ]
                    [ div [ class "field" ]
                        [ div [ class "control" ]
                            [ case viewMode of
                                Write ->
                                    textarea
                                        [ id "textAreaModal"
                                        , class "textarea"
                                        , rows 5
                                        , placeholder "Leave a comment"
                                        , value message
                                        , onInput (ChangeTensionPost "message")
                                        ]
                                        []

                                Preview ->
                                    div [] [ renderMarkdown message "is-light", hr [] [] ]
                            ]
                        ]
                    , case result of
                        Failure err ->
                            viewGqlErrors err

                        _ ->
                            div [] []
                    , div [ class "field is-grouped is-grouped-right" ]
                        [ div [ class "control" ]
                            [ div [ class "buttons" ]
                                [ button
                                    ([ class "button has-text-weight-semibold"
                                     , classList
                                        [ ( "is-danger", tension.status == TensionStatus.Open )
                                        , ( "is-loading", isLoading )
                                        ]
                                     ]
                                        ++ submitCloseOpenTension
                                    )
                                    [ text closeOpenText ]
                                , button
                                    ([ class "button has-text-weight-semibold"
                                     , classList [ ( "is-success", isSendable ), ( "is-loading", isLoading ) ]
                                     , disabled (not isSendable)
                                     ]
                                        ++ submitComment
                                    )
                                    [ text "Comment" ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewSidePane : TensionExtended -> Html Msg
viewSidePane t =
    let
        labels_m =
            t.labels |> Maybe.map (\ls -> ternary (List.length ls == 0) Nothing (Just ls)) |> withDefault Nothing
    in
    div []
        [ div [ class "media" ]
            [ div [ class "media-content" ]
                [ h2 [ class "subtitle" ] [ text "Labels" ]
                , case labels_m of
                    Just labels ->
                        viewLabels labels

                    Nothing ->
                        div [ class "is-italic" ] [ text "no labels yet" ]
                ]
            ]
        , div [ class "media" ]
            [ div [ class "media-content" ]
                [ h2 [ class "subtitle" ] [ text "Mandate" ]
                , case t.mandate of
                    Just m ->
                        case t.action of
                            Just a ->
                                viewMandate a m

                            Nothing ->
                                div [ class "is-italic" ] [ text "no mandate attached" ]

                    Nothing ->
                        div [ class "is-italic" ] [ text "no mandate attached" ]
                ]
            ]
        ]


viewMandate : TensionAction.TensionAction -> Mandate -> Html Msg
viewMandate action mandate =
    let
        txt =
            case action of
                TensionAction.NewCircle ->
                    NewNodeText Text.newCircle Text.tensionCircleAdded Text.circleNameHelp Text.circleMessageHelp Text.phCirclePurpose Text.phCircleResponsabilities Text.phCircleDomains Text.phCirclePolicies Text.tensionCircleSubmit Text.tensionCircleCloseSubmit Text.firstLinkCircleMessageHelp

                TensionAction.NewRole ->
                    NewNodeText Text.newRole Text.tensionRoleAdded Text.roleNameHelp Text.roleMessageHelp Text.phRolePurpose Text.phRoleResponsabilities Text.phRoleDomains Text.phRolePolicies Text.tensionRoleSubmit Text.tensionRoleCloseSubmit Text.firstLinkRoleMessageHelp

        --_ ->
        --    NewNodeText "" "" "" "" "" "" "" "" "" "" ""
    in
    div [ class "card" ]
        [ div [ class "cnard-header" ] [ div [ class "card-header-title" ] [ text Text.mandateH ] ]
        , div [ class "card-content" ]
            [ div [ class "field" ]
                [ div [ class "label" ] [ text Text.purposeH ]
                , div [ class "control" ]
                    [ textarea
                        [ id "textAreaModal"
                        , class "textarea"
                        , rows 5
                        , readonly True
                        , value mandate.purpose

                        --, placeholder (txt.ph_purpose ++ "*")
                        --, onInput <| changePostMsg "purpose"
                        ]
                        []
                    ]
                ]
            , div [ class "field" ]
                [ div [ class "label" ] [ text Text.responsabilitiesH ]
                , div [ class "control" ]
                    [ textarea
                        [ id "textAreaModal"
                        , class "textarea"
                        , rows 5
                        , readonly True
                        , value (mandate.responsabilities |> withDefault ("<" ++ Text.noResponsabilities ++ ">"))

                        --, placeholder txt.ph_responsabilities
                        --, onInput <| changePostMsg "responsabilities"
                        ]
                        []
                    ]
                ]
            , div [ class "field" ]
                [ div [ class "label" ] [ text Text.domainsH ]
                , div [ class "control" ]
                    [ textarea
                        [ id "textAreaModal"
                        , class "textarea"
                        , rows 5
                        , readonly True
                        , value (mandate.domains |> withDefault ("<" ++ Text.noDomains ++ ">"))

                        --, placeholder txt.ph_domains
                        --, onInput <| changePostMsg "domains"
                        ]
                        []
                    ]
                ]
            , div [ class "field" ]
                [ div [ class "label" ] [ text Text.policiesH ]
                , div [ class "control" ]
                    [ textarea
                        [ id "textAreaModal"
                        , class "textarea"
                        , rows 5
                        , readonly True
                        , value (mandate.policies |> withDefault ("<" ++ Text.noPolicies ++ ">"))

                        --, placeholder txt.ph_policies
                        --, onInput <| changePostMsg "policies"
                        ]
                        []
                    ]
                ]
            ]
        ]


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
                    div [ class "box is-light modalClose", onClick (DoCloseModal "") ]
                        [ Fa.icon0 "fas fa-check fa-2x has-text-success" " ", "Welcome in " ++ (form.rootnameid |> String.split "#" |> List.head |> withDefault "Unknonwn") |> text ]

                Failure err ->
                    viewGqlErrors err

                default ->
                    div [ class "box spinner" ] [ text Text.loading ]



--
-- Utils
--


commentsFromForm : TensionPatchForm -> List Comment
commentsFromForm form =
    let
        createdAt =
            Dict.get "createdAt" form.post |> withDefault ""

        createdBy =
            Username form.uctx.username
    in
    [ Dict.get "message" form.post
        |> Maybe.map
            (\comment ->
                { createdAt = createdAt
                , createdBy = createdBy
                , message = comment
                }
            )
    , Dict.get "message_action" form.post
        |> Maybe.map
            (\message_action ->
                { createdAt = createdAt
                , createdBy = createdBy
                , message = message_action
                }
            )
    ]
        |> List.filterMap identity


initTensionForm : String -> UserState -> TensionPatchForm
initTensionForm tensionid user =
    { uctx =
        case user of
            LoggedIn uctx ->
                uctx

            LoggedOut ->
                UserCtx "" Nothing (UserRights False False) []
    , id = tensionid
    , status = Nothing
    , emitter = Nothing
    , receiver = Nothing
    , post = Dict.empty
    }
