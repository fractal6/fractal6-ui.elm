module Components.Org.Tension exposing (Flags, Model, Msg, init, page, subscriptions, update, view)

import Components.Fa as Fa
import Components.HelperBar as HelperBar
import Components.Loading as Loading exposing (viewAuthNeeded, viewGqlErrors, viewHttpErrors, viewWarnings)
import Components.Markdown exposing (renderMarkdown)
import Components.Text as T
import Date exposing (formatTime)
import Dict exposing (Dict)
import Extra exposing (ternary, withMaybeData)
import Extra.Events exposing (onClickPD, onClickPD2)
import Form exposing (isPostSendable)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Global exposing (Msg(..), send)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, li, nav, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, placeholder, readonly, rows, target, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Uri exposing (FractalBaseRoute(..), NodeFocus, focusState)
import ModelCommon.View
    exposing
        ( getAvatar
        , getNodeTextFromAction
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
import Query.PatchTension exposing (patchComment, patchTitle, pushTensionComment)
import Query.QueryNode exposing (queryLocalGraph)
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
    , title_result : GqlData String
    , comment_form : CommentPatchForm
    , comment_result : GqlData Comment
    , isTitleEdit : Bool
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
    | DoUpdateComment String
    | ChangeCommentPost String String
    | CancelCommentPatch
    | SubmitCommentPatch CommentPatchForm Time.Posix
    | CommentPatchAck (GqlData Comment)
    | DoChangeTitle
    | CancelTitle
    | SubmitTitle TensionPatchForm Time.Posix
    | TitleAck (GqlData String)
      -- JoinOrga Action
    | DoJoinOrga String Time.Posix
    | JoinAck (GqlData Node)
      -- JS Interop
    | DoOpenModal -- ports receive / Open  modal
    | DoCloseModal String -- ports receive / Close modal
      -- Util
    | ChangeInputViewMode InputViewMode
    | ChangeUpdateViewMode InputViewMode



---- INIT ----


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        apis =
            global.session.apis

        -- Focus
        newFocus =
            NodeFocus flags.param1 True flags.param1 NodeType.Circle

        -- What has changed
        fs =
            focusState TensionsBaseUri global.session.referer global.session.node_focus newFocus

        model =
            { node_focus = newFocus
            , path_data =
                global.session.path_data
                    |> Maybe.map (\x -> Success x)
                    |> withDefault Loading
            , tensionid = flags.param2
            , tension_data = Loading
            , tension_form = initTensionForm flags.param2 global.session.user
            , tension_result = NotAsked
            , title_result = NotAsked
            , comment_form = initCommentPatchForm global.session.user
            , comment_result = NotAsked
            , inputViewMode = Write
            , node_action = NoOp
            , isModalActive = False
            , isTitleEdit = False
            }

        cmds =
            [ ternary fs.focusChange (queryLocalGraph apis.gql newFocus.nameid GotPath) Cmd.none
            , getTension apis.gql model.tensionid GotTension
            , Global.sendSleep PassedSlowLoadTreshold 500
            ]
    in
    ( model
    , Cmd.batch cmds
    , send (UpdateSessionFocus (Just newFocus))
    )



--- UPDATE ----


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global msg model =
    let
        apis =
            global.session.apis
    in
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

        GotTension result ->
            let
                newModel =
                    { model | tension_data = result }
            in
            ( newModel, Cmd.none, Ports.bulma_driver "" )

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
            ( model, pushTensionComment apis.gql newForm TensionPatchAck, Cmd.none )

        SubmitChangeStatus form status time ->
            let
                newForm =
                    { form
                        | post = Dict.insert "message_action" ("$action$ status " ++ TensionStatus.toString status) form.post
                        , status = Just status
                    }
            in
            ( { model | tension_form = newForm }, send (SubmitTensionPatch newForm time), Cmd.none )

        TensionPatchAck result ->
            case result of
                Success cid ->
                    let
                        newComments =
                            commentsFromForm cid.id model.tension_form

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
                    ( { model | tension_data = tension_d, tension_form = resetForm, tension_result = result }, Cmd.none, Ports.bulma_driver "" )

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

        DoUpdateComment id ->
            let
                form =
                    model.comment_form

                newForm =
                    { form | id = id }
            in
            ( { model | comment_form = newForm }, Cmd.none, Cmd.none )

        ChangeCommentPost field value ->
            let
                form =
                    model.comment_form

                newForm =
                    { form | post = Dict.insert field value form.post }
            in
            ( { model | comment_form = newForm }, Cmd.none, Cmd.none )

        CancelCommentPatch ->
            let
                form =
                    model.comment_form

                newForm =
                    { form | id = "" }
            in
            ( { model | comment_form = newForm }, Cmd.none, Ports.bulma_driver "" )

        SubmitCommentPatch form time ->
            let
                newForm =
                    { form | post = Dict.insert "updatedAt" (fromTime time) form.post }
            in
            ( model, patchComment apis.gql newForm CommentPatchAck, Cmd.none )

        CommentPatchAck result ->
            case result of
                Success comment ->
                    let
                        tension_d =
                            case model.tension_data of
                                Success t ->
                                    let
                                        comments =
                                            t.comments |> withDefault []

                                        n =
                                            comments
                                                |> LE.findIndex (\c -> c.id == comment.id)
                                                |> withDefault 0

                                        newTension =
                                            { t
                                                | comments = Just (LE.setAt n comment comments)

                                                --| comments = Just (List.take n comments ++ comment :: List.drop (n + 1) comments)
                                            }
                                    in
                                    Success newTension

                                other ->
                                    other

                        resetForm =
                            initCommentPatchForm global.session.user
                    in
                    ( { model | tension_data = tension_d, comment_form = resetForm, comment_result = result }, Cmd.none, Ports.bulma_driver "" )

                _ ->
                    ( { model | comment_result = result }, Cmd.none, Cmd.none )

        DoChangeTitle ->
            -- init form
            ( { model | isTitleEdit = True }, Cmd.none, Cmd.none )

        CancelTitle ->
            -- init form
            ( { model | isTitleEdit = False }, Cmd.none, Cmd.none )

        SubmitTitle form time ->
            let
                newForm =
                    { form | post = Dict.insert "createdAt" (fromTime time) form.post }
            in
            ( model, patchTitle apis.gql newForm TitleAck, Cmd.none )

        TitleAck result ->
            case result of
                Success title ->
                    let
                        tension_d =
                            case model.tension_data of
                                Success t ->
                                    Success { t | title = title }

                                other ->
                                    other

                        resetForm =
                            initTensionForm model.tensionid global.session.user
                    in
                    ( { model | tension_data = tension_d, tension_form = resetForm, title_result = result, isTitleEdit = False }, Cmd.none, Ports.bulma_driver "" )

                _ ->
                    ( { model | title_result = result }, Cmd.none, Cmd.none )

        ChangeInputViewMode viewMode ->
            ( { model | inputViewMode = viewMode }, Cmd.none, Cmd.none )

        ChangeUpdateViewMode viewMode ->
            let
                form =
                    model.comment_form

                newForm =
                    { form | viewMode = viewMode }
            in
            ( { model | comment_form = newForm }, Cmd.none, Cmd.none )

        -- Join
        DoJoinOrga rootnameid time ->
            case global.session.user of
                LoggedOut ->
                    ( { model | node_action = ActionAuthNeeded }, send DoOpenModal, Cmd.none )

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
                    ( newModel, Cmd.batch [ addNewMember apis.gql form JoinAck, send DoOpenModal ], Cmd.none )

        JoinAck result ->
            case model.node_action of
                JoinOrga (JoinInit form) ->
                    case result of
                        Success n ->
                            ( { model | node_action = JoinOrga (JoinValidation form result) }
                            , Cmd.none
                            , Cmd.batch [ send UpdateUserToken ]
                            )

                        other ->
                            ( { model | node_action = JoinOrga (JoinValidation form result) }, Cmd.none, Cmd.none )

                default ->
                    ( model, Cmd.none, Cmd.none )

        -- Modal
        DoOpenModal ->
            ( { model | isModalActive = True }, Cmd.none, Ports.open_modal )

        DoCloseModal _ ->
            ( { model | isModalActive = False }, Cmd.none, Ports.close_modal )


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
                |> List.map (\c -> viewComment c model)

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
                [ h1 [ class "title tensionTitle" ] <|
                    case model.isTitleEdit of
                        True ->
                            [ div [ class "field is-grouped" ]
                                [ p [ class "control is-expanded" ]
                                    [ input
                                        [ class "input"
                                        , type_ "text"
                                        , placeholder "Title*"
                                        , value (Dict.get "title" model.tension_form.post |> withDefault t.title)
                                        , onInput (ChangeTensionPost "title")
                                        ]
                                        []
                                    ]
                                , p [ class "control buttons" ]
                                    [ span [ class "button has-text-weight-normal is-danger is-small", onClick CancelTitle ] [ text T.cancel ]
                                    , span [ class "button has-text-weight-normal is-success is-small", onClick (Submit <| SubmitTitle model.tension_form) ] [ text T.updateTitle ]
                                    ]
                                ]
                            , case model.title_result of
                                Failure err ->
                                    viewGqlErrors err

                                _ ->
                                    div [] []
                            ]

                        False ->
                            [ text t.title
                            , span [ class "button has-text-weight-normal is-pulled-right is-small", onClick DoChangeTitle ] [ text T.edit ]
                            ]
                , div [ class "tensionSubtitle" ]
                    [ span [ class ("tag is-rounded is-" ++ statusColor t.status) ]
                        [ t.status |> TensionStatus.toString |> text ]
                    , span [ class "tag is-rounded is-light" ] [ div [ class <| "Circle " ++ tensionTypeColor "text" t.type_ ] [ text "\u{00A0}" ], t.type_ |> TensionType.toString |> text ]
                    , viewTensionDateAndUser t.createdAt t.createdBy
                    , viewTensionArrowB "is-pulled-right" t.emitter t.receiver
                    ]
                , hr [ class "has-background-grey-dark" ] []
                ]
            ]
        , div [ class "columns" ]
            [ div [ class "column is-8 tensionComments" ]
                [ div [] subComments
                , hr [ class "has-background-grey is-3" ] []
                , userInput
                ]
            , div [ class "column tensionSidePane" ]
                [ viewSidePane t ]
            ]
        ]


viewComment : Comment -> Model -> Html Msg
viewComment c model =
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

        username =
            model.tension_form.uctx.username
    in
    case msg of
        Just status ->
            let
                action =
                    case status of
                        TensionStatus.Open ->
                            ( "far fa-circle ", "reopened" )

                        TensionStatus.Closed ->
                            ( "fas fa-ban ", "closed" )

                actionIcon =
                    Tuple.first action

                actionText =
                    Tuple.second action
            in
            div [ class "media section is-paddingless actionComment" ]
                [ div [ class "media-left" ] [ Fa.icon0 (actionIcon ++ "fa-2x has-text-" ++ statusColor status) "" ]
                , div [ class "media-content" ]
                    [ div [ class "is-italic" ] [ viewUsernameLink c.createdBy.username, text " ", text actionText, text " the ", text (formatTime c.createdAt) ]
                    ]
                ]

        Nothing ->
            div [ class "media section is-paddingless" ]
                [ div [ class "media-left" ] [ div [ class "image is-48x48 circleBase circle1" ] [ getAvatar c.createdBy.username ] ]
                , div [ class "media-content" ]
                    [ if model.comment_form.id == c.id then
                        viewUpdateInput model.comment_form.uctx c model.comment_form model.comment_result

                      else
                        div [ class "message" ]
                            [ div [ class "message-header" ]
                                [ viewTensionDateAndUserC c.createdAt c.createdBy
                                , if c.createdBy.username == username then
                                    div [ class "dropdown has-dropdown is-right" ]
                                        [ div [ class "dropdown-trigger" ]
                                            [ div
                                                [ class "is-pulled-right ellipsis button-light"
                                                , attribute "aria-controls" "dropdown-menu_ellipsis"
                                                , attribute "aria-haspopup" "true"
                                                ]
                                                [ Fa.icon0 "fas fa-ellipsis-h" "" ]
                                            ]
                                        , div [ class "dropdown-menu", id "dropdown-menu_ellipsis", attribute "role" "menu" ]
                                            [ div [ class "dropdown-content" ]
                                                [ div [ class "dropdown-item" ] [ p [ onClick (DoUpdateComment c.id) ] [ text "Edit" ] ]
                                                ]
                                            ]
                                        ]

                                  else
                                    div [] []
                                ]
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
        message =
            Dict.get "message" form.post |> withDefault ""

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
                    ternary (message == "") "Close tension" "Close and comment"

                TensionStatus.Closed ->
                    ternary (message == "") "Reopen tension" "Reopen and comment"
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
                                     , classList [ ( "is-danger", tension.status == TensionStatus.Open ), ( "is-loading", isLoading ) ]
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


viewUpdateInput : UserCtx -> Comment -> CommentPatchForm -> GqlData Comment -> Html Msg
viewUpdateInput uctx comment form result =
    let
        message =
            Dict.get "message" form.post |> withDefault comment.message

        viewMode =
            form.viewMode

        isLoading =
            result == LoadingSlowly

        isSendable =
            message /= comment.message
    in
    div [ class "message" ]
        [ div [ class "message-header" ]
            [ div [ class "tabs is-boxed is-small" ]
                [ ul []
                    [ li [ classList [ ( "is-active", viewMode == Write ) ] ] [ a [ onClickPD2 (ChangeUpdateViewMode Write), target "_blank" ] [ text "Write" ] ]
                    , li [ classList [ ( "is-active", viewMode == Preview ) ] ] [ a [ onClickPD2 (ChangeUpdateViewMode Preview), target "_blank" ] [ text "Preview" ] ]
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
                                , onInput (ChangeCommentPost "message")
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
                            [ class "button has-text-weight-semibold"
                            , classList [ ( "is-danger", True ), ( "is-loading", isLoading ) ]
                            , onClick CancelCommentPatch
                            ]
                            [ text T.cancel ]
                        , button
                            [ class "button has-text-weight-semibold"
                            , classList [ ( "is-success", isSendable ), ( "is-loading", isLoading ) ]
                            , disabled (not isSendable)
                            , onClick (Submit <| SubmitCommentPatch form)
                            ]
                            [ text T.updateComment ]
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
                [ case t.action of
                    Just action ->
                        case t.data of
                            Just data ->
                                viewData action data

                            Nothing ->
                                div [ class "is-italic" ] [ text ("no data attached for: " ++ TensionAction.toString action) ]

                    Nothing ->
                        div [ class "is-italic" ] [ text "no action requested" ]
                ]
            ]
        ]


viewData : TensionAction.TensionAction -> NodeFragment -> Html Msg
viewData action nf =
    let
        txt =
            getNodeTextFromAction action
    in
    div []
        [ h2 [ class "subtitle" ] [ text ("Action | " ++ TensionAction.toString action) ]
        , case nf.mandate of
            Just mandate ->
                div [ class "card" ]
                    [ div [ class "card-header" ] [ div [ class "card-header-title" ] [ text T.mandateH ] ]
                    , div [ class "card-content" ]
                        [ div [ class "field" ]
                            [ div [ class "label" ] [ text T.purposeH ]
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
                            [ div [ class "label" ] [ text T.responsabilitiesH ]
                            , div [ class "control" ]
                                [ textarea
                                    [ id "textAreaModal"
                                    , class "textarea"
                                    , rows 5
                                    , readonly True
                                    , value (mandate.responsabilities |> withDefault ("<" ++ T.noResponsabilities ++ ">"))

                                    --, placeholder txt.ph_responsabilities
                                    --, onInput <| changePostMsg "responsabilities"
                                    ]
                                    []
                                ]
                            ]
                        , div [ class "field" ]
                            [ div [ class "label" ] [ text T.domainsH ]
                            , div [ class "control" ]
                                [ textarea
                                    [ id "textAreaModal"
                                    , class "textarea"
                                    , rows 5
                                    , readonly True
                                    , value (mandate.domains |> withDefault ("<" ++ T.noDomains ++ ">"))

                                    --, placeholder txt.ph_domains
                                    --, onInput <| changePostMsg "domains"
                                    ]
                                    []
                                ]
                            ]
                        , div [ class "field" ]
                            [ div [ class "label" ] [ text T.policiesH ]
                            , div [ class "control" ]
                                [ textarea
                                    [ id "textAreaModal"
                                    , class "textarea"
                                    , rows 5
                                    , readonly True
                                    , value (mandate.policies |> withDefault ("<" ++ T.noPolicies ++ ">"))

                                    --, placeholder txt.ph_policies
                                    --, onInput <| changePostMsg "policies"
                                    ]
                                    []
                                ]
                            ]
                        ]
                    ]

            Nothing ->
                div [] []
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
        [ div [ classList [ ( "modal-background", True ) ] ]
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
                ]
            ]
            []
        ]


viewJoinOrgaStep : JoinStep JoinOrgaForm -> Html Msg
viewJoinOrgaStep step =
    case step of
        JoinInit _ ->
            div [ class "box spinner" ] [ text T.loading ]

        JoinNotAuthorized errMsg ->
            viewGqlErrors errMsg

        JoinValidation form result ->
            case result of
                Success _ ->
                    div [ class "box is-light", onClick (DoCloseModal "") ]
                        [ Fa.icon0 "fas fa-check fa-2x has-text-success" " "
                        , text (T.welcomIn ++ " ")
                        , span [ class "has-font-weight-semibold" ] [ (form.rootnameid |> String.split "#" |> List.head |> withDefault "Unknonwn") |> text ]
                        ]

                Failure err ->
                    viewGqlErrors err

                default ->
                    div [ class "box spinner" ] [ text T.loading ]



--
-- Utils
--


commentsFromForm : String -> TensionPatchForm -> List Comment
commentsFromForm tid form =
    let
        createdAt =
            Dict.get "createdAt" form.post |> withDefault ""

        updatedAt =
            Dict.get "updatedAt" form.post

        createdBy =
            Username form.uctx.username
    in
    [ Dict.get "message" form.post
        |> Maybe.map
            (\comment ->
                { id = tid
                , createdAt = createdAt
                , updatedAt = updatedAt
                , createdBy = createdBy
                , message = comment
                }
            )
    , Dict.get "message_action" form.post
        |> Maybe.map
            (\message_action ->
                { id = ""
                , createdAt = createdAt
                , updatedAt = Nothing
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


initCommentPatchForm : UserState -> CommentPatchForm
initCommentPatchForm user =
    { uctx =
        case user of
            LoggedIn uctx ->
                uctx

            LoggedOut ->
                UserCtx "" Nothing (UserRights False False) []
    , id = ""
    , post = Dict.empty
    , viewMode = Write
    }
