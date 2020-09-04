module Components.Org.Tension exposing (Flags, Model, Msg, TensionTab(..), init, page, subscriptions, update, view)

import Auth exposing (doRefreshToken, refreshAuthModal)
import Browser.Navigation as Nav
import Components.Fa as Fa
import Components.HelperBar as HelperBar
import Components.Loading as Loading exposing (WebData, viewAuthNeeded, viewGqlErrors, viewHttpErrors)
import Components.Markdown exposing (renderMarkdown)
import Components.Node exposing (ActionView(..), updateNodeForm, viewNodeDoc, viewTensionToolbar)
import Components.Text as T
import Date exposing (formatTime)
import Dict exposing (Dict)
import Extra exposing (ternary, withMaybeData, withStateString)
import Extra.Events exposing (onClickPD, onClickPD2)
import Extra.Url exposing (queryBuilder, queryParser)
import Form exposing (isPostSendable)
import Fractal.Enum.BlobType as BlobType
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Generated.Route as Route exposing (Route, toHref)
import Global exposing (Msg(..), send, sendSleep)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, li, nav, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, autofocus, class, classList, disabled, href, id, placeholder, readonly, rows, target, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (..)
import ModelCommon.Requests exposing (login)
import ModelCommon.Uri exposing (FractalBaseRoute(..), NodeFocus, focusState, uriFromUsername)
import ModelCommon.View
    exposing
        ( blobTypeStr
        , byAt
        , getAvatar
        , getNodeTextFromAction
        , statusColor
        , tensionTypeColor
        , tensionTypeSpan
        , viewActionIcon
        , viewLabels
        , viewTensionArrowB
        , viewTensionDateAndUser
        , viewTensionDateAndUserC
        , viewUpdated
        , viewUsernameLink
        )
import ModelSchema exposing (..)
import Page exposing (Document, Page)
import Ports
import Query.AddNode exposing (addNewMember)
import Query.PatchTension exposing (PatchTensionPayloadID, patchComment, patchTitle, pushTensionPatch)
import Query.QueryNode exposing (queryLocalGraph)
import Query.QueryTension exposing (getTensionBlobs, getTensionComments, getTensionHead)
import RemoteData exposing (RemoteData)
import String.Extra as SE
import Task
import Time
import Url exposing (Url)



---- PROGRAM ----


type alias Flags =
    { param1 : String
    , param2 : String
    , param3 : TensionTab
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

    -- Page
    , tensionid : String
    , activeTab : TensionTab
    , actionView : ActionView
    , tension_head : GqlData TensionHead
    , tension_comments : GqlData TensionComments
    , tension_blobs : GqlData TensionBlobs
    , tension_form : TensionPatchForm
    , tension_patch : GqlData PatchTensionPayloadID
    , comment_form : CommentPatchForm
    , comment_result : GqlData Comment
    , isTitleEdit : Bool
    , title_result : GqlData String

    -- Blob Edit Section
    , isBlobEdit : Bool

    --
    -- Common
    , node_action : ActionState
    , isModalActive : Bool -- Only use by JoinOrga for now. (other actions rely on Bulma drivers)
    , modalAuth : ModalAuth
    , inputViewMode : InputViewMode
    }



-- Query parameters


type TensionTab
    = Conversation
    | Document


actionViewEncoder : ActionView -> String
actionViewEncoder x =
    case x of
        DocView ->
            ""

        DocEdit ->
            "edit"

        DocVersion ->
            "history"


actionViewDecoder : String -> ActionView
actionViewDecoder x =
    case x of
        "edit" ->
            DocEdit

        "history" ->
            DocVersion

        default ->
            DocView



---- MSG ----


type Msg
    = PassedSlowLoadTreshold -- timer
    | Submit (Time.Posix -> Msg) -- Get Current Time
      -- Gql Data Queries
    | GotPath (GqlData LocalGraph) -- GraphQL
    | GotPath2 (GqlData LocalGraph) -- GraphQL
    | GotTensionHead (GqlData TensionHead)
    | GotTensionComments (GqlData TensionComments)
    | GotTensionBlobs (GqlData TensionBlobs)
      -- Page Action
    | ChangeTensionPost String String -- {field value}
    | SubmitTensionPatch TensionPatchForm Time.Posix -- Send form
    | SubmitComment TensionPatchForm (Maybe TensionStatus.TensionStatus) Time.Posix -- Send form
    | TensionPatchAck (GqlData PatchTensionPayloadID)
    | DoUpdateComment String
    | ChangeCommentPost String String
    | SubmitCommentPatch CommentPatchForm Time.Posix
    | CommentPatchAck (GqlData Comment)
    | CancelCommentPatch
    | DoChangeTitle
    | SubmitTitle TensionPatchForm Time.Posix
    | TitleAck (GqlData String)
    | CancelTitle
      -- Blob edit
    | DoBlobEdit BlobType.BlobType
    | ChangeBlobNode String String
    | ChangeBlobMD String
    | SubmitBlob TensionPatchForm Bool Time.Posix -- Send form
    | CancelBlob
      -- JoinOrga Action
    | DoJoinOrga String Time.Posix
    | JoinAck (GqlData Node)
      -- Common
    | Navigate String
    | DoOpenModal -- ports receive / Open  modal
    | DoCloseModal String -- ports receive / Close modal
    | DoCloseAuthModal
    | ChangeAuthPost String String
    | SubmitUser UserForm
    | GotSignin (WebData UserCtx)
    | SubmitKeyDown Int
    | ChangeInputViewMode InputViewMode
    | ChangeUpdateViewMode InputViewMode



---- INIT ----


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        apis =
            global.session.apis

        -- Query parameters
        query =
            queryParser global.url

        -- Focus
        rootnameid =
            flags.param1

        tensionid =
            flags.param2

        tab =
            flags.param3

        -- Focus
        newFocus =
            NodeFocus rootnameid True rootnameid NodeType.Circle

        -- What has changed
        fs =
            focusState TensionBaseUri global.session.referer global.session.node_focus newFocus

        model =
            { node_focus = newFocus
            , tensionid = tensionid
            , activeTab = tab
            , actionView = Dict.get "v" query |> withDefault "" |> actionViewDecoder
            , path_data =
                global.session.path_data
                    |> Maybe.map (\x -> Success x)
                    |> withDefault Loading
            , tension_head =
                global.session.tension_head
                    |> Maybe.map (\x -> Success x)
                    |> withDefault Loading
            , tension_comments = Loading
            , tension_blobs = Loading
            , tension_form = initTensionForm tensionid global.session.user
            , tension_patch = NotAsked
            , comment_form = initCommentPatchForm global.session.user
            , comment_result = NotAsked
            , isTitleEdit = False
            , title_result = NotAsked

            -- Blob
            , isBlobEdit = False

            -- Common
            , node_action = NoOp
            , isModalActive = False
            , modalAuth = Inactive
            , inputViewMode = Write
            }

        cmds =
            [ ternary fs.focusChange (queryLocalGraph apis.gql newFocus.nameid GotPath) Cmd.none
            , if tensionChanged global.session.referer global.url || model.tension_head == Loading then
                getTensionHead apis.gql model.tensionid GotTensionHead

              else
                Cmd.none
            , case tab of
                Conversation ->
                    getTensionComments apis.gql model.tensionid GotTensionComments

                Document ->
                    case model.actionView of
                        DocView ->
                            Cmd.none

                        DocEdit ->
                            Cmd.none

                        DocVersion ->
                            getTensionBlobs apis.gql model.tensionid GotTensionBlobs
            , sendSleep PassedSlowLoadTreshold 500
            ]
    in
    ( model
    , Cmd.batch cmds
    , if fs.focusChange || fs.refresh then
        send (UpdateSessionFocus (Just newFocus))

      else
        Cmd.none
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
                tension_h =
                    ternary (model.tension_head == Loading) LoadingSlowly model.tension_head

                tension_c =
                    ternary (model.tension_comments == Loading) LoadingSlowly model.tension_comments

                tension_b =
                    ternary (model.tension_blobs == Loading) LoadingSlowly model.tension_blobs
            in
            ( { model | tension_head = tension_h, tension_comments = tension_c, tension_blobs = tension_b }, Cmd.none, Cmd.none )

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

        GotTensionHead result ->
            let
                newModel =
                    { model | tension_head = result }
            in
            ( newModel, Ports.bulma_driver "", send (UpdateSessionTensionHead (withMaybeData result)) )

        GotTensionComments result ->
            let
                newModel =
                    { model | tension_comments = result }
            in
            ( newModel, Cmd.none, Ports.bulma_driver "" )

        GotTensionBlobs result ->
            let
                newModel =
                    { model | tension_blobs = result }
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
                    }
            in
            ( { model | tension_patch = LoadingSlowly }, pushTensionPatch apis.gql newForm TensionPatchAck, Cmd.none )

        SubmitComment form status_m time ->
            let
                eventComment =
                    case Dict.get "message" form.post of
                        Just _ ->
                            [ TensionEvent.CommentPushed ]

                        Nothing ->
                            []

                eventStatus =
                    case status_m of
                        Just TensionStatus.Open ->
                            [ TensionEvent.Reopened ]

                        Just TensionStatus.Closed ->
                            [ TensionEvent.Closed ]

                        Nothing ->
                            []

                action_m =
                    if List.member TensionEvent.Reopened eventStatus then
                        withMaybeData model.tension_head
                            |> Maybe.map
                                (\th ->
                                    th.action
                                        |> Maybe.map
                                            (\a ->
                                                case a of
                                                    TensionAction.NewRole ->
                                                        TensionAction.EditRole

                                                    TensionAction.NewCircle ->
                                                        TensionAction.EditCircle

                                                    _ ->
                                                        a
                                            )
                                )
                            |> withDefault Nothing

                    else
                        Nothing

                newForm =
                    { form
                        | status = status_m
                        , action = action_m
                        , events_type = Just (eventComment ++ eventStatus)
                    }
            in
            ( { model | tension_form = newForm }, send (SubmitTensionPatch newForm time), Cmd.none )

        TensionPatchAck result ->
            case result of
                Success tp ->
                    let
                        tension_h =
                            case model.tension_head of
                                Success t ->
                                    Success
                                        { t
                                            | status = model.tension_form.status |> withDefault t.status
                                            , action = ternary (model.tension_form.action == Nothing) t.action model.tension_form.action
                                            , blobs = ternary (tp.blobs == Nothing) t.blobs tp.blobs
                                        }

                                other ->
                                    other

                        tension_c =
                            case model.tension_comments of
                                Success t ->
                                    Success { t | comments = Just ((t.comments |> withDefault []) ++ (tp.comments |> withDefault [])) }

                                other ->
                                    other

                        resetForm =
                            initTensionForm model.tensionid global.session.user
                    in
                    ( { model
                        | tension_head = tension_h
                        , tension_comments = tension_c
                        , tension_form = resetForm
                        , tension_patch = result
                      }
                    , Ports.bulma_driver ""
                    , send (UpdateSessionTensionHead (withMaybeData tension_h))
                    )

                Failure _ ->
                    let
                        form =
                            model.tension_form

                        resetForm =
                            { form | status = Nothing }
                    in
                    if doRefreshToken result then
                        ( { model | modalAuth = Active { post = Dict.fromList [ ( "username", form.uctx.username ) ], result = RemoteData.NotAsked }, tension_patch = NotAsked }
                        , Cmd.none
                        , Ports.open_auth_modal
                        )

                    else
                        ( { model | tension_patch = result, tension_form = resetForm }, Cmd.none, Cmd.none )

                _ ->
                    ( { model | tension_patch = result }, Cmd.none, Cmd.none )

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
            ( { model | comment_form = newForm, comment_result = NotAsked }, Cmd.none, Ports.bulma_driver "" )

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
                        tension_c =
                            case model.tension_comments of
                                Success t ->
                                    let
                                        comments =
                                            t.comments |> withDefault []

                                        n =
                                            comments
                                                |> LE.findIndex (\c -> c.id == comment.id)
                                                |> withDefault 0

                                        newComments =
                                            { t
                                                | comments = Just (LE.setAt n comment comments)

                                                --| comments = Just (List.take n comments ++ comment :: List.drop (n + 1) comments)
                                            }
                                    in
                                    Success newComments

                                other ->
                                    other

                        resetForm =
                            initCommentPatchForm global.session.user
                    in
                    ( { model | tension_comments = tension_c, comment_form = resetForm, comment_result = result }, Cmd.none, Ports.bulma_driver "" )

                other ->
                    if doRefreshToken other then
                        ( { model | modalAuth = Active { post = Dict.fromList [ ( "username", model.comment_form.uctx.username ) ], result = RemoteData.NotAsked } }, Cmd.none, Ports.open_auth_modal )

                    else
                        ( { model | comment_result = result }, Cmd.none, Cmd.none )

        DoChangeTitle ->
            ( { model | isTitleEdit = True }, Cmd.none, Cmd.none )

        CancelTitle ->
            ( { model | isTitleEdit = False, tension_form = initTensionForm model.tensionid global.session.user, title_result = NotAsked }, Cmd.none, Ports.bulma_driver "" )

        SubmitTitle form time ->
            let
                newForm =
                    { form
                        | post = Dict.insert "createdAt" (fromTime time) form.post
                        , events_type = Just [ TensionEvent.TitleUpdated ]
                    }
            in
            ( model, patchTitle apis.gql newForm TitleAck, Cmd.none )

        TitleAck result ->
            case result of
                Success title ->
                    let
                        tension_h =
                            case model.tension_head of
                                Success t ->
                                    Success { t | title = title }

                                other ->
                                    other

                        resetForm =
                            initTensionForm model.tensionid global.session.user
                    in
                    ( { model | tension_head = tension_h, tension_form = resetForm, title_result = result, isTitleEdit = False }, Cmd.none, Ports.bulma_driver "" )

                other ->
                    if doRefreshToken other then
                        ( { model | modalAuth = Active { post = Dict.fromList [ ( "username", model.tension_form.uctx.username ) ], result = RemoteData.NotAsked } }, Cmd.none, Ports.open_auth_modal )

                    else
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

        DoBlobEdit blobType ->
            let
                form =
                    model.tension_form

                newForm =
                    { form
                        | blob_type = Just blobType
                        , node = withMaybeData model.tension_head |> Maybe.map (\th -> nodeFragmentFromTensionHead th) |> withDefault initNodeFragment
                        , md = withMaybeData model.tension_head |> Maybe.map (\th -> mdFromTensionHead th) |> withDefault Nothing
                    }
            in
            ( { model | isBlobEdit = True, tension_form = newForm }, Cmd.none, Cmd.none )

        ChangeBlobNode field value ->
            let
                newForm =
                    updateNodeForm field value model.tension_form
            in
            ( { model | tension_form = newForm }, Cmd.none, Cmd.none )

        ChangeBlobMD value ->
            let
                form =
                    model.tension_form

                newForm =
                    { form | md = Just value }
            in
            ( { model | tension_form = newForm }, Cmd.none, Cmd.none )

        SubmitBlob form doPush time ->
            let
                eventPushed =
                    ternary doPush [ TensionEvent.BlobPushed ] []

                newForm =
                    { form | events_type = Just ([ TensionEvent.BlobCommitted ] ++ eventPushed) }
            in
            ( { model | tension_form = newForm }, send (SubmitTensionPatch newForm time), Cmd.none )

        CancelBlob ->
            ( { model | isBlobEdit = False, tension_form = initTensionForm model.tensionid global.session.user, tension_patch = NotAsked }, Cmd.none, Ports.bulma_driver "" )

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
                            if doRefreshToken other then
                                ( { model | modalAuth = Active { post = Dict.fromList [ ( "username", form.uctx.username ) ], result = RemoteData.NotAsked } }, Cmd.none, Ports.open_auth_modal )

                            else
                                ( { model | node_action = JoinOrga (JoinValidation form result) }, Cmd.none, Cmd.none )

                default ->
                    ( model, Cmd.none, Cmd.none )

        -- Modal
        Navigate url ->
            ( model, Cmd.none, Nav.pushUrl global.key url )

        DoOpenModal ->
            ( { model | isModalActive = True }, Cmd.none, Ports.open_modal )

        DoCloseModal link ->
            let
                gcmd =
                    if link /= "" then
                        send (Navigate link)

                    else
                        Cmd.none
            in
            ( { model | isModalActive = False }, gcmd, Ports.close_modal )

        DoCloseAuthModal ->
            ( { model | modalAuth = Inactive }, Cmd.none, Ports.close_auth_modal )

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
                    ( { model | modalAuth = Inactive }
                    , send DoCloseAuthModal
                    , send (UpdateUserSession uctx)
                    )

                other ->
                    case model.modalAuth of
                        Active form ->
                            let
                                newForm =
                                    { form | result = result }
                            in
                            ( { model | modalAuth = Active newForm }, Cmd.none, Cmd.none )

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
                                    UserForm Dict.empty RemoteData.NotAsked
                    in
                    --ENTER
                    if isPostSendable [ "password" ] form.post then
                        ( model, send (SubmitUser form), Cmd.none )

                    else
                        ( model, Cmd.none, Cmd.none )

                _ ->
                    ( model, Cmd.none, Cmd.none )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions global model =
    Sub.batch
        [ Ports.closeModalFromJs DoCloseModal ]



---- VIEW ----


view : Global.Model -> Model -> Document Msg
view global model =
    { title =
        case model.tension_head of
            Success t ->
                t.title

            _ ->
                "Loading tension..."
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
                [ case model.tension_head of
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
        , refreshAuthModal model.modalAuth { closeModal = DoCloseAuthModal, changePost = ChangeAuthPost, submit = SubmitUser, submitEnter = SubmitKeyDown }
        ]


viewTension : UserState -> TensionHead -> Model -> Html Msg
viewTension u t model =
    let
        username =
            model.tension_form.uctx.username
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
                                        , autofocus True
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
                            , if t.createdBy.username == username then
                                -- @Debug check user rights
                                span
                                    [ class "button has-text-weight-normal is-pulled-right is-small tooltip"
                                    , attribute "data-tooltip" T.editTitle
                                    , onClick DoChangeTitle
                                    ]
                                    [ Fa.icon0 "fas fa-pen" "" ]

                              else
                                span [ class "button has-text-weight-normal is-pulled-right is-small", onClick DoChangeTitle ] [ Fa.icon0 "fas fa-pen" "" ]
                            ]
                , div [ class "tensionSubtitle" ]
                    [ span [ class ("tag is-rounded is-" ++ statusColor t.status) ]
                        [ t.status |> TensionStatus.toString |> text ]
                    , span [ class "tag is-rounded is-light" ] [ div [ class <| "Circle " ++ tensionTypeColor "text" t.type_ ] [ text "\u{00A0}" ], t.type_ |> TensionType.toString |> text ]
                    , viewTensionDateAndUser t.createdAt t.createdBy
                    , viewTensionArrowB "is-pulled-right" t.emitter t.receiver
                    ]
                ]
            ]
        , div [ class "columns is-variable is-6" ]
            [ div [ class "column is-8 " ]
                [ div [ class "tabs is-md" ]
                    [ ul []
                        [ li [ classList [ ( "is-active", model.activeTab == Conversation ) ] ]
                            [ a
                                [ href (Route.Tension_Dynamic_Dynamic { param1 = model.node_focus.rootnameid, param2 = t.id } |> toHref) ]
                                [ Fa.icon "fas fa-comments fa-sm" "Conversation" ]
                            ]
                        , if t.action /= Nothing then
                            li [ classList [ ( "is-active", model.activeTab == Document ) ] ]
                                [ a
                                    [ href (Route.Tension_Dynamic_Dynamic_Action { param1 = model.node_focus.rootnameid, param2 = t.id } |> toHref) ]
                                    [ Fa.icon "fas fa-clone fa-sm" "Document" ]
                                ]

                          else
                            li [] []
                        ]
                    ]
                , case model.activeTab of
                    Conversation ->
                        viewComments u t model

                    Document ->
                        viewDocument u t model
                ]
            , div [ class "column is-3 tensionSidePane" ]
                [ viewSidePane model.node_focus t ]
            ]
        ]


viewComments : UserState -> TensionHead -> Model -> Html Msg
viewComments u t model =
    case model.tension_comments of
        Success tension_c ->
            let
                subComments =
                    tension_c.comments
                        |> withDefault []
                        |> List.map (\c -> viewComment c model)

                userInput =
                    case u of
                        LoggedIn uctx ->
                            viewCommentInput uctx t model.tension_form model.tension_patch model.inputViewMode

                        LoggedOut ->
                            viewJoinNeeded model.node_focus
            in
            div [ class "tensionComments" ]
                [ div [] subComments
                , hr [ class "has-background-grey is-3" ] []
                , userInput
                ]

        Failure err ->
            viewGqlErrors err

        LoadingSlowly ->
            div [ class "spinner" ] []

        other ->
            div [] []


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
                [ div [ class "media-left" ] [ Fa.icon (actionIcon ++ "fa-2x has-text-" ++ statusColor status) "" ]
                , div [ class "media-content" ]
                    [ div [ class "is-italic" ] [ viewUsernameLink c.createdBy.username, text " ", text actionText, text " the ", text (formatTime c.createdAt) ]
                    ]
                ]

        Nothing ->
            div [ class "media section is-paddingless" ]
                [ div [ class "media-left" ] [ a [ class "image circleBase circle1", href (uriFromUsername UsersBaseUri c.createdBy.username) ] [ getAvatar c.createdBy.username ] ]
                , div [ class "media-content" ]
                    [ if model.comment_form.id == c.id then
                        viewUpdateInput model.comment_form.uctx c model.comment_form model.comment_result

                      else
                        div [ class "message" ]
                            [ div [ class "message-header" ]
                                [ viewTensionDateAndUserC c.createdAt c.createdBy
                                , case c.updatedAt of
                                    Just updatedAt ->
                                        viewUpdated updatedAt

                                    Nothing ->
                                        span [] []
                                , if c.createdBy.username == username then
                                    div [ class "dropdown has-dropdown is-right is-pulled-right" ]
                                        [ div [ class "dropdown-trigger" ]
                                            [ div
                                                [ class "ellipsis button-light"
                                                , attribute "aria-controls" "dropdown-menu_ellipsis"
                                                , attribute "aria-haspopup" "true"
                                                ]
                                                [ Fa.icon0 "fas fa-ellipsis-h" "" ]
                                            ]
                                        , div [ class "dropdown-menu", id "dropdown-menu_ellipsis", attribute "role" "menu" ]
                                            [ div [ class "dropdown-content" ]
                                                [ div [ class "dropdown-item button-light" ] [ p [ onClick (DoUpdateComment c.id) ] [ text T.edit ] ]
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


viewCommentInput : UserCtx -> TensionHead -> TensionPatchForm -> GqlData PatchTensionPayloadID -> InputViewMode -> Html Msg
viewCommentInput uctx tension form result viewMode =
    let
        message =
            Dict.get "message" form.post |> withDefault ""

        isLoading =
            result == LoadingSlowly

        isSendable =
            isPostSendable [ "message" ] form.post

        submitComment =
            ternary isSendable [ onClick (Submit <| SubmitComment form Nothing) ] []

        submitCloseOpenTension =
            case tension.status of
                TensionStatus.Open ->
                    [ onClick (Submit <| SubmitComment form (Just TensionStatus.Closed)) ]

                TensionStatus.Closed ->
                    [ onClick (Submit <| SubmitComment form (Just TensionStatus.Open)) ]

        closeOpenText =
            case tension.status of
                TensionStatus.Open ->
                    ternary (message == "") "Close tension" "Close and comment"

                TensionStatus.Closed ->
                    ternary (message == "") "Reopen tension" "Reopen and comment"
    in
    div [ class "media section is-paddingless tensionCommentInput" ]
        [ div [ class "media-left" ] [ a [ class "image circleBase circle1", href (uriFromUsername UsersBaseUri uctx.username) ] [ getAvatar uctx.username ] ]
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
                                        , rows 7
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
                                , autofocus True
                                , class "textarea"
                                , rows 7
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
                            [ class "button has-text-weight-semibold is-danger"
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


viewDocument : UserState -> TensionHead -> Model -> Html Msg
viewDocument u t model =
    let
        md =
            mdFromTensionHead t

        node =
            nodeFragmentFromTensionHead t

        nodeData =
            { data = Success t.id, node = node, isLazy = False, source = TensionBaseUri, focus = model.node_focus }

        hasBeenPushed =
            t.history |> List.map (\e -> e.event_type) |> List.member TensionEvent.BlobPushed
    in
    div [ class "tensionDocument" ]
        [ viewTensionToolbar model.node_focus t.id (Just model.actionView)
        , case model.actionView of
            DocView ->
                viewNodeDoc nodeData Nothing hasBeenPushed

            DocEdit ->
                let
                    msgs =
                        { editBlob = DoBlobEdit
                        , changeNode = ChangeBlobNode
                        , cancel = CancelBlob
                        , submit = Submit
                        , submitBlob = SubmitBlob
                        , isBlobEdit = model.isBlobEdit
                        , form = model.tension_form
                        , result = model.tension_patch
                        }
                in
                viewNodeDoc nodeData (Just msgs) hasBeenPushed

            DocVersion ->
                viewDocVersions model.tension_blobs
        ]


viewDocVersions : GqlData TensionBlobs -> Html Msg
viewDocVersions blobsData =
    case blobsData of
        Success tblobs ->
            div [ class "box" ]
                [ tblobs.blobs
                    |> withDefault []
                    |> List.map
                        (\blob ->
                            div [ class "media" ]
                                [ div [ class "media-content" ]
                                    [ div [ class "level" ]
                                        [ span [ class "level-left" ]
                                            [ span [] [ text (blobTypeStr blob.blob_type) ]
                                            , text "\u{00A0}"
                                            , byAt blob.createdBy blob.createdAt
                                            ]
                                        , if blob.pushedFlag /= Nothing then
                                            span [ class "level-item" ] [ Fa.icon0 "fas fa-tag" "" ]

                                          else
                                            span [] []
                                        ]
                                    ]
                                ]
                        )
                    |> div []
                ]

        Failure err ->
            viewGqlErrors err

        LoadingSlowly ->
            div [ class "spinner" ] []

        _ ->
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


viewSidePane : NodeFocus -> TensionHead -> Html Msg
viewSidePane focus t =
    let
        labels_m =
            t.labels |> Maybe.map (\ls -> ternary (List.length ls == 0) Nothing (Just ls)) |> withDefault Nothing
    in
    div []
        [ div [ class "media" ]
            [ div [ class "media-content" ]
                [ h2 [ class "subtitle has-text-weight-semibold is-md" ] [ text "Labels" ]
                , div [ class "" ]
                    [ case labels_m of
                        Just labels ->
                            viewLabels labels

                        Nothing ->
                            div [ class "is-italic" ] [ text "no labels yet" ]
                    ]
                ]
            ]
        , div [ class "media" ]
            [ div [ class "media-content" ]
                [ h2 [ class "subtitle has-text-weight-semibold is-md" ] [ text "Action" ]
                , div [ class "" ]
                    [ case t.action of
                        Just action ->
                            a
                                [ href (Route.Tension_Dynamic_Dynamic_Action { param1 = focus.rootnameid, param2 = t.id } |> toHref) ]
                                [ viewActionIcon (Just action), text (SE.humanize (TensionAction.toString action)) ]

                        Nothing ->
                            div [ class "is-italic" ] [ text "no action requested" ]
                    ]
                ]
            ]
        ]



-- Actions


setupActionModal : Bool -> ActionState -> Html Msg
setupActionModal isModalActive action =
    div
        [ id "actionModal"
        , class "modal modal-fx-fadeIn elmModal"
        , classList [ ( "is-active", isModalActive ) ]
        ]
        [ div
            [ class "modal-background modal-escape"
            , attribute "data-modal" "actionModal"
            , onClick (DoCloseModal "")
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

                other ->
                    div [] [ text "Action not implemented." ]
            ]
        , button [ class "modal-close is-large", onClick (DoCloseModal "") ] []
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
                        [ Fa.icon "fas fa-check fa-2x has-text-success" " "
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
    , tension_type = Nothing
    , action = Nothing
    , emitter = Nothing
    , receiver = Nothing
    , post = Dict.empty
    , events_type = Nothing
    , blob_type = Nothing
    , node = initNodeFragment
    , md = Nothing
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


tensionChanged : Url -> Url -> Bool
tensionChanged from to =
    let
        id1 =
            case Route.fromUrl from of
                Just r ->
                    case r of
                        Route.Tension_Dynamic_Dynamic params ->
                            params.param2

                        Route.Tension_Dynamic_Dynamic_Action params ->
                            params.param2

                        _ ->
                            ""

                Nothing ->
                    ""

        id2 =
            case Route.fromUrl to of
                Just r ->
                    case r of
                        Route.Tension_Dynamic_Dynamic params ->
                            params.param2

                        Route.Tension_Dynamic_Dynamic_Action params ->
                            params.param2

                        _ ->
                            ""

                Nothing ->
                    ""
    in
    id1 /= id2


nodeFragmentFromTensionHead : TensionHead -> NodeFragment
nodeFragmentFromTensionHead t =
    t.blobs
        |> withDefault []
        |> List.head
        |> Maybe.map (\h -> h.node)
        |> withDefault Nothing
        |> withDefault initNodeFragment


mdFromTensionHead : TensionHead -> Maybe String
mdFromTensionHead t =
    t.blobs
        |> withDefault []
        |> List.head
        |> Maybe.map (\h -> h.md)
        |> withDefault Nothing
