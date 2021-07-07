module Components.ContractsPage exposing (Msg(..), State, init, subscriptions, update, view)

import Auth exposing (AuthState(..), doRefreshToken)
import Components.Loading as Loading exposing (GqlData, ModalData, RequestResult(..), loadingSpin, viewGqlErrors, withMapData, withMaybeData, withMaybeDataMap)
import Components.Markdown exposing (renderMarkdown)
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Date exposing (formatTime)
import Dict exposing (Dict)
import Extra exposing (ternary)
import Extra.Events exposing (onClickPD, onClickPD2)
import Form exposing (isPostEmpty, isPostSendable)
import Fractal.Enum.ContractType as ContractType
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.TensionEvent as TensionEvent
import Generated.Route as Route exposing (Route, toHref)
import Global exposing (send, sendNow, sendSleep)
import Html exposing (Html, a, br, button, div, form, h1, h2, hr, i, input, label, li, nav, option, p, pre, section, select, span, table, tbody, td, text, textarea, tfoot, th, thead, tr, ul)
import Html.Attributes exposing (attribute, checked, class, classList, colspan, disabled, for, href, id, list, name, placeholder, required, rows, selected, target, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Icon as I
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (Apis, GlobalCmd(..), UserState(..), uctxFromUser)
import ModelCommon.Codecs exposing (FractalBaseRoute(..), getCoordoRoles, getOrgaRoles, memberIdDecodec, nid2eor, uriFromUsername)
import ModelCommon.View exposing (byAt, contractEventToText, contractTypeToText, getAvatar, viewTensionArrow, viewTensionDateAndUserC, viewUpdated, viewUsernameLink)
import ModelSchema exposing (..)
import Ports
import Query.AddContract exposing (deleteOneContract)
import Query.PatchContract exposing (sendVote)
import Query.QueryContract exposing (getContract, getContractComments, getContracts)
import Text as T exposing (textH, textT, upH)
import Time


type State
    = State Model


type alias Model =
    { user : UserState
    , rootnameid : String
    , form : ContractForm -- user inputs
    , contracts_result : GqlData Contracts -- result of any query
    , contract_result : GqlData ContractFull
    , contract_result_del : GqlData IdPayload
    , voteForm : VoteForm
    , vote_result : GqlData ContractResult
    , activeView : ContractsPageView

    -- Common
    , refresh_trial : Int -- use to refresh user token
    , modal_confirm : ModalConfirm Msg
    }


type ContractsPageView
    = ContractView
    | ContractsView


initModel : String -> UserState -> Model
initModel rootnameid user =
    { user = user
    , rootnameid = rootnameid
    , contracts_result = NotAsked
    , contract_result = NotAsked
    , contract_result_del = NotAsked
    , vote_result = NotAsked
    , form = initContractForm user
    , voteForm = initVoteForm user
    , activeView = ContractsView

    -- Common
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg
    }


type alias Contracts =
    List Contract


type alias ContractForm =
    { uctx : UserCtx
    , tid : String
    , cid : String
    , page : Int
    , page_len : Int
    , events_type : Maybe (List TensionEvent.TensionEvent)
    , post : Post
    }


initContractForm : UserState -> ContractForm
initContractForm user =
    { uctx = uctxFromUser user
    , tid = ""
    , cid = ""
    , page = 0
    , page_len = 10
    , events_type = Nothing
    , post = Dict.empty
    }


type alias VoteForm =
    { uctx : UserCtx
    , cid : String
    , rootnameid : String
    , vote : Int
    , post : Post
    }


initVoteForm : UserState -> VoteForm
initVoteForm user =
    { uctx = uctxFromUser user
    , cid = ""
    , rootnameid = ""
    , vote = 0
    , post = Dict.empty
    }


init : String -> UserState -> State
init rid user =
    initModel rid user |> State



-- Global methods
--isOpen_ : State -> Bool
--isOpen_ (State model) =
--    model.isOpen
--- State Controls


updatePost : String -> String -> Model -> Model
updatePost field value model =
    let
        form =
            model.form
    in
    { model | form = { form | post = Dict.insert field value form.post } }


setContractsResult : GqlData Contracts -> Model -> Model
setContractsResult result model =
    { model | contracts_result = result }


setContractResult : GqlData ContractFull -> Model -> Model
setContractResult result model =
    { model | contract_result = result }


setContractDelResult : GqlData IdPayload -> Model -> Model
setContractDelResult result model =
    { model | contract_result_del = result }



-- utils


canExitSafe : Model -> Bool
canExitSafe model =
    -- Condition to close safely (e.g. empty form data)
    (hasData model && withMaybeData model.contracts_result == Nothing) == False


hasData : Model -> Bool
hasData model =
    -- When you can commit (e.g. empty form data)
    isPostEmpty [ "message" ] model.form.post == False



-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = -- Data
      OnLoad String (Maybe String)
    | OnChangePost String String
    | DoClickContract String
    | DoQueryContracts
    | DoQueryContract String
    | DoQueryContractComments String
    | DoDeleteContract String
    | DoPopContract String
    | DoVote Int Time.Posix
    | OnVoteAck (GqlData ContractResult)
    | OnSubmit (Time.Posix -> Msg)
    | OnContractsAck (GqlData Contracts)
    | OnContractAck (GqlData ContractFull)
    | OnContractCommentsAck (GqlData ContractComments)
    | OnContractDeleteAck (GqlData IdPayload)
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
    , result : Maybe ( Bool, List IdPayload ) -- define what data is to be returned
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
        OnLoad tid cid_m ->
            let
                form =
                    model.form

                f =
                    { form | tid = tid, cid = withDefault form.cid cid_m }
            in
            case cid_m of
                Just cid ->
                    ( { model | form = f, activeView = ContractView }, out0 [ send (DoQueryContract cid) ] )

                Nothing ->
                    ( { model | form = f, activeView = ContractsView }, out0 [ send DoQueryContracts ] )

        OnChangePost field value ->
            ( updatePost field value model, noOut )

        DoClickContract cid ->
            let
                form =
                    model.form

                f =
                    { form | cid = cid }

                url =
                    Route.Tension_Dynamic_Dynamic_Contract_Dynamic { param1 = model.rootnameid, param2 = model.form.tid, param3 = cid } |> toHref
            in
            ( { model | form = f, activeView = ContractView }, out1 [ DoNavigate url ] )

        DoQueryContracts ->
            ( { model | contracts_result = LoadingSlowly }, out0 [ getContracts apis.gql model.form OnContractsAck ] )

        DoQueryContract cid ->
            ( { model | contract_result = LoadingSlowly }, out0 [ getContract apis.gql model.form OnContractAck ] )

        DoQueryContractComments cid ->
            let
                form =
                    model.form

                f =
                    { form | cid = cid }
            in
            ( { model | form = f }, out0 [ getContractComments apis.gql f OnContractCommentsAck ] )

        DoDeleteContract cid ->
            let
                form =
                    model.form

                f =
                    { form | cid = cid }
            in
            ( { model | contract_result_del = LoadingSlowly, form = f }, out0 [ deleteOneContract apis.gql f OnContractDeleteAck ] )

        OnSubmit next ->
            ( model
            , out0 [ sendNow next ]
            )

        OnContractsAck result ->
            let
                data =
                    setContractsResult result model
            in
            case doRefreshToken result data.refresh_trial of
                Authenticate ->
                    ( setContractsResult NotAsked model
                    , out1 [ DoAuth data.form.uctx ]
                    )

                RefreshToken i ->
                    ( { data | refresh_trial = i }, out2 [ sendSleep DoQueryContracts 500 ] [ DoUpdateToken ] )

                OkAuth d ->
                    ( data, Out [] [] (Just ( True, List.map (\c -> { id = c.id }) d )) )

                NoAuth ->
                    ( data, noOut )

        OnContractAck result ->
            let
                data =
                    setContractResult result model
            in
            case doRefreshToken result data.refresh_trial of
                Authenticate ->
                    ( setContractResult NotAsked model
                    , out1 [ DoAuth data.form.uctx ]
                    )

                RefreshToken i ->
                    ( { data | refresh_trial = i }, out2 [ sendSleep (DoQueryContract model.form.cid) 500 ] [ DoUpdateToken ] )

                OkAuth d ->
                    ( data, Out [ send (DoQueryContractComments d.id) ] [] (Just ( True, [ { id = d.id } ] )) )

                NoAuth ->
                    ( data, noOut )

        OnContractCommentsAck result ->
            let
                newResult =
                    model.contract_result
                        |> withMapData
                            (\c ->
                                withMaybeDataMap (\r -> { c | comments = r.comments }) result
                                    |> withDefault c
                            )

                data =
                    setContractResult newResult model
            in
            case doRefreshToken result data.refresh_trial of
                Authenticate ->
                    ( setContractResult NotAsked model
                    , out1 [ DoAuth data.form.uctx ]
                    )

                RefreshToken i ->
                    ( { data | refresh_trial = i }, out2 [ sendSleep (DoQueryContractComments model.form.cid) 500 ] [ DoUpdateToken ] )

                OkAuth d ->
                    ( data, Out [] [] (Just ( True, [] )) )

                NoAuth ->
                    ( data, noOut )

        OnContractDeleteAck result ->
            let
                data =
                    setContractDelResult result model
            in
            case doRefreshToken result data.refresh_trial of
                Authenticate ->
                    ( setContractDelResult NotAsked model
                    , out1 [ DoAuth data.form.uctx ]
                    )

                RefreshToken i ->
                    ( { data | refresh_trial = i }, out2 [ sendSleep (DoDeleteContract model.form.cid) 500 ] [ DoUpdateToken ] )

                OkAuth d ->
                    ( data, Out [ sendSleep (DoPopContract model.form.cid) 500 ] [] (Just ( True, [] )) )

                NoAuth ->
                    ( data, noOut )

        DoPopContract cid ->
            let
                result =
                    model.contracts_result
                        |> withMapData
                            (\cs ->
                                case LE.findIndex (\c -> c.id == cid) cs of
                                    Just i ->
                                        LE.removeAt i cs

                                    Nothing ->
                                        cs
                            )
            in
            ( setContractsResult result model, noOut )

        DoVote v time ->
            let
                f =
                    model.voteForm

                form =
                    { f
                        | vote = v
                        , cid = model.form.cid
                        , rootnameid = model.rootnameid
                        , post = Dict.insert "createdAt" (fromTime time) f.post
                    }
            in
            ( { model | voteForm = form }, out0 [ sendVote apis.gql form OnVoteAck ] )

        OnVoteAck result ->
            let
                data =
                    { model | vote_result = result }
            in
            case doRefreshToken result data.refresh_trial of
                Authenticate ->
                    ( { model | vote_result = NotAsked }, out1 [ DoAuth data.form.uctx ] )

                RefreshToken i ->
                    ( { data | refresh_trial = i }, out2 [ sendSleep (OnSubmit <| DoVote model.voteForm.vote) 500 ] [ DoUpdateToken ] )

                OkAuth d ->
                    ( data, noOut )

                NoAuth ->
                    ( data, noOut )

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


subscriptions =
    [ Ports.mcPD Ports.closeModalConfirmFromJs LogErr DoModalConfirmClose
    ]



-- ------------------------------
-- V I E W
-- ------------------------------


type alias Op =
    { emitterid : String
    , receiverid : String
    , isAdmin : Bool
    }


view : Op -> State -> Html Msg
view op (State model) =
    div []
        [ div [ class "columns" ]
            [ div
                [ class "slider column is-12"
                , classList [ ( "is-slide-left", model.activeView /= ContractsView ), ( "is-transparent", model.activeView /= ContractsView ) ]
                ]
                [ viewContracts op model ]
            , div
                [ class "slider column is-12"
                , classList [ ( "is-slide-left", model.activeView == ContractView ), ( "is-transparent", model.activeView /= ContractView ) ]
                ]
                [ viewContract op model ]
            ]
        , ModalConfirm.view { data = model.modal_confirm, onClose = DoModalConfirmClose, onConfirm = DoModalConfirmSend }
        ]


viewContracts : Op -> Model -> Html Msg
viewContracts op model =
    case model.contracts_result of
        Success data ->
            viewContractsTable data op model

        Failure err ->
            viewGqlErrors err

        LoadingSlowly ->
            div [ class "spinner" ] []

        _ ->
            text ""


headers : List String
headers =
    [ "Event", "Validation", "Author", "Opened", "" ]


viewContractsTable : Contracts -> Op -> Model -> Html Msg
viewContractsTable data op model =
    table
        [ class "table is-fullwidth tensionContracts" ]
        [ thead [ class "is-size-7" ]
            [ tr [] (headers |> List.map (\x -> th [ class "has-text-weight-light" ] [ textH x ]))
            ]
        , data
            |> List.map (\d -> viewRow d op model)
            |> List.concat
            |> tbody []
        ]


viewRow : Contract -> Op -> Model -> List (Html Msg)
viewRow d op model =
    let
        deleteLoading =
            (model.contract_result_del == LoadingSlowly) && d.id == model.form.cid

        isDeleted =
            (withMaybeData model.contract_result_del /= Nothing) && model.form.cid == d.id

        isAuthor =
            d.createdBy.username == model.form.uctx.username
    in
    [ tr
        [ class "mediaBox is-hoverable"
        , classList [ ( "do-clear", isDeleted ) ]
        ]
        [ td [ onClick (DoClickContract d.id) ]
            [ a
                [ href (Route.Tension_Dynamic_Dynamic_Contract_Dynamic { param1 = model.rootnameid, param2 = model.form.tid, param3 = d.id } |> toHref) ]
                [ span [] [ textH (contractEventToText d.event.event_type) ] ]
            ]
        , td [ onClick (DoClickContract d.id) ] [ span [] [ textH (contractTypeToText d.contract_type) ] ]
        , td [ class "has-links-light" ] [ viewUsernameLink d.createdBy.username ]
        , td [] [ text (formatTime d.createdAt) ]

        -- participant
        -- n comments icons
        , td [ class "is-aligned-right is-size-7", attribute "style" "min-width: 6rem;" ]
            [ if isAuthor || op.isAdmin then
                span
                    [ class "button-light"
                    , onClick <| DoModalConfirmOpen (DoDeleteContract d.id) { message = Nothing, txts = [ ( upH T.confirmDeleteContract, "" ), ( "?", "" ) ] }
                    ]
                    [ span [ class "tag is-danger is-light is-smaller2" ] [ I.icon "icon-x", loadingSpin deleteLoading ] ]

              else
                text ""
            ]
        ]
    ]
        ++ (if model.form.cid == d.id then
                [ case model.contract_result of
                    Failure err ->
                        td [ colspan (List.length headers) ] [ viewGqlErrors err ]

                    _ ->
                        text ""
                ]
                    ++ [ case model.contract_result_del of
                            Failure err ->
                                td [ colspan (List.length headers) ] [ viewGqlErrors err ]

                            _ ->
                                text ""
                       ]

            else
                []
           )


viewContract : Op -> Model -> Html Msg
viewContract op model =
    case model.contract_result of
        Success data ->
            viewContractPage data op model

        Failure err ->
            viewGqlErrors err

        LoadingSlowly ->
            div [ class "spinner" ] []

        _ ->
            text ""


viewContractPage : ContractFull -> Op -> Model -> Html Msg
viewContractPage data op model =
    div []
        [ viewContractBox data op model
        , case data.comments of
            Nothing ->
                div [ class "spinner" ] []

            Just comments ->
                --viewComments comments data op model -- @todo
                text ""
        ]


viewContractBox : ContractFull -> Op -> Model -> Html Msg
viewContractBox data op model =
    let
        uctx =
            model.form.uctx

        isCandidate =
            data.candidates |> withDefault [] |> List.map (\x -> x.username) |> List.member uctx.username

        participants =
            data.participants |> withDefault []

        isParticipant =
            participants |> List.map (\x -> memberIdDecodec x.node.nameid) |> List.member uctx.username

        isValidator =
            withDefault False data.isValidator
    in
    div []
        [ form [ class "box is-light form" ]
            [ div [ class "field is-horizontal" ]
                [ div [ class "field-label" ] [ label [ class "label" ] [ textH T.contractType ] ]
                , div [ class "field-body" ]
                    [ div [ class "field is-narrow" ]
                        [ input [ class "input", value (upH (contractTypeToText data.contract_type)), disabled True ] []
                        ]
                    ]
                ]
            , div [ class "field is-horizontal" ]
                [ div [ class "field-label" ] [ label [ class "label" ] [ textH T.contractEvent ] ]
                , div [ class "field-body" ] <|
                    case data.event.event_type of
                        TensionEvent.Moved ->
                            let
                                emitter =
                                    data.event.old |> withDefault "unknown" |> nid2eor

                                receiver =
                                    data.event.new |> withDefault "unkown" |> nid2eor
                            in
                            [ div [ class "field is-narrow" ]
                                [ input [ class "input", value (upH (contractEventToText data.event.event_type)), disabled True ] []
                                ]
                            , viewTensionArrow "is-pulled-right" emitter receiver
                            ]

                        _ ->
                            [ text "not implemented" ]
                ]
            , div [ class "field pb-2" ] [ span [ class "is-pulled-right is-smaller" ] [ textH (T.created ++ "\u{00A0}"), byAt data.createdBy data.createdAt ] ]
            ]
        , if (isValidator || isCandidate) && isParticipant == False then
            p [ class "buttons is-centered" ]
                [ div
                    [ class "button is-success is-light is-rounded"
                    , onClick (OnSubmit <| DoVote 1)
                    ]
                    [ span [ class "mx-4" ] [ textH "accept" ] ]
                , div
                    [ class "button is-danger is-light is-rounded"
                    , onClick (OnSubmit <| DoVote 0)
                    ]
                    [ span [ class "mx-4" ] [ textH "decline" ] ]
                ]

          else
            text ""
        ]



--viewComments : List Comment -> Contract -> Op -> Model -> Html Msg
--viewComments comments data op model =
--    let
--        userInput =
--            case model.user of
--                LoggedIn uctx ->
--                    let
--                        orgaRoles =
--                            getOrgaRoles uctx.roles [ op.emitterid, op.receiverid ]
--                    in
--                    case orgaRoles of
--                        [] ->
--                            viewJoinNeeded model.node_focus
--
--                        _ ->
--                            viewCommentInput uctx t model.tension_form model.tension_patch model.inputViewMode
--
--                LoggedOut ->
--                    viewJoinNeeded model.node_focus
--    in
--    comments
--        |> List.map
--            (\c ->
--                let
--                    isAuthor =
--                        c.createdBy.username == model.form.uctx.username
--                in
--                viewComment c isAuthor
--            )
--        |> div []


viewComment : Comment -> Bool -> Html Msg
viewComment c isAuthor =
    div [ class "media section is-paddingless" ]
        [ div [ class "media-left" ] [ a [ class "image circleBase circle1", href (uriFromUsername UsersBaseUri c.createdBy.username) ] [ getAvatar c.createdBy.username ] ]
        , div
            [ class "media-content"
            , attribute "style" "width: 66.66667%;"
            ]
            [ if False then
                --if model.comment_form.id == c.id then
                --viewUpdateInput model.comment_form.uctx c model.comment_form model.comment_result
                text "edit box"

              else
                div [ class "message" ]
                    [ div [ class "message-header" ]
                        [ viewTensionDateAndUserC c.createdAt c.createdBy
                        , case c.updatedAt of
                            Just updatedAt ->
                                viewUpdated updatedAt

                            Nothing ->
                                text ""
                        , if isAuthor then
                            div [ class "dropdown is-right is-pulled-right " ]
                                [ div [ class "dropdown-trigger" ]
                                    [ div
                                        [ class "ellipsis"
                                        , attribute "aria-controls" ("dropdown-menu_ellipsis" ++ c.id)
                                        , attribute "aria-haspopup" "true"
                                        ]
                                        [ I.icon "icon-ellipsis-v" ]
                                    ]
                                , div [ id ("dropdown-menu_ellipsis" ++ c.id), class "dropdown-menu", attribute "role" "menu" ]
                                    [ div [ class "dropdown-content" ]
                                        --[ div [ class "dropdown-item button-light" ] [ p [ onClick (DoUpdateComment c.id) ] [ textH T.edit ] ] ]
                                        []
                                    ]
                                ]

                          else
                            text ""
                        ]
                    , div [ class "message-body" ]
                        [ case c.message of
                            "" ->
                                div [ class "is-italic" ] [ text "No description provided." ]

                            message ->
                                renderMarkdown "is-light" message
                        ]
                    ]
            ]
        ]
