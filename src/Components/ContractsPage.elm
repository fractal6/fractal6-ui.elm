{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2023 Fractale Co

   This file is part of Fractale.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU Affero General Public License as
   published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Affero General Public License for more details.

   You should have received a copy of the GNU Affero General Public License
   along with Fractale.  If not, see <http://www.gnu.org/licenses/>.
-}


module Components.ContractsPage exposing (Msg(..), State, init, subscriptions, update, view)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Bulk exposing (CommentPatchForm, InputViewMode(..), UserState(..), initCommentPatchForm, nodeFromTension, pushCommentReaction, removeCommentReaction, uctxFromUser)
import Bulk.Codecs exposing (FractalBaseRoute(..), contractIdCodec, memberIdDecodec, nid2eor, nid2rootid, nodeIdCodec, uriFromNameid)
import Bulk.Error exposing (viewGqlErrors)
import Bulk.Event exposing (cev2c, cev2p, contractEventToText, contractEventToValue, contractTypeToText)
import Bulk.View exposing (byAt, viewRole, viewTensionArrow, viewUserFull, viewUsernameLink)
import Components.Comments as Comments
import Components.ModalConfirm as ModalConfirm exposing (ModalConfirm, TextMessage)
import Dict
import Extra exposing (space_, ternary, upH)
import Extra.Date exposing (formatDate)
import Form exposing (isPostEmpty)
import Fractal.Enum.ContractStatus as ContractStatus
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Generated.Route as Route exposing (toHref)
import Global exposing (send, sendNow, sendSleep)
import Html exposing (Html, a, br, div, form, hr, i, input, label, p, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (attribute, class, classList, colspan, disabled, href, id, name, selected, type_, value)
import Html.Events exposing (onClick)
import Html.Lazy as Lazy
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Loading exposing (GqlData, ModalData, RequestResult(..), isFailure, loadingSpin, withMapData, withMaybeData, withMaybeMapData)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Ports
import Query.AddContract exposing (deleteOneContract)
import Query.PatchContract exposing (pushContractComment, sendVote)
import Query.PatchTension exposing (patchComment)
import Query.QueryContract exposing (getContract, getContracts)
import Query.Reaction exposing (addReaction, deleteReaction)
import Session exposing (Apis, Conf, GlobalCmd(..))
import Text as T
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

    -- Comments
    , comment_form : CommentPatchForm
    , comment_patch_form : CommentPatchForm
    , comment_result : GqlData Comment

    -- Common
    , conf : Conf
    , refresh_trial : Int -- use to refresh user token
    , modal_confirm : ModalConfirm Msg
    , comments : Comments.State
    }


type ContractsPageView
    = ContractView
    | ContractsView


initModel : String -> UserState -> Conf -> Model
initModel focusid user conf =
    { user = user
    , rootnameid = nid2rootid focusid
    , contracts_result = NotAsked
    , contract_result = NotAsked
    , contract_result_del = NotAsked
    , vote_result = NotAsked
    , form = initContractForm user
    , voteForm = initVoteForm user
    , activeView = ContractsView
    , comment_form = initCommentPatchForm user []
    , comment_patch_form = initCommentPatchForm user []
    , comment_result = NotAsked

    -- Common
    , conf = conf
    , refresh_trial = 0
    , modal_confirm = ModalConfirm.init NoMsg

    -- Components
    , comments = Comments.init focusid "" user
    }


type alias Contracts =
    List Contract


type alias ContractForm =
    { uctx : UserCtx
    , tid : String
    , cid : String
    , contractid : String
    , page : Int
    , page_len : Int
    , post : Post
    }


initContractForm : UserState -> ContractForm
initContractForm user =
    { uctx = uctxFromUser user
    , tid = ""
    , cid = ""
    , contractid = ""
    , page = 0
    , page_len = 10
    , post = Dict.empty
    }


type alias VoteForm =
    { uctx : UserCtx
    , cid : String
    , contractid : String
    , rootnameid : String
    , vote : Int
    , post : Post
    }


initVoteForm : UserState -> VoteForm
initVoteForm user =
    { uctx = uctxFromUser user
    , cid = ""
    , contractid = ""
    , rootnameid = ""
    , vote = 0
    , post = Dict.empty
    }


init : String -> UserState -> Conf -> State
init rid user conf =
    initModel rid user conf |> State



-- Global methods


hasCid : State -> Bool
hasCid (State model) =
    model.activeView == ContractView


hasLoadFailure : State -> Bool
hasLoadFailure (State model) =
    case model.activeView of
        ContractView ->
            isFailure model.contract_result

        ContractsView ->
            isFailure model.contracts_result



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
    let
        form =
            model.form

        newForm =
            case result of
                Success c ->
                    --{ form | cid = c.id }
                    { form | contractid = contractIdCodec c.tension.id (TensionEvent.toString c.event.event_type) (withDefault "" c.event.old) (withDefault "" c.event.new) }

                _ ->
                    form
    in
    { model | contract_result = result, form = newForm }


setContractDelResult : GqlData IdPayload -> Model -> Model
setContractDelResult result model =
    { model | contract_result_del = result }



-- utils


canExitSafe : Model -> Bool
canExitSafe model =
    -- Condition to close safely (e.g. empty form data)
    not (hasData model && withMaybeData model.contracts_result == Nothing)


hasData : Model -> Bool
hasData model =
    -- When you can commit (e.g. empty form data)
    not (isPostEmpty [ "message" ] model.form.post)



-- ------------------------------
-- U P D A T E
-- ------------------------------


type Msg
    = -- Data
      OnLoad String (Maybe String)
    | OnChangePost String String
    | SetRootnameid String
    | DoClickContract String
    | DoQueryContracts
    | DoQueryContract
    | DoDeleteContract String
    | DoPopContract String
    | DoVote Int Time.Posix
    | OnVoteAck (GqlData ContractResult)
    | OnSubmit Bool (Time.Posix -> Msg)
    | OnContractsAck (GqlData Contracts)
    | OnContractAck (GqlData ContractFull)
    | OnContractDeleteAck (GqlData IdPayload)
      -- Comments
    | PushContractComment
    | PushCommentPatch
    | DoUpdateComment Comment
    | CancelCommentPatch
    | ChangeCommentPost String String
    | ChangeCommentPatch String String
    | SubmitCommentPost (Maybe TensionStatus.TensionStatus) Time.Posix
    | SubmitCommentPatch Time.Posix
    | CommentAck (GqlData Comment)
    | CommentPatchAck (GqlData Comment)
    | OnRichText String String
    | OnToggleMdHelp String
    | OnAddReaction String Int
    | OnAddReactionAck (GqlData ReactionResponse)
    | OnDeleteReaction String Int
    | OnDeleteReactionAck (GqlData ReactionResponse)
      -- Confirm Modal
    | DoModalConfirmOpen Msg TextMessage
    | DoModalConfirmClose ModalData
    | DoModalConfirmSend
      -- Common
    | NoMsg
    | LogErr String
    | UpdateUctx UserCtx
    | ChangeInputViewMode InputViewMode
    | ChangeUpdateViewMode InputViewMode
    | CommentsMsg Comments.Msg


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

                cid =
                    withDefault form.cid cid_m

                f =
                    { form | tid = tid, cid = cid }
            in
            case cid of
                "" ->
                    ( { model | form = f, activeView = ContractsView }, out0 [ send DoQueryContracts ] )

                _ ->
                    -- @debug: sendSleep to try to fix a race condition where the contract is fetch before tension_head
                    ( { model | form = f, activeView = ContractView }, out0 [ sendSleep DoQueryContract 250 ] )

        OnChangePost field value ->
            ( updatePost field value model, noOut )

        SetRootnameid rootnameid ->
            ( { model | rootnameid = rootnameid }, noOut )

        DoClickContract cid ->
            let
                form =
                    model.form

                url =
                    Route.Tension_Dynamic_Dynamic_Contract_Dynamic { param1 = model.rootnameid, param2 = model.form.tid, param3 = cid } |> toHref
            in
            ( { model | form = { form | cid = cid }, activeView = ContractView }
            , out1 [ DoNavigate url ]
            )

        DoQueryContracts ->
            ( { model | contracts_result = LoadingSlowly }, out0 [ getContracts apis model.form OnContractsAck ] )

        DoQueryContract ->
            ( { model | contract_result = LoadingSlowly }, out0 [ getContract apis model.form OnContractAck ] )

        DoDeleteContract cid ->
            let
                form =
                    model.form

                f =
                    { form | cid = cid }
            in
            ( { model | contract_result_del = LoadingSlowly, form = f }, out0 [ deleteOneContract apis f OnContractDeleteAck ] )

        OnSubmit isSendable next ->
            if isSendable then
                ( model, out0 [ sendNow next ] )

            else
                ( model, noOut )

        OnContractsAck result ->
            let
                data =
                    setContractsResult result model
            in
            case parseErr result data.refresh_trial of
                Authenticate ->
                    ( setContractsResult NotAsked model
                    , out0 [ Ports.raiseAuthModal data.form.uctx ]
                    )

                RefreshToken i ->
                    ( { data | refresh_trial = i }, out2 [ sendSleep DoQueryContracts 500 ] [ DoUpdateToken ] )

                OkAuth d ->
                    ( data, Out [] [] (Just ( True, List.map (\c -> { id = c.id }) d )) )

                _ ->
                    ( data, noOut )

        OnContractAck result ->
            let
                data =
                    setContractResult result model
            in
            case parseErr result data.refresh_trial of
                Authenticate ->
                    ( setContractResult NotAsked model
                    , out0 [ Ports.raiseAuthModal data.form.uctx ]
                    )

                RefreshToken i ->
                    ( { data | refresh_trial = i }, out2 [ sendSleep DoQueryContract 500 ] [ DoUpdateToken ] )

                OkAuth d ->
                    -- Memory Optimization; Do no store comments twice.
                    ( { data | contract_result = Success { d | comments = Nothing } }
                    , Out
                        [ Cmd.map CommentsMsg (send <| Comments.SetContractid d.id)
                        , Cmd.map CommentsMsg (send <| Comments.SetComments (withDefault [] d.comments))
                        ]
                        []
                        (Just ( True, [ { id = d.id } ] ))
                    )

                _ ->
                    ( data, noOut )

        OnContractDeleteAck result ->
            let
                data =
                    setContractDelResult result model
            in
            case parseErr result data.refresh_trial of
                Authenticate ->
                    ( setContractDelResult NotAsked model
                    , out0 [ Ports.raiseAuthModal data.form.uctx ]
                    )

                RefreshToken i ->
                    ( { data | refresh_trial = i }, out2 [ sendSleep (DoDeleteContract model.form.cid) 500 ] [ DoUpdateToken ] )

                OkAuth _ ->
                    ( data, Out [ sendSleep (DoPopContract model.form.cid) 500 ] [] (Just ( True, [] )) )

                _ ->
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

                        --, cid = model.form.cid
                        , contractid = model.form.contractid
                        , rootnameid = model.rootnameid
                        , post = Dict.insert "createdAt" (fromTime time) f.post
                    }
            in
            ( { model | voteForm = form, vote_result = LoadingSlowly }, out0 [ sendVote apis form OnVoteAck ] )

        OnVoteAck result ->
            let
                data =
                    { model | vote_result = result }
            in
            case parseErr result data.refresh_trial of
                Authenticate ->
                    ( { model | vote_result = NotAsked }, out0 [ Ports.raiseAuthModal data.form.uctx ] )

                RefreshToken i ->
                    ( { data | refresh_trial = i }, out2 [ sendSleep (OnSubmit True <| DoVote model.voteForm.vote) 500 ] [ DoUpdateToken ] )

                OkAuth _ ->
                    ( data, ternary (data.voteForm.vote > 0) (out1 [ DoUpdateToken ]) noOut )

                _ ->
                    ( data, noOut )

        -- Comments
        PushContractComment ->
            ( model, out0 [ pushContractComment apis model.comment_form CommentAck ] )

        PushCommentPatch ->
            ( model, out0 [ patchComment apis model.comment_patch_form CommentPatchAck ] )

        DoUpdateComment c ->
            let
                form =
                    model.comment_patch_form
            in
            ( { model | comment_patch_form = { form | id = c.id } }, out0 [ Ports.focusOn "updateCommentInput", Ports.bulma_driver c.createdAt ] )

        CancelCommentPatch ->
            let
                form =
                    model.comment_patch_form
            in
            ( { model | comment_patch_form = { form | id = "", post = Dict.remove "message" form.post }, comment_result = NotAsked }, out0 [ Ports.bulma_driver "" ] )

        ChangeCommentPost field value ->
            let
                form =
                    model.comment_form
            in
            ( { model | comment_form = { form | post = Dict.insert field value form.post } }, noOut )

        ChangeCommentPatch field value ->
            let
                form =
                    model.comment_patch_form
            in
            ( { model | comment_patch_form = { form | post = Dict.insert field value form.post } }, noOut )

        SubmitCommentPost _ time ->
            let
                form =
                    model.comment_form
            in
            ( { model
                | comment_form =
                    { form
                        | post =
                            form.post
                                |> Dict.insert "createdAt" (fromTime time)
                                |> Dict.insert "contractid" (withMaybeMapData .id model.contract_result |> withDefault "")
                    }
                , comment_result = LoadingSlowly
              }
            , out0 [ send PushContractComment ]
            )

        SubmitCommentPatch time ->
            let
                form =
                    model.comment_patch_form
            in
            ( { model | comment_patch_form = { form | post = Dict.insert "updatedAt" (fromTime time) form.post }, comment_result = LoadingSlowly }
            , out0 [ send PushCommentPatch ]
            )

        CommentAck result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( { model | comment_result = NotAsked }, out0 [ Ports.raiseAuthModal model.form.uctx ] )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, out2 [ sendSleep PushContractComment 500 ] [ DoUpdateToken ] )

                OkAuth data ->
                    let
                        contract =
                            case model.contract_result of
                                Success t ->
                                    Success { t | comments = Just (withDefault [] t.comments ++ [ data ]) }

                                other ->
                                    other

                        resetForm =
                            initCommentPatchForm model.user []
                    in
                    ( { model
                        | contract_result = contract
                        , comment_form = resetForm
                        , comment_result = result
                      }
                    , out0 [ Ports.bulma_driver "" ]
                    )

                _ ->
                    ( { model | comment_result = result }, noOut )

        CommentPatchAck result ->
            case parseErr result model.refresh_trial of
                Authenticate ->
                    ( { model | comment_result = NotAsked }, out0 [ Ports.raiseAuthModal model.form.uctx ] )

                RefreshToken i ->
                    ( { model | refresh_trial = i }, out2 [ sendSleep PushCommentPatch 500 ] [ DoUpdateToken ] )

                OkAuth comment ->
                    let
                        contract =
                            case model.contract_result of
                                Success t ->
                                    let
                                        comments =
                                            withDefault [] t.comments

                                        n =
                                            comments
                                                |> LE.findIndex (\c -> c.id == comment.id)
                                                |> withDefault 0
                                    in
                                    Success { t | comments = Just (LE.setAt n comment comments) }

                                other ->
                                    other

                        resetForm =
                            initCommentPatchForm model.user []
                    in
                    ( { model | contract_result = contract, comment_patch_form = resetForm, comment_result = result }
                    , out0 [ Ports.bulma_driver "" ]
                    )

                _ ->
                    ( { model | comment_result = result }, noOut )

        OnRichText targetid command ->
            ( model, out0 [ Ports.richText targetid command ] )

        OnToggleMdHelp targetid ->
            case targetid of
                "commentContractInput" ->
                    let
                        form =
                            model.comment_form

                        field =
                            "isMdHelpOpen" ++ targetid

                        v =
                            Dict.get field form.post |> withDefault "false"

                        value =
                            ternary (v == "true") "false" "true"
                    in
                    ( { model | comment_form = { form | post = Dict.insert field value form.post } }, noOut )

                "updateCommentInput" ->
                    let
                        form =
                            model.comment_patch_form

                        field =
                            "isMdHelpOpen" ++ targetid

                        v =
                            Dict.get field form.post |> withDefault "false"

                        value =
                            ternary (v == "true") "false" "true"
                    in
                    ( { model | comment_patch_form = { form | post = Dict.insert field value form.post } }, noOut )

                _ ->
                    ( model, noOut )

        OnAddReaction cid type_ ->
            case model.user of
                LoggedIn uctx ->
                    ( model, out0 [ addReaction apis uctx.username cid type_ OnAddReactionAck ] )

                LoggedOut ->
                    ( model, out0 [ Ports.raiseAuthModal (uctxFromUser model.user) ] )

        OnAddReactionAck result ->
            let
                uctx =
                    uctxFromUser model.user
            in
            case parseErr result 2 of
                Authenticate ->
                    ( model, out0 [ Ports.raiseAuthModal uctx ] )

                OkAuth r ->
                    let
                        contract_result =
                            model.contract_result
                                |> withMapData (\tc -> { tc | comments = Maybe.map (pushCommentReaction uctx.username r) tc.comments })
                    in
                    ( { model | contract_result = contract_result }, noOut )

                _ ->
                    ( model, noOut )

        OnDeleteReaction cid type_ ->
            case model.user of
                LoggedIn uctx ->
                    ( model, out0 [ deleteReaction apis uctx.username cid type_ OnDeleteReactionAck ] )

                LoggedOut ->
                    ( model, out0 [ Ports.raiseAuthModal (uctxFromUser model.user) ] )

        OnDeleteReactionAck result ->
            let
                uctx =
                    uctxFromUser model.user
            in
            case parseErr result 2 of
                Authenticate ->
                    ( model, out0 [ Ports.raiseAuthModal uctx ] )

                OkAuth r ->
                    let
                        contract_result =
                            model.contract_result
                                |> withMapData (\tc -> { tc | comments = Maybe.map (removeCommentReaction uctx.username r) tc.comments })
                    in
                    ( { model | contract_result = contract_result }, noOut )

                _ ->
                    ( model, noOut )

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

        UpdateUctx uctx ->
            let
                form =
                    model.form

                voteForm =
                    model.voteForm

                cForm =
                    model.comment_form

                cpForm =
                    model.comment_patch_form
            in
            ( { model
                | user = LoggedIn uctx
                , form = { form | uctx = uctx }
                , voteForm = { voteForm | uctx = uctx }
                , comment_form = { cForm | uctx = uctx }
                , comment_patch_form = { cpForm | uctx = uctx }
              }
            , noOut
            )

        ChangeInputViewMode viewMode ->
            let
                form =
                    model.comment_form
            in
            ( { model | comment_form = { form | viewMode = viewMode } }, noOut )

        ChangeUpdateViewMode viewMode ->
            let
                form =
                    model.comment_patch_form
            in
            ( { model | comment_patch_form = { form | viewMode = viewMode } }, noOut )

        -- Components
        CommentsMsg msg ->
            let
                ( data, out ) =
                    Comments.update apis msg model.comments

                ( cmds, gcmds ) =
                    ( [], [] )
            in
            ( { model | comments = data }
            , out2 (out.cmds |> List.map (\m -> Cmd.map CommentsMsg m) |> List.append cmds) out.gcmds
            )


subscriptions : State -> List (Sub Msg)
subscriptions (State model) =
    [ Ports.mcPD Ports.closeModalConfirmFromJs LogErr DoModalConfirmClose
    , Ports.uctxPD Ports.loadUserCtxFromJs LogErr UpdateUctx
    ]
        ++ (Comments.subscriptions model.comments |> List.map (\s -> Sub.map CommentsMsg s))



-- ------------------------------
-- V I E W
-- ------------------------------


type alias Op =
    { receiverid : String
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
            if List.length data == 0 then
                div [] [ text "No contracts yet..." ]

            else
                viewContractsTable data op model

        Failure err ->
            viewGqlErrors err

        LoadingSlowly ->
            div [ class "spinner" ] []

        _ ->
            text ""


headers : List String
headers =
    [ T.contractEvent, "Validation", T.author, upH T.opened, "" ]


viewContractsTable : Contracts -> Op -> Model -> Html Msg
viewContractsTable data op model =
    table
        [ class "table is-fullwidth" ]
        [ thead [ class "is-size-7" ]
            [ tr [] (headers |> List.map (\x -> th [] [ text x ]))
            ]
        , data
            |> List.concatMap (\d -> viewRow d op model)
            |> tbody []
        ]


viewRow : Contract -> Op -> Model -> List (Html Msg)
viewRow d op model =
    let
        isDeleted =
            (withMaybeData model.contract_result_del /= Nothing) && model.form.cid == d.id

        isAuthor =
            d.createdBy.username == model.form.uctx.username
    in
    tr
        [ class "mediaBox is-hoverable"
        , classList [ ( "do-clear", isDeleted ) ]
        ]
        [ td [ onClick (DoClickContract d.id) ]
            [ a
                [ class "discrete-link"
                , href (Route.Tension_Dynamic_Dynamic_Contract_Dynamic { param1 = model.rootnameid, param2 = model.form.tid, param3 = d.id } |> toHref)
                ]
                (text (contractEventToText Nothing d.event.event_type)
                    :: (Maybe.map (\x -> [ text " ・ ", x ]) (contractEventToValue d.event) |> withDefault [])
                )
            ]
        , td [] [ span [] [ text (contractTypeToText d.contract_type) ] ]
        , td [ class "has-links-discrete" ] [ viewUsernameLink d.createdBy.username ]
        , td [] [ text (formatDate model.conf.lang model.conf.now d.createdAt) ]

        -- participant
        -- n comments icons
        , td [ class "is-aligned-right is-size-7", attribute "style" "min-width: 6rem;" ]
            [ if isAuthor || op.isAdmin then
                let
                    deleteLoading =
                        (model.contract_result_del == LoadingSlowly) && d.id == model.form.cid
                in
                span
                    [ class "button-light"
                    , onClick <| DoModalConfirmOpen (DoDeleteContract d.id) { message = Nothing, txts = [ ( T.confirmDeleteContract, "" ), ( "?", "" ) ] }
                    ]
                    [ span [ class "tag is-danger is-light is-smaller2 tooltip has-tooltip-arrow", attribute "data-tooltip" T.deleteThisContract ] [ A.icon "icon-x", loadingSpin deleteLoading ] ]

              else
                text ""
            ]
        ]
        :: (if model.form.cid == d.id then
                [ case model.contract_result of
                    Failure err ->
                        td [ colspan (List.length headers) ] [ viewGqlErrors err ]

                    _ ->
                        text ""
                , case model.contract_result_del of
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
viewContractPage c op model =
    let
        participants =
            c.participants |> List.map (\x -> memberIdDecodec x.node.nameid)

        candidates =
            c.candidates |> withDefault [] |> List.map .username

        isCandidate =
            List.member model.form.uctx.username candidates

        isParticipant =
            List.member model.form.uctx.username participants

        isValidator =
            withDefault False c.isValidator

        isVoteSuccess =
            withMaybeData model.vote_result /= Nothing
    in
    div [ class "comments" ]
        [ viewContractBox c op model
        , if (isValidator || isCandidate) && not isParticipant then
            if isVoteSuccess && model.voteForm.vote == 1 then
                let
                    n =
                        nodeFromTension c.tension
                in
                div [ class "notification is-success is-light" ]
                    [ A.icon1 "icon-check icon-2x has-text-success" " "
                    , ternary isCandidate
                        (text (cev2c n.type_ c.event.event_type))
                        (text (cev2p n.type_ c.event.event_type))
                    ]

            else if isVoteSuccess && model.voteForm.vote == 0 then
                div [ class "notification is-danger is-light" ] [ text T.invitationRejected ]

            else
                viewVoteBox model.form.uctx isValidator participants candidates c model

          else if isParticipant then
            -- @TODO: Change your vote button
            --div [ class "help has-text-centered" ] [ text T.alreadyVoted ]
            text ""

          else
            -- Close, Cancelled or no auth.
            text ""
        , c.comments
            |> Maybe.map
                (\_ ->
                    Comments.viewCommentsContract model.conf model.comments |> Html.map CommentsMsg
                )
            |> withDefault (text "")
        , hr [ class "has-background-border-light is-2" ] []
        , case model.user of
            LoggedIn _ ->
                if isParticipant || isValidator || isCandidate then
                    Comments.viewContractCommentInput model.conf model.comments |> Html.map CommentsMsg

                else
                    text ""

            LoggedOut ->
                text ""
        ]


viewContractBox : ContractFull -> Op -> Model -> Html Msg
viewContractBox c op model =
    let
        n =
            nodeFromTension c.tension
    in
    form [ class "box form" ]
        [ div [ class "columns" ]
            [ div [ class "column is-5" ]
                [ div [ class "field is-horizontal" ]
                    [ div [ class "field-label" ] [ label [ class "label" ] [ text T.contractType ] ]
                    , div [ class "field-bod" ]
                        [ div [ class "field is-narrow" ]
                            [ input [ class "input", value (contractTypeToText c.contract_type), disabled True ] [] ]
                        ]
                    ]
                , div [ class "field is-horizontal" ]
                    [ div [ class "field-label" ] [ label [ class "label" ] [ text T.contractEvent ] ]
                    , div [ class "field-bod" ]
                        [ div [ class "field is-narrow" ]
                            [ input [ class "input", value (contractEventToText n.type_ c.event.event_type), disabled True ] [] ]
                        ]
                    ]
                ]
            , div [ class "column is-6" ]
                [ case c.event.event_type of
                    TensionEvent.Moved ->
                        let
                            emitter =
                                c.event.old |> withDefault "unknown" |> nid2eor

                            receiver =
                                c.event.new |> withDefault "unkown" |> nid2eor
                        in
                        viewTensionArrow "margin-left-25" emitter receiver

                    TensionEvent.MemberLinked ->
                        let
                            user =
                                c.event.new |> withDefault "unkown"

                            isYou =
                                user == model.form.uctx.username

                            role =
                                { name = withDefault "" n.name
                                , nameid =
                                    if op.receiverid == "" then
                                        ""

                                    else
                                        Maybe.map (\x -> nodeIdCodec op.receiverid x NodeType.Role) n.nameid |> withDefault ""
                                , role_type = withDefault RoleType.Peer n.role_type
                                , color = n.color
                                }

                            baseUri =
                                uriFromNameid MandateBaseUri role.nameid [ c.tension.id ]
                        in
                        div [ class "subtitle" ]
                            ([ viewUserFull 1 True True { username = user, name = Nothing } ]
                                ++ (if isYou then
                                        [ text ",", br [] [], text T.youHaveBeenInvitedRole, text " " ]

                                    else
                                        [ text " ", text T.userHasBeenInvitedRole, text " " ]
                                   )
                                ++ [ span [ class "is-text-aligned" ] [ viewRole "mt-4" False False Nothing (Just baseUri) (\_ _ _ -> NoMsg) role ] ]
                            )

                    TensionEvent.UserJoined ->
                        let
                            user =
                                c.event.new |> withDefault "unkown"

                            isYou =
                                user == model.form.uctx.username
                        in
                        span []
                            ([ viewUserFull 1 True True { username = user, name = Nothing } ]
                                ++ (if isYou then
                                        [ text ",", br [] [], br [] [], text T.youHaveBeenInvitedOrga ]

                                    else
                                        [ text " ", text T.userHasBeenInvitedOrga ]
                                   )
                            )

                    _ ->
                        text T.notImplemented
                ]
            ]
        , div [ class "field pb-2" ] [ span [ class "is-pulled-right" ] [ text (T.created ++ space_), byAt model.conf c.createdBy c.createdAt ] ]
        , div [ class "" ] <|
            case c.status of
                ContractStatus.Closed ->
                    [ span [ class "has-text-success" ] [ text T.closedContract ] ]

                ContractStatus.Canceled ->
                    [ span [ class "has-text-warning" ] [ text T.canceledContract ] ]

                ContractStatus.Open ->
                    []
        ]


viewVoteBox : UserCtx -> Bool -> List String -> List String -> ContractFull -> Model -> Html Msg
viewVoteBox uctx isValidator participants candidates c model =
    let
        isCandidate =
            List.member uctx.username candidates

        hasPendingCandidateVote =
            List.any (\x -> not (List.member x participants)) candidates

        isLoading =
            model.vote_result == LoadingSlowly
    in
    div [ class "mb-5" ]
        [ p [ class "buttons is-centered voteButton" ] <|
            (-- If contract is an user invitation and the candidate vote is waited
             -- Only show a "cancel invitation" button for coordinator.
             if hasPendingCandidateVote && not isCandidate then
                [ div
                    [ class "button is-danger is-rounded"
                    , classList [ ( "is-loading", isLoading && model.voteForm.vote == 0 ) ]
                    , onClick (OnSubmit (not isLoading) <| DoVote 0)
                    ]
                    [ span [ class "mx-4" ] [ text T.cancelInvitation ] ]
                ]

             else
                -- Otherwire show a "Accept/Cancel" buttons
                [ div
                    [ class "button is-success is-light is-rounded"
                    , classList [ ( "is-loading", isLoading && model.voteForm.vote == 1 ) ]
                    , onClick (OnSubmit (not isLoading) <| DoVote 1)
                    ]
                    [ span [ class "mx-4" ] [ text T.accept ] ]
                , div
                    [ class "button is-danger is-light is-rounded"
                    , classList [ ( "is-loading", isLoading && model.voteForm.vote == 0 ) ]
                    , onClick (OnSubmit (not isLoading) <| DoVote 0)
                    ]
                    [ span [ class "mx-4" ] [ text T.decline ] ]
                ]
            )
        , case model.vote_result of
            Failure err ->
                viewGqlErrors err

            _ ->
                text ""
        ]
