module Elm.SubmitGuardTest exposing (suite)

import Components.ActionPanel as ActionPanel
import Components.Comments as Comments
import Components.ConfirmContract as ConfirmContract
import Components.ConfirmOwner as ConfirmOwner
import Components.ContractsPage as ContractsPage
import Components.JoinOrga as JoinOrga
import Components.NodeDoc as NodeDoc
import Components.ProjectColumnModal as ProjectColumnModal
import Components.UserInput as UserInput
import Dict
import Expect exposing (Expectation)
import Form.Help as Help
import Form.NewTension as NT
import Fractale.Form exposing (Ev, initActionForm)
import Fractale.User exposing (UserState(..))
import Html.Attributes as Attr
import Loading exposing (RequestResult(..))
import ModelSchema exposing (Comment, Tension, Username, initUserctx)
import Ports
import Schema.Enum.Lang as Lang
import Schema.Enum.NodeType as NodeType
import Schema.Enum.TensionEvent as TensionEvent
import Schema.Enum.TensionStatus as TensionStatus
import Schema.Enum.TensionType as TensionType
import Session exposing (Apis, GlobalCmd(..), SessionCommon)
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Time
import Url


apis : Apis
apis =
    Apis "" "" "" "" "" "" "test"


session : SessionCommon
session =
    { user = LoggedIn { initUserctx | username = "author" }
    , screen = { w = 1200, h = 800 }
    , theme = Session.SystemTheme
    , lang = Lang.En
    , now = Time.millisToPosix 0
    , url = Url.Url Url.Http "localhost" Nothing "/" Nothing Nothing
    , query = Dict.empty
    , viewMode = Session.DesktopView
    , node_focus = Nothing
    , path_data = Nothing
    , lexicon = Dict.empty
    , scrollPosition = Ports.ScrollTop
    , file_server_url = ""
    }


comment : Comment
comment =
    Comment "0xc" "2026-01-01T00:00:00Z" Nothing (Username "author") "- [ ] task" [] []


tension : Tension
tension =
    Tension "0xt"
        comment.createdAt
        comment.createdBy
        "Subject"
        TensionType.Operational
        Nothing
        { name = "Org", nameid = "org", role_type = Nothing, color = Nothing }
        Nothing
        Nothing
        TensionStatus.Open
        Nothing
        Nothing


helpStep : Help.Msg -> Help.State -> Help.State
helpStep msg =
    Help.update apis msg >> Tuple.first


commentsStep : Comments.Msg -> Comments.State -> Comments.State
commentsStep msg =
    Comments.update apis msg >> Tuple.first


ntStep : NT.Msg -> NT.Model -> NT.Model
ntStep msg =
    NT.update_ apis msg >> Tuple.first


readyHelp : Help.State
readyHelp =
    List.foldl helpStep
        (Help.init session)
        [ Help.OnChangePostAsk "title" "Question"
        , Help.OnChangePostAsk "message" "Question draft"
        , Help.OnChangePostFeedback "title" "Feedback"
        , Help.OnChangePostFeedback "message" "Feedback draft"
        ]


editingComment : Comments.State
editingComment =
    List.foldl commentsStep
        (Comments.init "org" tension.id session)
        [ Comments.SetComments [ comment, { comment | id = "0xedit" } ]
        , Comments.OnUpdateComment { comment | id = "0xedit" }
        , Comments.OnChangePatchComment "message" "Editor draft"
        ]


invitingRole : NT.Model
invitingRole =
    List.foldl ntStep
        (NT.initModel session)
        [ NT.OnSwitchTab NT.NewRoleTab
        , NT.OnChangeNodeStep NT.NodeValidateStep
        , NT.OnChangePost "name" "New role"
        , NT.OnChangePost "purpose" "Purpose"
        , NT.DoInvite
        , NT.InviteInputMsg (UserInput.OnClickUser { username = "guest", name = Nothing })
        ]


roleDraft : NT.Model
roleDraft =
    List.foldl ntStep
        (NT.initModel session)
        [ NT.OnSwitchTab NT.NewRoleTab
        , NT.OnChangePost "name" "New role"
        , NT.OnChangePost "purpose" "Purpose"
        ]


createdRole : NT.Model
createdRole =
    invitingRole
        |> ntStep (NT.OnSubmitTension True session.now)
        |> ntStep (NT.OnTensionAck (Success { tension = tension, initial_cid = Nothing }))


expectNoOp : model -> ( model, { a | cmds : List cmd, gcmds : List gcmd, result : Maybe result } ) -> Expectation
expectNoOp before ( after, out ) =
    Expect.equal ( before, ( 0, 0, Nothing ) )
        ( after, ( List.length out.cmds, List.length out.gcmds, out.result ) )


{-| First submit sends one request, the same submit while in flight is a no-op.
-}
expectGuarded : (model -> ( model, { a | cmds : List cmd, gcmds : List gcmd, result : Maybe result } )) -> model -> Expectation
expectGuarded submit before =
    let
        ( busy, out ) =
            submit before
    in
    Expect.all
        [ \_ -> Expect.equal 1 (List.length out.cmds)
        , \_ -> submit busy |> expectNoOp busy
        ]
        ()


suite : Test
suite =
    describe "Operation-specific submit guards"
        [ describe "Help"
            ([ ( "question", "AskQuestion", Help.OnAskFeedback LoadingSlowly )
             , ( "feedback", "Feedback", Help.OnAskAck LoadingSlowly )
             ]
                |> List.map
                    (\( name, tab, busyOther ) ->
                        test (name ++ " submits while the other form is busy") <|
                            \_ ->
                                let
                                    before =
                                        readyHelp |> helpStep busyOther |> helpStep (Help.OnOpen tab)
                                in
                                case
                                    Help.view {} before
                                        |> Query.fromHtml
                                        |> Query.find [ Selector.id "helpModal" ]
                                        |> Query.find [ Selector.tag "button", Selector.class "is-success" ]
                                        |> Event.simulate Event.click
                                        |> Event.toResult
                                of
                                    Ok ((Help.OnSubmit True next) as click) ->
                                        let
                                            ( gated, out ) =
                                                Help.update apis click before

                                            submitted =
                                                helpStep (next session.now) gated
                                        in
                                        Expect.all
                                            [ \_ -> Expect.equal 1 (List.length out.cmds)
                                            , \_ ->
                                                Help.view {} submitted
                                                    |> Query.fromHtml
                                                    |> Query.find [ Selector.id "helpModal" ]
                                                    |> Query.find [ Selector.tag "button", Selector.class "is-success" ]
                                                    |> Query.has [ Selector.class "is-loading", Selector.attribute (Attr.disabled True) ]
                                            ]
                                            ()

                                    _ ->
                                        Expect.fail "Expected an enabled submit event"
                    )
            )
        , test "Help rejects busy or incomplete posts before changing either form" <|
            \_ ->
                [ ( Help.OnSubmitAsk session.now, readyHelp |> helpStep (Help.OnAskAck Loading) )
                , ( Help.OnSubmitFeedback session.now, readyHelp |> helpStep (Help.OnAskFeedback LoadingSlowly) )
                , ( Help.OnSubmitAsk session.now, readyHelp |> helpStep (Help.OnChangePostAsk "message" "  ") )
                , ( Help.OnSubmitFeedback session.now, readyHelp |> helpStep (Help.OnChangePostFeedback "title" "") )
                ]
                    |> List.map (\( msg, state ) -> \_ -> Help.update apis msg state |> expectNoOp state)
                    |> (\checks -> Expect.all checks ())
        , test "feedback success clears only feedback and preserves a busy question draft" <|
            \_ ->
                let
                    done =
                        readyHelp
                            |> helpStep (Help.OnSubmitAsk session.now)
                            |> helpStep (Help.OnSubmitFeedback session.now)
                            |> helpStep (Help.OnAskFeedback (Success { tension = tension, initial_cid = Nothing }))

                    question =
                        done |> helpStep (Help.OnOpen "AskQuestion") |> Help.view {} |> Query.fromHtml
                in
                Expect.all
                    [ \_ -> question |> Query.find [ Selector.tag "input", Selector.class "input" ] |> Query.has [ Selector.attribute (Attr.value "Question") ]
                    , \_ -> question |> Query.find [ Selector.tag "textarea" ] |> Query.has [ Selector.attribute (Attr.value "Question draft") ]
                    , \_ -> question |> Query.find [ Selector.id "helpModal" ] |> Query.find [ Selector.tag "button", Selector.class "is-success" ] |> Query.has [ Selector.class "is-loading" ]
                    , \_ -> Help.update apis (Help.OnSubmitFeedback session.now) done |> expectNoOp done
                    ]
                    ()
        , test "Comments protects the editor and checkbox backup before mutation" <|
            \_ ->
                [ Loading, LoadingSlowly ]
                    |> List.concatMap
                        (\busy ->
                            let
                                state =
                                    commentsStep (Comments.CommentPatchAck busy) editingComment
                            in
                            [ Comments.OnCheckbox { cid = comment.id, position = 0, isChecked = True }
                            , Comments.OnDeleteComment comment.id
                            , Comments.SubmitCommentPatch session.now
                            , Comments.SubmitContractComment session.now
                            ]
                                |> List.map (\msg -> \_ -> Comments.update apis msg state |> expectNoOp state)
                        )
                    |> (\checks -> Expect.all checks ())
        , test "checkbox edits remain independent of a pending tension comment and restore the editor" <|
            \_ ->
                let
                    checked =
                        editingComment
                            |> commentsStep (Comments.TensionCommentAck LoadingSlowly)
                            |> commentsStep (Comments.OnCheckbox { cid = comment.id, position = 0, isChecked = True })

                    ( gated, out ) =
                        Comments.update apis (Comments.OnSubmit True Comments.SubmitCommentPatch) checked

                    ( submitted, patchOut ) =
                        Comments.update apis (Comments.SubmitCommentPatch session.now) gated

                    restored =
                        commentsStep (Comments.CommentPatchAck (Success { comment | message = "- [x] task" })) submitted
                in
                Expect.all
                    [ \_ -> Expect.equal ( 1, 1 ) ( List.length out.cmds, List.length patchOut.cmds )
                    , \_ -> Comments.update apis (Comments.SubmitCommentPatch (Time.millisToPosix 1000)) submitted |> expectNoOp submitted
                    , \_ ->
                        Comments.viewCommentsContract session restored
                            |> Query.fromHtml
                            |> Query.find [ Selector.id "updateCommentInput" ]
                            |> Query.has [ Selector.attribute (Attr.value "Editor draft") ]
                    ]
                    ()
        , test "contract comments submit independently of a pending tension comment" <|
            \_ ->
                let
                    before =
                        editingComment
                            |> commentsStep (Comments.TensionCommentAck LoadingSlowly)
                            |> commentsStep (Comments.OnChangeContractComment "message" "Contract comment")

                    ( gated, gateOut ) =
                        Comments.update apis (Comments.OnSubmit True Comments.SubmitContractComment) before

                    ( submitted, out ) =
                        Comments.update apis (Comments.SubmitContractComment session.now) gated
                in
                Expect.all
                    [ \_ -> Expect.equal ( 1, 1 ) ( List.length gateOut.cmds, List.length out.cmds )
                    , \_ -> Comments.update apis (Comments.SubmitContractComment (Time.millisToPosix 1000)) submitted |> expectNoOp submitted
                    ]
                    ()
        , test "tension comments submit independently, reject duplicates, and still allow status-only changes" <|
            \_ ->
                let
                    before =
                        editingComment
                            |> commentsStep (Comments.CommentPatchAck LoadingSlowly)
                            |> commentsStep (Comments.OnChangeComment "message" "New comment")

                    ( gated, gateOut ) =
                        Comments.update apis (Comments.OnSubmit True (Comments.SubmitTensionComment Nothing)) before

                    ( submitted, out ) =
                        Comments.update apis (Comments.SubmitTensionComment Nothing session.now) gated

                    done =
                        commentsStep (Comments.TensionCommentAck (Success { comments = Just [ comment ], blobs = Nothing })) submitted

                    ( _, statusOut ) =
                        Comments.update apis (Comments.SubmitTensionComment (Just TensionStatus.Closed) session.now) done
                in
                Expect.all
                    [ \_ -> Expect.equal ( 1, 1, 1 ) ( List.length gateOut.cmds, List.length out.cmds, List.length statusOut.cmds )
                    , \_ -> Comments.update apis (Comments.SubmitTensionComment Nothing session.now) submitted |> expectNoOp submitted
                    , \_ -> Comments.update apis (Comments.SubmitTensionComment Nothing session.now) done |> expectNoOp done
                    , \_ ->
                        Comments.viewTensionCommentInput session tension submitted
                            |> Query.fromHtml
                            |> Query.find [ Selector.class "buttons" ]
                            |> Query.children [ Selector.tag "button" ]
                            |> Query.each (Query.has [ Selector.attribute (Attr.disabled True) ])
                    ]
                    ()
        , test "status-only failures stay visible and close/reopen retries remain enabled" <|
            \_ ->
                [ ( TensionStatus.Open, TensionStatus.Closed ), ( TensionStatus.Closed, TensionStatus.Open ) ]
                    |> List.map
                        (\( status, nextStatus ) ->
                            \_ ->
                                let
                                    failed =
                                        Comments.init "org" tension.id session
                                            |> commentsStep (Comments.SubmitTensionComment (Just nextStatus) session.now)
                                            |> commentsStep (Comments.TensionCommentAck (Failure [ "Status failed" ]))

                                    view =
                                        Comments.viewTensionCommentInput session { status = status } failed |> Query.fromHtml

                                    button =
                                        view |> Query.find [ Selector.class "buttons" ] |> Query.children [ Selector.tag "button" ] |> Query.first
                                in
                                Expect.all
                                    [ \_ -> view |> Query.has [ Selector.text "Status failed" ]
                                    , \_ -> view |> Query.find [ Selector.class "defaultSubmit" ] |> Query.has [ Selector.attribute (Attr.disabled True) ]
                                    , \_ -> button |> Query.has [ Selector.attribute (Attr.disabled False) ]
                                    , \_ ->
                                        button
                                            |> Event.simulate Event.click
                                            |> Event.toResult
                                            |> Result.map (\click -> Comments.update apis click failed |> Tuple.second |> .cmds |> List.length)
                                            |> Expect.equal (Ok 1)
                                    ]
                                    ()
                        )
                    |> (\checks -> Expect.all checks ())
        , test "NewTension rejects duplicate creation while pending or done" <|
            \_ ->
                [ Loading, LoadingSlowly, Success tension ]
                    |> List.concatMap
                        (\result ->
                            let
                                before =
                                    { invitingRole | result = result }
                            in
                            [ NT.OnSubmitTension False session.now, NT.OnSubmitTension True session.now ]
                                |> List.map (\msg -> \_ -> NT.update_ apis msg before |> expectNoOp before)
                        )
                    |> (\checks -> Expect.all checks ())
        , test "creation validates the active tab's fields before submitting" <|
            \_ ->
                let
                    tensionDraft =
                        NT.initModel session |> ntStep (NT.OnChangePost "title" "Draft")

                    blankTitle =
                        NT.initModel session |> ntStep (NT.OnChangePost "title" "  ")

                    freshRoleTab =
                        NT.initModel session |> ntStep (NT.OnSwitchTab NT.NewRoleTab)

                    roleNoPurpose =
                        freshRoleTab |> ntStep (NT.OnChangePost "name" "New role")

                    circleDraft =
                        roleDraft |> ntStep (NT.OnSwitchTab NT.NewCircleTab)

                    submitted =
                        NT.update_ apis (NT.OnSubmitTension False session.now) tensionDraft
                in
                Expect.all
                    [ \_ -> Expect.equal ( LoadingSlowly, 1 ) ( Tuple.first submitted |> .result, Tuple.second submitted |> .cmds |> List.length )
                    , \_ -> NT.update_ apis (NT.OnSubmitTension False session.now) (NT.initModel session) |> expectNoOp (NT.initModel session)
                    , \_ -> NT.update_ apis (NT.OnSubmitTension False session.now) blankTitle |> expectNoOp blankTitle
                    , \_ -> NT.update_ apis (NT.OnSubmitTension False session.now) freshRoleTab |> expectNoOp freshRoleTab
                    , \_ -> NT.update_ apis (NT.OnSubmitTension False session.now) roleNoPurpose |> expectNoOp roleNoPurpose
                    , \_ -> Expect.equal LoadingSlowly (Tuple.first (NT.update_ apis (NT.OnSubmitTension False session.now) roleDraft)).result
                    , \_ -> Expect.equal LoadingSlowly (Tuple.first (NT.update_ apis (NT.OnSubmitTension False session.now) circleDraft)).result
                    ]
                    ()
        , test "failed role invitation retries only the invitation" <|
            \_ ->
                let
                    failed =
                        createdRole
                            |> ntStep (NT.OnInvite session.now)
                            |> ntStep (NT.PushAck (Failure [ "Invitation failed" ]))
                            |> ntStep (NT.OnChangePost "title" "")

                    button =
                        NT.viewStep NotAsked (NT.State failed)
                            |> Query.fromHtml
                            |> Query.find [ Selector.tag "button", Selector.class "defaultSubmit" ]
                in
                case button |> Event.simulate Event.click |> Event.toResult of
                    Ok (NT.OnSubmit True next) ->
                        case next session.now of
                            NT.OnInvite time ->
                                let
                                    ( retried, out ) =
                                        NT.update_ apis (NT.OnInvite time) failed
                                in
                                Expect.all
                                    [ \_ -> button |> Query.has [ Selector.attribute (Attr.disabled False) ]
                                    , \_ -> Expect.equal ( Success tension, tension.id, LoadingSlowly ) ( retried.result, retried.nodeDoc.form.id, retried.action_result )
                                    , \_ -> Expect.equal 1 (List.length out.cmds)
                                    , \_ -> NT.update_ apis (NT.OnSubmitTension True time) retried |> expectNoOp retried
                                    ]
                                    ()

                            _ ->
                                Expect.fail "Retry must invite, not create another tension"

                    _ ->
                        Expect.fail "Expected an enabled invitation retry despite empty creation fields"
        , test "invitation guards preserve state for busy, completed, or invalid contexts" <|
            \_ ->
                [ { createdRole | action_result = Loading }
                , { createdRole | action_result = LoadingSlowly }
                , { createdRole | action_result = Success { id = "0xcontract" } }
                , { createdRole | result = NotAsked }
                , { createdRole | doInvite = False }
                , { createdRole | nodeDoc = NodeDoc.setUsers [] createdRole.nodeDoc }
                ]
                    |> List.map (\before -> \_ -> NT.update_ apis (NT.OnInvite session.now) before |> expectNoOp before)
                    |> (\checks -> Expect.all checks ())
        , test "invitation token refresh and authentication permit retries while creation stays guarded" <|
            \_ ->
                let
                    busy =
                        ntStep (NT.OnInvite session.now) createdRole

                    ( refreshed, refreshOut ) =
                        NT.update_ apis (NT.PushAck (Failure [ "access denied" ])) busy

                    ( retried, retryOut ) =
                        NT.update_ apis (NT.OnInvite session.now) refreshed

                    authenticated =
                        busy |> ntStep (NT.PushAck (Failure [ "token is expired" ])) |> ntStep (NT.UpdateUctx { initUserctx | username = "author" })

                    completed =
                        ntStep (NT.PushAck (Success { id = "0xcontract" })) retried
                in
                Expect.all
                    [ \_ -> Expect.equal ( 1, NotAsked, [ DoUpdateToken ] ) ( refreshed.refresh_trial, refreshed.action_result, refreshOut.gcmds )
                    , \_ -> Expect.equal ( LoadingSlowly, Success tension, 1 ) ( retried.action_result, retried.result, List.length retryOut.cmds )
                    , \_ -> Expect.equal LoadingSlowly (ntStep (NT.OnInvite session.now) authenticated).action_result
                    , \_ -> NT.update_ apis (NT.OnInvite session.now) retried |> expectNoOp retried
                    , \_ -> NT.update_ apis (NT.OnInvite session.now) completed |> expectNoOp completed
                    ]
                    ()
        , test "creation dispatch remains a valid busy continuation and no-invitation success cannot invite" <|
            \_ ->
                let
                    pending =
                        NT.initModel session
                            |> ntStep (NT.OnChangePost "title" "Draft")
                            |> ntStep (NT.OnSubmitTension False session.now)

                    ( _, pushOut ) =
                        NT.update_ apis (NT.PushTension NT.OnTensionAck) pending

                    done =
                        ntStep (NT.OnTensionAck (Success { tension = tension, initial_cid = Nothing })) pending
                in
                Expect.all
                    [ \_ -> Expect.equal ( LoadingSlowly, 1 ) ( pending.result, List.length pushOut.cmds )
                    , \_ -> NT.update_ apis (NT.OnSubmitTension False session.now) pending |> expectNoOp pending
                    , \_ -> NT.update_ apis (NT.OnInvite session.now) done |> expectNoOp done
                    , \_ -> Expect.equal NotAsked (ntStep NT.OnReset done).result
                    ]
                    ()
        , describe "ConfirmOwner"
            [ test "make-owner rejects empty targets and double submits" <|
                \_ ->
                    let
                        focus =
                            { rootnameid = "org", nameid = "org#circle", type_ = NodeType.Circle }

                        fresh =
                            ConfirmOwner.init focus session

                        ready =
                            Tuple.first (ConfirmOwner.update apis (ConfirmOwner.OnOpen "guest") fresh)

                        ( submitted, out ) =
                            ConfirmOwner.update apis (ConfirmOwner.OnMakeOwner session.now) ready
                    in
                    Expect.all
                        [ \_ -> Expect.equal 1 (List.length out.cmds)
                        , \_ -> ConfirmOwner.update apis (ConfirmOwner.OnMakeOwner session.now) submitted |> expectNoOp submitted
                        , \_ -> ConfirmOwner.update apis (ConfirmOwner.OnMakeOwner session.now) fresh |> expectNoOp fresh
                        ]
                        ()
            ]
        , describe "creation guards"
            [ test "ActionPanel" <| \_ -> expectGuarded (ActionPanel.update apis (ActionPanel.OnActionSubmit session.now)) (ActionPanel.init session)
            , test "ConfirmContract" <| \_ -> expectGuarded (ConfirmContract.update apis ConfirmContract.DoAddContract) (ConfirmContract.init session)
            , test "ProjectColumnModal" <| \_ -> expectGuarded (ProjectColumnModal.update apis ProjectColumnModal.OnColAdd) (ProjectColumnModal.init "" session)
            , test "JoinOrga" <|
                \_ ->
                    let
                        form =
                            initActionForm tension.id session.user
                    in
                    expectGuarded
                        (JoinOrga.update apis (JoinOrga.PushGuest { form | users = [ { username = "guest", name = Nothing, email = "", pattern = "" } ], events = [ Ev TensionEvent.UserJoined "" "guest" ] }))
                        (JoinOrga.init "org" session)
            ]
        , describe "ContractsPage"
            [ test "votes reject double submits and resume after a token refresh" <|
                \_ ->
                    let
                        fresh =
                            ContractsPage.init "org" session

                        ( pending, out ) =
                            ContractsPage.update apis (ContractsPage.DoVote 1 session.now) fresh

                        ( refreshed, refreshOut ) =
                            ContractsPage.update apis (ContractsPage.OnVoteAck (Failure [ "access denied" ])) pending

                        ( _, retryOut ) =
                            ContractsPage.update apis (ContractsPage.DoVote 1 session.now) refreshed
                    in
                    Expect.all
                        [ \_ -> Expect.equal 1 (List.length out.cmds)
                        , \_ -> ContractsPage.update apis (ContractsPage.DoVote 1 session.now) pending |> expectNoOp pending
                        , \_ -> Expect.equal [ DoUpdateToken ] refreshOut.gcmds
                        , \_ -> Expect.equal 1 (List.length retryOut.cmds)
                        ]
                        ()
            ]
        ]
