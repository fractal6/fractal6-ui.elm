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


module Components.Comments exposing
    ( Msg(..)
    , OutType(..)
    , State
    , init
    , subscriptions
    , update
    , viewCommentsContract
    , viewCommentsTension
    , viewContractCommentInput
    , viewNewTensionCommentInput
    , viewTensionCommentInput
    )

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Bulk exposing (CommentPatchForm, Ev, InputViewMode(..), TensionForm, UserState(..), eventFromForm, initCommentPatchForm, initTensionForm, pushCommentReaction, removeCommentReaction, uctxFromUser)
import Bulk.Codecs exposing (DocType(..), FractalBaseRoute(..), getTensionCharac, nid2rootid, tensionAction2NodeType, toLink, uriFromNameid)
import Bulk.Error exposing (viewGqlErrors)
import Bulk.View exposing (action2str, statusColor, tensionIcon2, tensionStatus2str, viewLabel, viewNodeRefShort, viewTensionDateAndUserC, viewUpdated, viewUser0, viewUser2, viewUsernameLink)
import Components.UserInput as UserInput
import Dict
import Extra exposing (decap, ternary, textD)
import Extra.Date exposing (formatDate)
import Extra.Events exposing (onClickSafe)
import Form exposing (isPostSendable)
import Fractal.Enum.Lang as Lang
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Generated.Route as Route exposing (toHref)
import Global exposing (sendNow)
import Html exposing (Html, a, br, button, div, hr, i, li, p, span, strong, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, placeholder, rows, style, target, value)
import Html.Events exposing (onClick, onInput)
import Html.Lazy as Lazy
import Iso8601 exposing (fromTime)
import List.Extra as LE
import Loading exposing (GqlData, RequestResult(..), withMapData, withMaybeMapData)
import Markdown exposing (renderMarkdown)
import Maybe exposing (withDefault)
import ModelSchema exposing (Comment, Event, Label, PatchTensionPayloadID, ReactionResponse, TensionHead, UserCtx)
import Ports
import Query.PatchContract exposing (pushContractComment)
import Query.PatchTension exposing (patchComment, pushTensionPatch)
import Query.Reaction exposing (addReaction, deleteReaction)
import Session exposing (Apis, Conf, GlobalCmd, isMobile, toReflink)
import String.Extra as SE
import String.Format as Format
import Text as T
import Time



-- ------------------------------
-- M O D E L
-- ------------------------------


type State
    = State Model


type alias TensionCommon a =
    { a | status : TensionStatus.TensionStatus }


type alias EventTracker =
    { type_ : Maybe TensionEvent.TensionEvent
    , createdAt : String
    , i : Int
    , n : Int
    }


type alias Model =
    { user : UserState
    , focusid : String
    , tensionid : String
    , comments : List Comment
    , history : List Event
    , expandedEvents : List Int

    -- Push comment (Tension)
    , tension_form : TensionForm
    , contract_form : CommentPatchForm
    , tension_patch : GqlData PatchTensionPayloadID

    -- Edit Comment (Tension & Contract)
    , comment_form : CommentPatchForm
    , comment_result : GqlData Comment

    -- Components
    , userInput : UserInput.State

    -- Common
    , refresh_trial : Int -- use to refresh user token
    }


initModel : String -> String -> UserState -> Model
initModel nameid tensionid user =
    { user = user
    , focusid = nameid
    , tensionid = tensionid
    , comments = []
    , history = []
    , expandedEvents = []
    , tension_form = initTensionForm tensionid Nothing user
    , tension_patch = NotAsked
    , contract_form = initCommentPatchForm user []
    , comment_form = initCommentPatchForm user [ ( "focusid", nameid ) ]
    , comment_result = NotAsked

    -- Components
    , userInput = UserInput.init [ nameid ] False False user

    -- Common
    , refresh_trial = 0
    }


init : String -> String -> UserState -> State
init nameid tensionid user =
    initModel nameid tensionid user |> State



-- Global methods
-- not yet
-- State Controls


resetModel : Model -> Model
resetModel model =
    initModel model.tensionid model.focusid model.user


type Msg
    = ExpandEvent Int
    | SetTensionid String
    | SetContractid String
    | SetComments (List Comment)
    | SetHistory (List Event)
    | PushEvents (List Event)
      -- Change Post
    | OnChangeComment String String
    | OnChangeContractComment String String
    | OnChangeCommentPatch String String
      -- Push Comment
    | SubmitTensionComment (Maybe TensionStatus.TensionStatus) Time.Posix
    | TensionCommentAck (GqlData PatchTensionPayloadID)
    | SubmitContractComment Time.Posix
    | ContractCommentAck (GqlData Comment)
      -- Edit comment
    | OnUpdateComment Comment
    | OnCancelComment String
    | SubmitCommentPatch Time.Posix
    | CommentPatchAck (GqlData Comment)
      -- Reaction
    | OnAddReaction String Int
    | OnAddReactionAck (GqlData ReactionResponse)
    | OnDeleteReaction String Int
    | OnDeleteReactionAck (GqlData ReactionResponse)
      -- Common
    | OnSubmit Bool (Time.Posix -> Msg)
    | NoMsg
    | LogErr String
    | ChangeInputViewMode InputViewMode
    | ChangeContractInputViewMode InputViewMode
    | ChangeUpdateViewMode InputViewMode
    | OnRichText String String
    | OnToggleMdHelp String
      -- Components
    | UserInputMsg UserInput.Msg


type alias Out =
    { cmds : List (Cmd Msg)
    , gcmds : List GlobalCmd
    , result : Maybe OutType
    }


type OutType
    = TensionCommentAdded (Maybe TensionStatus.TensionStatus)
    | PostChanged ( String, String )


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
        OnSubmit isSendable next ->
            if isSendable then
                ( model, out0 [ sendNow next ] )

            else
                ( model, noOut )

        ExpandEvent i ->
            -- @fix/bulma: dropdown clidk handler lost during the operation
            ( { model | expandedEvents = model.expandedEvents ++ [ i ] }, out0 [ Ports.bulma_driver "" ] )

        SetTensionid tensionid ->
            let
                form =
                    model.tension_form

                tension_form =
                    { form | id = tensionid }
            in
            ( { model | tension_form = tension_form }, noOut )

        SetContractid contractid ->
            let
                form =
                    model.contract_form

                contract_form =
                    { form | post = form.post |> Dict.insert "contractid" contractid }
            in
            ( { model | contract_form = contract_form }, noOut )

        SetComments comments ->
            ( { model | comments = comments }, out0 [ Ports.bulma_driver "" ] )

        SetHistory history ->
            ( { model | history = history }, noOut )

        PushEvents events ->
            -- @todo: update tension_head history here (need to create a Session.Cmd to handle this.
            ( { model | history = model.history ++ events }, noOut )

        OnChangeComment field value ->
            let
                form =
                    model.tension_form

                tension_form =
                    if field == "message" && value == "" then
                        { form | post = Dict.remove field form.post }

                    else
                        { form | post = Dict.insert field value form.post }
            in
            ( { model | tension_form = tension_form }, Out [] [] (Just (PostChanged ( field, value ))) )

        OnChangeContractComment field value ->
            let
                form =
                    model.contract_form

                contract_form =
                    if field == "message" && value == "" then
                        { form | post = Dict.remove field form.post }

                    else
                        { form | post = Dict.insert field value form.post }
            in
            ( { model | contract_form = contract_form }, Out [] [] (Just (PostChanged ( field, value ))) )

        SubmitTensionComment status_m time ->
            let
                form =
                    model.tension_form

                eventStatus =
                    case status_m of
                        Just TensionStatus.Open ->
                            [ Ev TensionEvent.Reopened "Closed" "Open" ]

                        Just TensionStatus.Closed ->
                            [ Ev TensionEvent.Closed "Open" "Closed" ]

                        Nothing ->
                            []

                tension_form =
                    { form
                        | post = Dict.insert "createdAt" (fromTime time) form.post
                        , status = status_m
                        , events = eventStatus
                    }
            in
            ( { model | tension_form = tension_form, tension_patch = LoadingSlowly }
            , out0 [ pushTensionPatch apis tension_form TensionCommentAck ]
            )

        TensionCommentAck result ->
            case parseErr result 2 of
                OkAuth tp ->
                    let
                        resetForm =
                            initTensionForm model.tensionid Nothing model.user
                    in
                    ( { model
                        | comments =
                            if (Dict.get "message" model.tension_form.post |> withDefault "") /= "" then
                                model.comments ++ withDefault [] tp.comments

                            else
                                model.comments
                        , history =
                            model.history
                                ++ (model.tension_form.events |> List.map (\e -> eventFromForm e model.tension_form))
                        , tension_form = resetForm
                        , tension_patch = result
                      }
                    , Out [ Ports.bulma_driver "" ] [] (Just (TensionCommentAdded model.tension_form.status))
                    )

                _ ->
                    case result of
                        Failure _ ->
                            let
                                form =
                                    model.tension_form

                                resetForm =
                                    { form | status = Nothing }
                            in
                            ( { model | tension_patch = result, tension_form = resetForm }, noOut )

                        _ ->
                            ( { model | tension_patch = result }, noOut )

        SubmitContractComment time ->
            let
                form =
                    model.contract_form

                contract_form =
                    { form | post = form.post |> Dict.insert "createdAt" (fromTime time) }
            in
            ( { model
                | contract_form = contract_form
                , comment_result = LoadingSlowly
              }
            , out0 [ pushContractComment apis contract_form ContractCommentAck ]
            )

        ContractCommentAck result ->
            case parseErr result 2 of
                OkAuth comment ->
                    let
                        resetForm =
                            initCommentPatchForm model.user []
                    in
                    ( { model
                        | comments =
                            if (Dict.get "message" model.contract_form.post |> withDefault "") /= "" then
                                model.comments ++ [ comment ]

                            else
                                model.comments
                        , contract_form = resetForm
                        , comment_result = result
                      }
                    , out0 [ Ports.bulma_driver "" ]
                    )

                _ ->
                    ( { model | comment_result = result }, noOut )

        OnUpdateComment c ->
            let
                form =
                    model.comment_form
            in
            ( { model | comment_form = { form | id = c.id } }, out0 [ Ports.focusOn "updateCommentInput", Ports.bulma_driver c.createdAt ] )

        OnCancelComment createdAt ->
            let
                form =
                    model.comment_form
            in
            ( { model | comment_form = { form | id = "", post = Dict.remove "message" form.post }, comment_result = NotAsked }, out0 [ Ports.bulma_driver createdAt ] )

        OnChangeCommentPatch field value ->
            let
                form =
                    model.comment_form
            in
            ( { model | comment_form = { form | post = Dict.insert field value form.post } }, noOut )

        SubmitCommentPatch time ->
            let
                form =
                    model.comment_form

                comment_form =
                    { form | post = Dict.insert "updatedAt" (fromTime time) form.post }
            in
            ( { model | comment_form = comment_form, comment_result = LoadingSlowly }
            , out0 [ patchComment apis comment_form CommentPatchAck ]
            )

        CommentPatchAck result ->
            case parseErr result 2 of
                OkAuth comment ->
                    let
                        comments =
                            let
                                n =
                                    model.comments
                                        |> LE.findIndex (\c -> c.id == comment.id)
                                        |> withDefault -1
                            in
                            LE.setAt n comment model.comments

                        resetForm =
                            initCommentPatchForm model.user [ ( "focusid", model.focusid ) ]
                    in
                    ( { model | comments = comments, comment_form = resetForm, comment_result = result }, out0 [ Ports.bulma_driver comment.createdAt ] )

                _ ->
                    ( { model | comment_result = result }, noOut )

        -- Common
        NoMsg ->
            ( model, noOut )

        LogErr err ->
            ( model, out0 [ Ports.logErr err ] )

        ChangeInputViewMode viewMode ->
            let
                form =
                    model.tension_form
            in
            ( { model | tension_form = { form | viewMode = viewMode } }, noOut )

        ChangeContractInputViewMode viewMode ->
            let
                form =
                    model.contract_form
            in
            ( { model | contract_form = { form | viewMode = viewMode } }, noOut )

        ChangeUpdateViewMode viewMode ->
            let
                form =
                    model.comment_form
            in
            ( { model | comment_form = { form | viewMode = viewMode } }, noOut )

        OnRichText targetid command ->
            ( model, out0 [ Ports.richText targetid command ] )

        OnToggleMdHelp targetid ->
            case targetid of
                "commentInput" ->
                    let
                        form =
                            model.tension_form

                        field =
                            "isMdHelpOpen" ++ targetid

                        v =
                            Dict.get field form.post |> withDefault "false"

                        value =
                            ternary (v == "true") "false" "true"
                    in
                    ( { model | tension_form = { form | post = Dict.insert field value form.post } }, noOut )

                "updateCommentInput" ->
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
                    ( { model | comments = pushCommentReaction uctx.username r model.comments }, noOut )

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
                    ( { model | comments = removeCommentReaction uctx.username r model.comments }, noOut )

                _ ->
                    ( model, noOut )

        -- Components
        UserInputMsg msg ->
            let
                ( data, out ) =
                    UserInput.update apis msg model.userInput

                cmd =
                    case out.result of
                        Just ( selected, us ) ->
                            if selected then
                                case us of
                                    [ u ] ->
                                        Ports.pushInputSelection u.username

                                    _ ->
                                        Cmd.none

                            else
                                Cmd.none

                        Nothing ->
                            Cmd.none

                --( cmds, gcmds ) =
                --    mapGlobalOutcmds out.gcmds
            in
            ( { model | userInput = data }, out2 (cmd :: (out.cmds |> List.map (\m -> Cmd.map UserInputMsg m))) out.gcmds )


subscriptions : State -> List (Sub Msg)
subscriptions (State model) =
    UserInput.subscriptions model.userInput |> List.map (\s -> Sub.map UserInputMsg s)



-- ------------------------------
-- V I E W
-- ------------------------------


viewCommentsContract : Conf -> State -> Html Msg
viewCommentsContract conf (State model) =
    model.comments
        |> List.map
            (\c ->
                Lazy.lazy5 viewComment conf c model.comment_form model.comment_result model.userInput
            )
        |> div []


viewCommentsTension : Conf -> Maybe TensionAction.TensionAction -> State -> Html Msg
viewCommentsTension conf action (State model) =
    Lazy.lazy8 viewComments_ conf action model.history model.comments model.comment_form model.comment_result model.expandedEvents model.userInput


viewComments_ :
    Conf
    -> Maybe TensionAction.TensionAction
    -> List Event
    -> List Comment
    -> CommentPatchForm
    -> GqlData Comment
    -> List Int
    -> UserInput.State
    -> Html Msg
viewComments_ conf action history comments comment_form comment_result expandedEvents userInput =
    let
        allEvts =
            -- When event and comment are created at the same time, show the comment first.
            List.indexedMap (\i c -> { type_ = Nothing, createdAt = c.createdAt, i = i, n = 0 }) comments
                ++ List.indexedMap (\i e -> { type_ = Just e.event_type, createdAt = e.createdAt, i = i, n = 0 }) history
                |> List.sortBy .createdAt

        viewCommentOrEvent : EventTracker -> Html Msg
        viewCommentOrEvent e =
            case e.type_ of
                Just _ ->
                    case LE.getAt e.i history of
                        Just event ->
                            viewEvent conf (Dict.get "focusid" comment_form.post) action event

                        Nothing ->
                            text ""

                Nothing ->
                    case LE.getAt e.i comments of
                        Just c ->
                            viewComment conf c comment_form comment_result userInput

                        Nothing ->
                            text ""
    in
    allEvts
        -- Filter events if there a above a given number.
        -- If above, we keep track of the extra number of event
        -- until a non-event (i.e a comment) is met.
        |> LE.indexedFoldr
            (\i e d ->
                let
                    evts =
                        Tuple.first d

                    state =
                        Tuple.second d

                    isAbove =
                        (List.length evts > 6)
                            && (e.type_ /= Nothing)
                            && (evts
                                    |> List.take 6
                                    |> List.filter (\x -> x.type_ == Nothing)
                                    |> List.length
                               )
                            == 0

                    isClicked =
                        state.isClicked || List.member i expandedEvents
                in
                if e.type_ == Just TensionEvent.Created then
                    -- Ignore these type
                    ( evts, state )

                else if isAbove && state.nskip == 0 && not isClicked then
                    ( evts, { state | nskip = 1, i = i } )

                else if isAbove && state.nskip > 0 && not state.isClicked then
                    ( evts, { state | nskip = state.nskip + 1 } )

                else if state.nskip > 0 && e.type_ == Nothing && not state.isClicked then
                    let
                        btn =
                            { type_ = Nothing, n = state.nskip, createdAt = "", i = state.i }
                    in
                    ( [ e ] ++ [ btn ] ++ evts, { state | nskip = 0, isClicked = False } )

                else if e.type_ == Nothing then
                    ( [ e ] ++ evts, { state | nskip = 0, isClicked = False } )

                else
                    ( [ e ] ++ evts, { state | isClicked = isClicked } )
            )
            -- The tuple.first: filterered list of events
            -- The tuple.second: state of the fold loop. We stored the skips when a new comment is
            -- encoutered in order to insert a button later at the current position.
            ( [], { nskip = 0, isCollapsed = True, isClicked = False, i = 0 } )
        |> Tuple.first
        |> List.map
            (\x ->
                if x.n > 0 then
                    div
                        [ class "button is-small actionComment m-4"
                        , attribute "style" "left:10%;"
                        , onClick (ExpandEvent x.i)
                        ]
                        [ text (T.showOlderEvents |> Format.value (String.fromInt x.n)) ]

                else
                    Lazy.lazy viewCommentOrEvent x
            )
        |> div []


viewComment : Conf -> Comment -> CommentPatchForm -> GqlData Comment -> UserInput.State -> Html Msg
viewComment conf c form result userInput =
    let
        isAuthor =
            c.createdBy.username == form.uctx.username

        reflink =
            toReflink conf.url ++ "?goto=" ++ c.createdAt
    in
    div [ id c.createdAt, class "media section is-paddingless" ]
        [ div [ class "media-left is-hidden-mobile" ] [ viewUser2 c.createdBy.username ]
        , div
            [ class "media-content"
            , attribute "style" "width: 66.66667%;"
            ]
            [ if form.id == c.id then
                viewUpdateInput conf c form result userInput

              else
                div [ class "message" ]
                    [ div [ class "message-header has-arrow-left pl-1-mobile", classList [ ( "is-author", isAuthor ) ] ]
                        [ span [ class "is-hidden-tablet" ] [ viewUser0 c.createdBy.username ]
                        , viewTensionDateAndUserC conf c.createdAt c.createdBy
                        , case c.updatedAt of
                            Just updatedAt ->
                                viewUpdated conf updatedAt

                            Nothing ->
                                text ""
                        , div [ class "is-pulled-right" ]
                            [ div [ class "dropdown is-right mr-2" ]
                                [ div [ class "dropdown-trigger is-w is-h" ]
                                    [ div
                                        [ class "ellipsis"
                                        , attribute "aria-controls" ("emoticon-" ++ c.id)
                                        , attribute "aria-haspopup" "true"
                                        ]
                                        [ A.icon "icon-smile icon-bg" ]
                                    ]
                                , div [ id ("emoticon-" ++ c.id), class "dropdown-menu emojis", attribute "role" "menu" ]
                                    [ Extra.emojis
                                        |> List.map (\( i, x, _ ) -> span [ onClick (OnAddReaction c.id i) ] [ text x ])
                                        |> div [ class "dropdown-content" ]
                                    ]
                                ]
                            , div [ class "dropdown is-right" ]
                                [ div [ class "dropdown-trigger is-w is-h" ]
                                    [ div
                                        [ class "ellipsis"
                                        , attribute "aria-controls" ("edit-ellipsis-" ++ c.id)
                                        , attribute "aria-haspopup" "true"
                                        ]
                                        [ A.icon "icon-more-horizontal icon-lg" ]
                                    ]
                                , div [ id ("edit-ellipsis-" ++ c.id), class "dropdown-menu", attribute "role" "menu" ]
                                    [ div [ class "dropdown-content p-0" ] <|
                                        [ div [ class "dropdown-item button-light", attribute "data-clipboard" reflink ] [ text "Copy link" ] ]
                                            ++ (if isAuthor then
                                                    [ hr [ class "dropdown-divider" ] []
                                                    , div [ class "dropdown-item button-light", onClick (OnUpdateComment c) ] [ text T.edit ]
                                                    ]

                                                else
                                                    []
                                               )
                                    ]
                                ]
                            ]
                        ]
                    , div [ class "message-body" ]
                        [ case c.message of
                            "" ->
                                div [ class "help is-italic" ] [ text T.noMessageProvided ]

                            message ->
                                renderMarkdown "is-human" message
                        , div [ class "emoji-reactions" ] <|
                            List.map
                                (\r ->
                                    case List.length r.users of
                                        0 ->
                                            text ""

                                        count ->
                                            let
                                                isSelected =
                                                    List.member form.uctx.username r.users

                                                elmId =
                                                    "emoji-" ++ c.id ++ String.fromInt r.type_
                                            in
                                            span
                                                [ class "tag mr-2 dropdown is-up"
                                                , classList [ ( "is-selected", isSelected ) ]
                                                ]
                                                [ div
                                                    [ class "dropdown-trigger"
                                                    , attribute "aria-controls" elmId
                                                    , attribute "aria-haspopup" "true"
                                                    , if isSelected then
                                                        onClick (OnDeleteReaction c.id r.type_)

                                                      else
                                                        onClick (OnAddReaction c.id r.type_)
                                                    ]
                                                    [ text (Extra.getEmoji r.type_), span [ class "px-1" ] [], text (String.fromInt count) ]
                                                , div [ id elmId, class "dropdown-menu", attribute "role" "menu" ]
                                                    [ div [ class "dropdown-content p-3" ]
                                                        [ span [ class "is-larger4 pr-2" ] [ text (Extra.getEmoji r.type_) ]
                                                        , case LE.unconsLast r.users of
                                                            Just ( u, [] ) ->
                                                                text (u ++ " " ++ T.reactedWith ++ " " ++ Extra.getEmojiName r.type_ ++ " emoji")

                                                            Just ( u, us ) ->
                                                                text (String.join ", " us ++ " " ++ T.and ++ " " ++ u ++ " " ++ T.reactedWith ++ " " ++ Extra.getEmojiName r.type_ ++ " emoji")

                                                            Nothing ->
                                                                text ""
                                                        ]
                                                    ]
                                                ]
                                )
                                c.reactions
                        ]
                    ]
            ]
        ]


viewNewTensionCommentInput : Conf -> State -> Html Msg
viewNewTensionCommentInput conf (State model) =
    div [ class "message" ]
        [ div [ class "message-header" ] [ viewCommentInputHeader "textAreaModal" model.tension_form ]
        , div [ class "message-body" ]
            [ div [ class "field" ]
                [ div [ class "control" ] [ viewCommentTextarea conf "textAreaModal" True T.leaveCommentOpt model.tension_form model.userInput ]
                , p [ class "help-label" ] [ text model.tension_form.txt.message_help ]
                , div [ class "is-hidden-mobile is-pulled-right help", style "font-size" "10px" ] [ text "Tips: <C+Enter> to submit" ]
                , br [ class "is-hidden-mobile" ] []
                ]
            ]
        ]


viewUpdateInput : Conf -> Comment -> CommentPatchForm -> GqlData Comment -> UserInput.State -> Html Msg
viewUpdateInput conf comment form_ result userInput =
    let
        message =
            Dict.get "message" form_.post |> withDefault comment.message

        form =
            { form_ | post = Dict.insert "message" message form_.post }

        isSendable =
            message /= comment.message

        isLoading =
            Loading.isLoading result
    in
    div [ class "message commentInput" ]
        [ div [ class "message-header has-arrow-left" ] [ viewCommentInputHeader "updateCommentInput" form ]
        , div [ class "message-body submitFocus" ]
            [ div [ class "field" ]
                [ div [ class "control" ]
                    [ viewCommentTextarea conf "updateCommentInput" False T.leaveComment form userInput ]
                ]
            , case result of
                Failure err ->
                    viewGqlErrors err

                _ ->
                    text ""
            , div [ class "field is-grouped is-grouped-right" ]
                [ div [ class "control" ]
                    [ div [ class "buttons" ]
                        [ button
                            [ class "button"
                            , onClick (OnCancelComment comment.createdAt)
                            ]
                            [ text T.cancel ]
                        , button
                            [ class "button is-success defaultSubmit"
                            , classList [ ( "is-loading", isLoading ) ]
                            , disabled (not isSendable)
                            , onClick (OnSubmit (isSendable && not isLoading) SubmitCommentPatch)
                            ]
                            [ text T.update ]
                        ]
                    ]
                ]
            ]
        ]


viewTensionCommentInput : Conf -> TensionCommon a -> State -> Html Msg
viewTensionCommentInput conf tension (State model) =
    let
        form =
            model.tension_form

        message =
            Dict.get "message" form.post |> withDefault ""

        isLoading =
            Loading.isLoading model.tension_patch

        isSendable =
            isPostSendable [ "message" ] form.post || (form.events |> List.filter (\x -> x.event_type == TensionEvent.Reopened || x.event_type == TensionEvent.Closed) |> List.length) > 0

        submit =
            onClick (OnSubmit (isSendable && not isLoading) <| SubmitTensionComment Nothing)

        ( submitCloseOpen, closeOpenTxt ) =
            case tension.status of
                TensionStatus.Open ->
                    ( onClick (OnSubmit (not isLoading) <| SubmitTensionComment (Just TensionStatus.Closed))
                    , ternary (message == "") T.close T.closeComment
                    )

                TensionStatus.Closed ->
                    ( onClick (OnSubmit (not isLoading) <| SubmitTensionComment (Just TensionStatus.Open))
                    , ternary (message == "") T.reopen T.reopenComment
                    )
    in
    div [ id "tensionCommentInput", class "media section is-paddingless commentInput" ]
        [ div [ class "media-left is-hidden-mobile" ] [ viewUser2 form.uctx.username ]
        , div [ class "media-content" ]
            [ div [ class "message" ]
                [ div [ class "message-header has-arrow-left" ] [ viewCommentInputHeader "commentInput" form ]
                , div [ class "message-body submitFocus" ]
                    [ div [ class "field" ]
                        [ div [ class "control" ]
                            [ viewCommentTextarea conf "commentInput" False T.leaveComment form model.userInput ]
                        ]
                    , case model.tension_patch of
                        Failure err ->
                            if isSendable then
                                viewGqlErrors err

                            else
                                text ""

                        _ ->
                            text ""
                    , div [ class "field is-grouped is-grouped-right" ]
                        [ div [ class "control", style "max-width" "100%" ]
                            [ div [ class "buttons" ]
                                [ button
                                    [ class "button"
                                    , classList [ ( "is-danger", tension.status == TensionStatus.Open ), ( "is-loading", isLoading && form.status /= Nothing ) ]
                                    , submitCloseOpen
                                    ]
                                    [ text closeOpenTxt ]
                                , button
                                    [ class "button is-success defaultSubmit"
                                    , classList [ ( "is-loading", isLoading && form.status == Nothing ) ]
                                    , disabled (not isSendable)
                                    , submit
                                    ]
                                    [ text T.comment ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewContractCommentInput : Conf -> State -> Html Msg
viewContractCommentInput conf (State model) =
    let
        form =
            model.contract_form

        isLoading =
            Loading.isLoading model.comment_result

        isSendable =
            isPostSendable [ "message" ] form.post
    in
    div [ id "tensionCommentInput", class "media section is-paddingless commentInput" ]
        [ div [ class "media-left is-hidden-mobile" ] [ viewUser2 form.uctx.username ]
        , div [ class "media-content" ]
            [ div [ class "message" ]
                [ div [ class "message-header has-arrow-left" ] [ viewCommentInputHeader "commentContractInput" form ]
                , div [ class "message-body submitFocus" ]
                    [ div [ class "field" ]
                        [ div [ class "control" ]
                            [ viewCommentTextarea conf "commentContractInput" False T.leaveComment form model.userInput ]
                        ]
                    , case model.comment_result of
                        Failure err ->
                            if isSendable then
                                viewGqlErrors err

                            else
                                text ""

                        _ ->
                            text ""
                    , div [ class "field is-grouped is-grouped-right" ]
                        [ div [ class "control" ]
                            [ div [ class "buttons" ]
                                [ button
                                    [ class "button defaultSubmit"
                                    , classList [ ( "is-loading", isLoading ) ]
                                    , disabled (not isSendable)
                                    , onClick (OnSubmit (isSendable && not isLoading) SubmitContractComment)
                                    ]
                                    [ text T.comment ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]



--
-- Shared View
--


viewCommentInputHeader targetid form =
    let
        isMdHelpOpen =
            Dict.get ("isMdHelpOpen" ++ targetid) form.post == Just "true"

        onChangeViewMode =
            if String.startsWith "update" targetid then
                ChangeUpdateViewMode

            else if targetid == "commentContractInput" then
                ChangeContractInputViewMode

            else
                ChangeInputViewMode
    in
    div [ class "level commentHeader" ]
        [ div [ class "level-left" ]
            [ div [ class "tabs is-boxed is-small" ]
                [ ul []
                    [ li [ classList [ ( "is-active", form.viewMode == Write ) ] ] [ a [ onClickSafe (onChangeViewMode Write), target "_blank" ] [ text T.write ] ]
                    , li [ classList [ ( "is-active", form.viewMode == Preview ) ] ] [ a [ onClickSafe (onChangeViewMode Preview), target "_blank" ] [ text T.preview ] ]
                    ]
                ]
            ]
        , div [ class "level-right is-hidden-mobile" ]
            [ div [ onClick (OnRichText targetid "Heading"), class "tooltip has-tooltip-bottom", attribute "data-tooltip" "Heading text" ] [ text "H" ]
            , div [ onClick (OnRichText targetid "Bold"), class "tooltip has-tooltip-bottom", attribute "data-tooltip" "Bold text" ] [ strong [] [ text "B" ] ]
            , div [ onClick (OnRichText targetid "Italic"), class "tooltip has-tooltip-bottom", attribute "data-tooltip" "Italic text" ] [ span [ class "is-italic" ] [ text "I" ] ]
            , div [ onClick (OnRichText targetid "Strikethrough"), class "tooltip has-tooltip-bottom", attribute "data-tooltip" "Strikethrough" ] [ span [] [ text ("̶" ++ "S" ++ "̶") ] ]
            , div [ onClick (OnRichText targetid "Quote"), class "tooltip has-tooltip-bottom mr-3", attribute "data-tooltip" "Quote" ] [ span [] [ A.icon "icon-quote-right icon-xs" ] ]
            , div [ onClick (OnRichText targetid "Link"), class "tooltip has-tooltip-bottom", attribute "data-tooltip" "Link" ] [ span [] [ A.icon "icon-link icon-sm" ] ]
            , div [ onClick (OnRichText targetid "List-ul"), class "tooltip has-tooltip-bottom", attribute "data-tooltip" "List" ] [ span [] [ A.icon "icon-list-ul icon-sm" ] ]
            , div [ onClick (OnRichText targetid "List-ol"), class "tooltip has-tooltip-bottom", attribute "data-tooltip" "Ordered list" ] [ span [] [ A.icon "icon-list-ol icon-sm" ] ]
            , div [ onClick (OnRichText targetid "List-check"), class "tooltip has-tooltip-bottom mr-3", attribute "data-tooltip" "Check list" ] [ span [] [ A.icon "icon-check-square icon-sm" ] ]
            , div [ onClick (OnRichText targetid "MentionUser"), class "tooltip has-tooltip-bottom", attribute "data-tooltip" "Mention an user" ] [ span [] [ A.icon "icon-at-sign icon-sm" ] ]
            , div [ onClick (OnRichText targetid "MentionTension"), class "tooltip has-tooltip-bottom mr-3", attribute "data-tooltip" "Reference a tension" ] [ A.icon "icon-exchange icon-sm" ]
            , div
                [ onClick (OnToggleMdHelp targetid)
                , class "tooltip has-tooltip-bottom is-right is-h is-w"
                , classList [ ( "is-highlight", isMdHelpOpen ) ]
                , attribute "data-tooltip" T.markdownSupport
                ]
                [ A.icon "icon-markdown" ]
            ]
        , if isMdHelpOpen then
            div [ id "mdLegend", class "box" ]
                [ button [ class "delete is-pulled-right", onClick (OnToggleMdHelp targetid) ] []
                , renderMarkdown "" T.markdownHelp
                ]

          else
            text ""
        ]


viewCommentTextarea conf targetid isModal placeholder_txt form userInput =
    let
        message =
            Dict.get "message" form.post |> withDefault ""

        line_len =
            List.length <| String.lines message

        ( max_len, min_len ) =
            if isMobile conf.screen then
                if isModal then
                    ( 4, 2 )

                else
                    ( 6, 4 )

            else if isModal then
                ( 10, 4 )

            else if targetid == "commentContractInput" then
                ( 15, 4 )

            else
                ( 15, 6 )

        onChangePost =
            if String.startsWith "update" targetid then
                OnChangeCommentPatch

            else if targetid == "commentContractInput" then
                OnChangeContractComment

            else
                OnChangeComment
    in
    div []
        [ textarea
            [ id targetid
            , class "textarea"
            , classList [ ( "is-invisible-force", form.viewMode == Preview ) ]
            , rows (min max_len (max line_len min_len))
            , placeholder placeholder_txt
            , value message
            , onInput (onChangePost "message")

            --, contenteditable True
            ]
            []
        , if form.viewMode == Preview then
            div [ class "mt-2 mx-3" ]
                [ renderMarkdown "is-human hidden-textarea" message, hr [ class "has-background-border-light" ] [] ]

          else
            text ""
        , span [ id (targetid ++ "searchInput"), class "searchInput", attribute "aria-hidden" "true", attribute "style" "display:none;" ]
            [ UserInput.viewUserSeeker userInput |> Html.map UserInputMsg ]

        -- @obsolete
        --[ Maybe.map2
        --    (\userSearchInput userSearchInputMsg ->
        --        UserInput.viewUserSeeker userSearchInput |> Html.map userSearchInputMsg
        --    )
        --    userInput
        --    UserInputMsg
        --    |> withDefault (text "")
        --]
        ]



--
-- <View Event>
--
--


viewEvent : Conf -> Maybe String -> Maybe TensionAction.TensionAction -> Event -> Html Msg
viewEvent conf focusid_m action event =
    let
        eventView =
            case event.event_type of
                TensionEvent.Reopened ->
                    viewEventStatus conf.lang conf.now event TensionStatus.Open

                TensionEvent.Closed ->
                    viewEventStatus conf.lang conf.now event TensionStatus.Closed

                TensionEvent.TitleUpdated ->
                    viewEventTitle conf.lang conf.now event

                TensionEvent.TypeUpdated ->
                    viewEventType conf.lang conf.now event

                TensionEvent.Visibility ->
                    viewEventVisibility conf.lang conf.now event

                TensionEvent.Authority ->
                    viewEventAuthority conf.lang conf.now event action

                TensionEvent.AssigneeAdded ->
                    viewEventAssignee conf.lang conf.now event True

                TensionEvent.AssigneeRemoved ->
                    viewEventAssignee conf.lang conf.now event False

                TensionEvent.LabelAdded ->
                    viewEventLabel focusid_m conf.lang conf.now event True

                TensionEvent.LabelRemoved ->
                    viewEventLabel focusid_m conf.lang conf.now event False

                TensionEvent.BlobPushed ->
                    viewEventPushed conf.lang conf.now event action

                TensionEvent.BlobArchived ->
                    viewEventArchived conf.lang conf.now event action True

                TensionEvent.BlobUnarchived ->
                    viewEventArchived conf.lang conf.now event action False

                TensionEvent.MemberLinked ->
                    viewEventMemberLinked conf.lang conf.now event action

                TensionEvent.MemberUnlinked ->
                    viewEventMemberUnlinked conf.lang conf.now event action

                TensionEvent.UserJoined ->
                    viewEventUserJoined conf.lang conf.now event action

                TensionEvent.UserLeft ->
                    viewEventUserLeft conf.lang conf.now event action

                TensionEvent.Moved ->
                    viewEventMoved conf.lang conf.now event

                TensionEvent.Mentioned ->
                    viewEventMentioned conf.lang conf.now event

                _ ->
                    []
    in
    if eventView == [] then
        text ""

    else
        div [ id event.createdAt, class "media is-paddingless actionComment" ] eventView


viewEventStatus : Lang.Lang -> Time.Posix -> Event -> TensionStatus.TensionStatus -> List (Html Msg)
viewEventStatus lang now event status =
    let
        ( actionIcon, actionText ) =
            case status of
                TensionStatus.Open ->
                    ( "icon-alert-circle", T.reopened2 )

                TensionStatus.Closed ->
                    ( "icon-alert-circle", T.closed2 )
    in
    [ span [ class "media-left", style "margin-left" "-4px" ] [ A.icon (actionIcon ++ " icon-1half has-text-" ++ statusColor status) ]
    , span [ class "media-content", attribute "style" "padding-top: 4px;margin-left: -4px" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink event.createdBy.username, strong [] [ text actionText ], text (formatDate lang now event.createdAt) ]
        ]
    ]


viewEventTitle : Lang.Lang -> Time.Posix -> Event -> List (Html Msg)
viewEventTitle lang now event =
    let
        icon =
            A.icon "icon-edit-2"
    in
    [ div [ class "media-left" ] [ icon ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink event.createdBy.username, text T.updated2, span [ class "is-strong" ] [ text T.theSubject ], text (formatDate lang now event.createdAt) ]
        , span [ class "ml-3" ]
            [ span [ class "is-strong is-crossed" ] [ event.old |> withDefault "" |> text ]
            , span [ class "arrow-right mx-1" ] []
            , span [ class "is-strong" ] [ event.new |> withDefault "" |> text ]
            ]
        ]
    ]


viewEventType : Lang.Lang -> Time.Posix -> Event -> List (Html Msg)
viewEventType lang now event =
    let
        icon =
            A.icon "icon-edit-2"
    in
    [ div [ class "media-left" ] [ icon ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink event.createdBy.username, text T.changed2, span [ class "is-strong" ] [ text T.theType_ ], text (formatDate lang now event.createdAt) ]
        , span [ class "ml-3" ]
            [ span [ class "is-strong" ] [ event.old |> withDefault "" |> TensionType.fromString |> withDefault TensionType.Operational |> tensionIcon2 ]
            , span [ class "arrow-right mx-1" ] []
            , span [ class "is-strong" ] [ event.new |> withDefault "" |> TensionType.fromString |> withDefault TensionType.Operational |> tensionIcon2 ]
            ]
        ]
    ]


viewEventVisibility : Lang.Lang -> Time.Posix -> Event -> List (Html Msg)
viewEventVisibility lang now event =
    let
        icon =
            A.icon "icon-eye"
    in
    [ div [ class "media-left" ] [ icon ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink event.createdBy.username, text T.changed2, span [ class "is-strong" ] [ text T.theVisibility ], text (formatDate lang now event.createdAt) ]
        , span [ class "ml-3" ]
            [ span [ class "is-strong" ] [ event.old |> withDefault "" |> text ]
            , span [ class "arrow-right mx-1" ] []
            , span [ class "is-strong" ] [ event.new |> withDefault "" |> text ]
            ]
        ]
    ]


viewEventAuthority : Lang.Lang -> Time.Posix -> Event -> Maybe TensionAction.TensionAction -> List (Html Msg)
viewEventAuthority lang now event action =
    let
        ( icon, eventText ) =
            case tensionAction2NodeType action of
                Just NodeType.Circle ->
                    ( A.icon "icon-shield", T.theGovernance )

                Just NodeType.Role ->
                    ( A.icon "icon-key", T.theAuthority )

                _ ->
                    ( A.icon "icon-key", "unknown action" )
    in
    [ div [ class "media-left" ] [ icon ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink event.createdBy.username, text T.changed2, span [ class "is-strong" ] [ text eventText ], text (formatDate lang now event.createdAt) ]
        , span [ class "ml-3" ]
            [ span [ class "is-strong" ] [ event.old |> withDefault "" |> text ]
            , span [ class "arrow-right mx-1" ] []
            , span [ class "is-strong" ] [ event.new |> withDefault "" |> text ]
            ]
        ]
    ]


viewEventAssignee : Lang.Lang -> Time.Posix -> Event -> Bool -> List (Html Msg)
viewEventAssignee lang now event isNew =
    let
        icon =
            A.icon "icon-user"

        ( actionText, value ) =
            if isNew then
                ( T.assigned2, withDefault "" event.new )

            else
                ( T.unassigned2, withDefault "" event.old )
    in
    [ div [ class "media-left" ] [ icon ]
    , div [ class "media-content" ]
        [ span [] <|
            List.intersperse (text " ")
                [ viewUsernameLink event.createdBy.username, strong [] [ text actionText ], viewUsernameLink value, text (formatDate lang now event.createdAt) ]
        ]
    ]


viewEventLabel : Maybe String -> Lang.Lang -> Time.Posix -> Event -> Bool -> List (Html Msg)
viewEventLabel focusid_m lang now event isNew =
    let
        icon =
            A.icon "icon-tag"

        ( actionText, value ) =
            if isNew then
                ( T.addedTheLabel, withDefault "unknown" event.new )

            else
                ( T.removedTheLabel, withDefault "unknown" event.old )

        label =
            Label "" (SE.leftOfBack "§" value) (SE.rightOfBack "§" value |> Just) []

        link =
            Maybe.map
                (\nid ->
                    toLink TensionsBaseUri nid [] ++ ("?l=" ++ label.name)
                )
                focusid_m
    in
    [ div [ class "media-left" ] [ icon ]
    , div [ class "media-content" ]
        [ span [ class "labelsList" ] <|
            List.intersperse (text " ")
                [ viewUsernameLink event.createdBy.username, strong [] [ text actionText ], viewLabel "" link label, text (formatDate lang now event.createdAt) ]
        ]
    ]


viewEventPushed : Lang.Lang -> Time.Posix -> Event -> Maybe TensionAction.TensionAction -> List (Html Msg)
viewEventPushed lang now event action_m =
    let
        action =
            withDefault TensionAction.NewRole action_m
    in
    [ div [ class "media-left" ] [ A.icon "icon-share" ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink event.createdBy.username, strong [] [ text T.published2 ], text T.this, textD (action2str action), text (formatDate lang now event.createdAt) ]
        ]
    ]


viewEventArchived : Lang.Lang -> Time.Posix -> Event -> Maybe TensionAction.TensionAction -> Bool -> List (Html Msg)
viewEventArchived lang now event action_m isArchived =
    let
        action =
            withDefault TensionAction.NewRole action_m

        ( icon, txt ) =
            if isArchived then
                ( A.icon "icon-archive", T.archived2 )

            else
                ( i [ class "icon-archive icon-is-slashed" ] [], T.unarchived2 )
    in
    [ div [ class "media-left" ] [ icon ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink event.createdBy.username, strong [] [ text txt ], text T.this, textD (action2str action), text (formatDate lang now event.createdAt) ]
        ]
    ]


viewEventMemberLinked : Lang.Lang -> Time.Posix -> Event -> Maybe TensionAction.TensionAction -> List (Html Msg)
viewEventMemberLinked lang now event action_m =
    [ div [ class "media-left" ] [ A.icon "icon-user-check has-text-success" ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink (withDefault "" event.new), strong [] [ text T.linked2 ], text T.toThisRole, text (formatDate lang now event.createdAt) ]
        ]
    ]


viewEventMemberUnlinked : Lang.Lang -> Time.Posix -> Event -> Maybe TensionAction.TensionAction -> List (Html Msg)
viewEventMemberUnlinked lang now event action_m =
    let
        action_txt =
            case (getTensionCharac (withDefault TensionAction.NewRole action_m)).doc_type of
                NODE NodeType.Circle ->
                    T.toThisOrganisation

                _ ->
                    T.toThisRole
    in
    [ div [ class "media-left" ] [ A.icon "icon-user has-text-danger" ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink (withDefault "" event.old), strong [] [ text T.unlinked2 ], text action_txt, text (formatDate lang now event.createdAt) ]
        ]
    ]


viewEventUserJoined : Lang.Lang -> Time.Posix -> Event -> Maybe TensionAction.TensionAction -> List (Html Msg)
viewEventUserJoined lang now event action_m =
    let
        action_txt =
            T.theOrganisation
    in
    [ div [ class "media-left" ] [ A.icon "icon-log-in" ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink (withDefault "" event.new), strong [] [ text T.joined2 ], text action_txt, text (formatDate lang now event.createdAt) ]
        ]
    ]


viewEventUserLeft : Lang.Lang -> Time.Posix -> Event -> Maybe TensionAction.TensionAction -> List (Html Msg)
viewEventUserLeft lang now event action_m =
    let
        action =
            withDefault TensionAction.NewRole action_m

        action_txt =
            case event.new of
                Just type_ ->
                    case RoleType.fromString type_ of
                        Just RoleType.Guest ->
                            T.theOrganisation

                        _ ->
                            T.this ++ " " ++ decap T.role

                Nothing ->
                    action2str action |> decap
    in
    [ div [ class "media-left" ] [ A.icon "icon-log-out" ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink (withDefault "" event.old), strong [] [ text T.left2 ], text action_txt, text (formatDate lang now event.createdAt) ]
        ]
    ]


viewEventMoved : Lang.Lang -> Time.Posix -> Event -> List (Html Msg)
viewEventMoved lang now event =
    [ div [ class "media-left" ] [ span [ class "arrow-right2 pl-0 pr-0 mr-0" ] [] ]
    , div [ class "media-content" ]
        [ span [] <|
            List.intersperse (text " ")
                [ viewUsernameLink event.createdBy.username
                , strong [] [ text T.moved2 ]
                , text T.from
                , event.old |> Maybe.map (\nid -> viewNodeRefShort OverviewBaseUri nid) |> withDefault (text "unknown")
                , text T.to
                , event.new |> Maybe.map (\nid -> viewNodeRefShort OverviewBaseUri nid) |> withDefault (text "unknown")
                , text (formatDate lang now event.createdAt)
                ]
        ]
    ]


viewEventMentioned : Lang.Lang -> Time.Posix -> Event -> List (Html Msg)
viewEventMentioned lang now event =
    case event.mentioned of
        Just { id, status, title, receiverid } ->
            let
                goto =
                    withDefault "" event.new
            in
            [ div [ class "media-left" ] [ A.icon "icon-message-square" ]
            , div [ class "media-content" ]
                [ span [] <|
                    List.intersperse (text " ")
                        [ viewUsernameLink event.createdBy.username
                        , strong [] [ text T.mentioned2 ]
                        , text (formatDate lang now event.createdAt)
                        , div []
                            [ a
                                [ class "is-strong is-size-6 discrete-link mr-4"
                                , href ((Route.Tension_Dynamic_Dynamic { param1 = nid2rootid receiverid, param2 = id } |> toHref) ++ "?goto=" ++ goto)
                                ]
                                [ span
                                    [ class "tooltip has-tooltip-arrow"
                                    , attribute "data-tooltip" (tensionStatus2str status)
                                    ]
                                    [ A.icon ("icon-alert-circle icon-sm marginTensionStatus has-text-" ++ statusColor status) ]
                                , text title
                                ]
                            , a
                                [ class "discrete-link is-discrete"
                                , href (uriFromNameid OverviewBaseUri receiverid [])
                                ]
                                [ receiverid |> String.replace "#" "/" |> text ]
                            ]
                        ]
                ]
            ]

        Nothing ->
            []



--
-- </ View Event>
--
