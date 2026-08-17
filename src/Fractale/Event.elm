{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2026 Fractale Co

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


module Fractale.Event exposing (..)

import Assets as A
import Fractale.Codecs exposing (FractalBaseRoute(..), nid2rootid, shortId, toLink)
import Fractale.Form exposing (decodeColumnRef, decodeLabel, decodeProjectRef)
import Fractale.User exposing (UserState(..))
import Fractale.View exposing (byAt, nodeType2str, statusColor, tensionIcon2, tensionStatus2str, viewCircleSimple, viewNodeRefShort, viewUsernameLink)
import Components.LabelSearchPanel exposing (viewLabel)
import Components.ProjectSearchPanel exposing (viewProjectColumnTag, viewProjectTag)
import Dict exposing (Dict)
import Utils.Bool exposing (ternary)
import Utils.Date exposing (formatDate)
import Utils.Html exposing (textD)
import Utils.String exposing (decap, space_)
import Schema.Enum.ContractType as ContractType
import Schema.Enum.NodeType as NodeType
import Schema.Enum.RoleType as RoleType
import Schema.Enum.TensionEvent as TensionEvent
import Schema.Enum.TensionStatus as TensionStatus
import Schema.Enum.TensionType as TensionType
import Generated.Route as Route exposing (toHref)
import Html exposing (Html, a, div, i, p, small, span, strong, text)
import Html.Attributes exposing (attribute, class, classList, href, id, style)
import Maybe exposing (withDefault)
import ModelSchema exposing (ContractNotif, Event, EventFragment, EventNotif, Label, UserEvent, Username)
import Session exposing (SessionCommon)
import String.Extra as SE
import Text as T


eventToLink : UserEvent -> EventNotif -> String
eventToLink ue e =
    if
        List.member e.event_type
            [ TensionEvent.Closed
            , TensionEvent.Reopened
            , TensionEvent.CommentPushed
            , TensionEvent.Moved
            , TensionEvent.UserLeft
            , TensionEvent.UserJoined
            , TensionEvent.MemberLinked
            , TensionEvent.MemberUnlinked
            , TensionEvent.Visibility
            , TensionEvent.Authority
            , TensionEvent.Pinned
            , TensionEvent.Unpinned
            ]
    then
        (Route.Tension_Dynamic_Dynamic { param1 = nid2rootid e.tension.receiver.nameid, param2 = e.tension.id } |> toHref)
            -- Comment
            ++ "?eid="
            ++ ue.id
            ++ "&goto="
            ++ e.createdAt

    else if List.member e.event_type [ TensionEvent.BlobPushed, TensionEvent.BlobArchived, TensionEvent.BlobUnarchived ] then
        (Route.Tension_Dynamic_Dynamic_Action { param1 = nid2rootid e.tension.receiver.nameid, param2 = e.tension.id } |> toHref)
            -- Document/Mandate
            ++ "?eid="
            ++ ue.id

    else
        (Route.Tension_Dynamic_Dynamic { param1 = nid2rootid e.tension.receiver.nameid, param2 = e.tension.id } |> toHref)
            -- Tension
            ++ "?eid="
            ++ ue.id


contractToLink : UserEvent -> ContractNotif -> String
contractToLink ue c =
    Route.Tension_Dynamic_Dynamic_Contract_Dynamic { param1 = nid2rootid c.tension.receiver.nameid, param2 = c.tension.id, param3 = c.id } |> toHref


viewEventMedia : SessionCommon -> Bool -> Dict String String -> Html msg
viewEventMedia session inline ev =
    div [ class "content" ]
        [ p [] <|
            [ a
                [ class "discrete-link is-icon-aligned"
                , href (Dict.get "link" ev |> withDefault "#")
                ]
              <|
                List.intersperse (text space_) <|
                    [ A.icon (Dict.get "icon" ev |> withDefault "")
                    , strong [ class "ml-1" ] [ Dict.get "title" ev |> withDefault "" |> text ]
                    , span [ class "is-discrete" ] [ text T.in_ ]
                    , span [ class "is-strong" ] [ Dict.get "target" ev |> withDefault "" |> text ]
                    , text ":"
                    , span [] [ Dict.get "title_" ev |> withDefault "" |> text ]
                    ]
            , small [ class "help", classList [ ( "is-pulled-right", inline ) ] ] [ byAt session (Username (Dict.get "author" ev |> withDefault "")) (Dict.get "date" ev |> withDefault "") ]
            ]
        ]


viewContractMedia : SessionCommon -> Dict String String -> Html msg
viewContractMedia session ev =
    div [ class "content" ]
        [ p [] <|
            [ a
                [ class "discrete-link is-icon-aligned"
                , href (Dict.get "link" ev |> withDefault "#")
                ]
              <|
                List.intersperse (text " ") <|
                    [ A.icon "icon-edit"
                    , strong [ class "ml-1" ] [ Dict.get "contract" ev |> withDefault "" |> text ]
                    , span [ class "is-discrete" ] [ Dict.get "jonction" ev |> withDefault "" |> text ]

                    --, A.icon (Dict.get "icon" ev |> withDefault "")
                    , strong [] [ Dict.get "title" ev |> withDefault "" |> text ]
                    , span [ class "is-discrete" ] [ text T.in_ ]
                    , span [ class "is-strong" ] [ Dict.get "target" ev |> withDefault "" |> text ]
                    ]
            , small [ class "help" ] [ byAt session (Username (Dict.get "author" ev |> withDefault "")) (Dict.get "date" ev |> withDefault "") ]
            ]
        ]


viewNotifMedia : SessionCommon -> Dict String String -> Html msg
viewNotifMedia session ev =
    div [ class "content" ]
        [ a
            [ class "discrete-link is-icon-aligned"
            , href (Dict.get "link" ev |> withDefault "#")
            ]
          <|
            List.intersperse (text " ") <|
                [ A.icon (Dict.get "icon" ev |> withDefault "")
                , Dict.get "title" ev |> withDefault "no input message." |> text
                ]
        , small [ class "help" ] [ byAt session (Username (Dict.get "author" ev |> withDefault "")) (Dict.get "date" ev |> withDefault "") ]
        ]


eventTypeToText : Dict.Dict String String -> TensionEvent.TensionEvent -> String
eventTypeToText lexicon e =
    case e of
        TensionEvent.Created ->
            T.created_event lexicon

        TensionEvent.Closed ->
            T.closed_event lexicon

        TensionEvent.Reopened ->
            T.reopened_event lexicon

        TensionEvent.CommentPushed ->
            T.commentPushed_event

        TensionEvent.Moved ->
            T.moved_event lexicon

        TensionEvent.TitleUpdated ->
            T.titleUpdated_event

        TensionEvent.TypeUpdated ->
            T.typeUpdated_event

        TensionEvent.Authority ->
            T.authority_event

        TensionEvent.Visibility ->
            T.visibility_event

        TensionEvent.LabelAdded ->
            T.labelAdded_event

        TensionEvent.LabelRemoved ->
            T.labelRemoved_event

        TensionEvent.AssigneeAdded ->
            T.assigneeAdded_event

        TensionEvent.AssigneeRemoved ->
            T.assigneeRemoved_event

        TensionEvent.BlobCommitted ->
            T.blobCommitted_event lexicon

        TensionEvent.BlobPushed ->
            T.blobCommitted_event lexicon

        TensionEvent.MemberLinked ->
            T.memberLinked_event

        TensionEvent.MemberUnlinked ->
            T.memberUnlinked_event

        TensionEvent.UserJoined ->
            T.userJoined_event

        TensionEvent.UserLeft ->
            T.userLeft_event

        TensionEvent.Pinned ->
            T.pinned_event lexicon

        TensionEvent.Unpinned ->
            T.unpinned_event lexicon

        TensionEvent.ProjectAdded ->
            T.addedToProject

        TensionEvent.ProjectRemoved ->
            T.removedFromProject

        TensionEvent.ProjectColumnMoved ->
            T.movedColumn

        _ ->
            e |> TensionEvent.toString |> SE.humanize


contractTypeToText : ContractType.ContractType -> String
contractTypeToText c =
    case c of
        ContractType.AnyCandidates ->
            "Invitation"

        ContractType.AnyCoordoDual ->
            "Coordinators validation needed"

        ContractType.AnyCoordoTarget ->
            "Coordinator validation needed"

        ContractType.AnyCoordoSource ->
            "Coordinator validation needed"


contractEventToText : Dict.Dict String String -> Maybe NodeType.NodeType -> TensionEvent.TensionEvent -> String
contractEventToText lexicon ntm c =
    case c of
        TensionEvent.Moved ->
            case ntm of
                Nothing ->
                    T.moved_contract lexicon

                Just NodeType.Circle ->
                    T.moved_circle

                Just NodeType.Role ->
                    T.moved_role

        TensionEvent.MemberLinked ->
            T.memberLinked_contract

        TensionEvent.MemberUnlinked ->
            T.memberUnlinked_contract

        TensionEvent.UserJoined ->
            T.userJoined_contract

        _ ->
            c |> TensionEvent.toString |> SE.humanize


contractEventToValue : EventFragment -> Maybe (Html msg)
contractEventToValue event =
    case event.event_type of
        TensionEvent.Moved ->
            Maybe.map2
                (\old new ->
                    span [] [ viewCircleSimple old, span [ class "arrow-right" ] [], viewCircleSimple new ]
                )
                event.old
                event.new

        _ ->
            Maybe.map (\x -> text x) event.new


contractToJonction : ContractType.ContractType -> String
contractToJonction c =
    case c of
        ContractType.AnyCandidates ->
            T.as_

        _ ->
            T.to


cev2c : Dict String String -> Maybe NodeType.NodeType -> TensionEvent.TensionEvent -> String
cev2c lexicon ntm c =
    case c of
        TensionEvent.Moved ->
            case ntm of
                Nothing ->
                    T.moved_contract_success lexicon

                Just NodeType.Circle ->
                    T.moved_circle_contract_success

                Just NodeType.Role ->
                    T.moved_role_contract_success

        TensionEvent.MemberLinked ->
            T.memberLinked_contract_success

        TensionEvent.MemberUnlinked ->
            T.memberUnlinked_contract_success

        TensionEvent.UserJoined ->
            T.userJoined_contract_success

        _ ->
            "@TODO contractEventToText"


cev2p : Dict String String -> Maybe NodeType.NodeType -> TensionEvent.TensionEvent -> String
cev2p lexicon ntm c =
    case c of
        TensionEvent.MemberLinked ->
            T.memberLinked_contract_success_ext

        TensionEvent.UserJoined ->
            T.userJoined_contract_success_ext

        _ ->
            cev2c lexicon ntm c


eventToIcon : TensionEvent.TensionEvent -> String
eventToIcon ev =
    case ev of
        TensionEvent.Created ->
            "icon-alert-circle has-text-" ++ statusColor TensionStatus.Open

        TensionEvent.Reopened ->
            "icon-alert-circle has-text-" ++ statusColor TensionStatus.Open

        TensionEvent.Closed ->
            "icon-alert-circle has-text-" ++ statusColor TensionStatus.Closed

        TensionEvent.TitleUpdated ->
            "icon-edit-2"

        TensionEvent.TypeUpdated ->
            "icon-edit-2"

        TensionEvent.Visibility ->
            "icon-lock"

        TensionEvent.Authority ->
            "icon-key"

        TensionEvent.AssigneeAdded ->
            "icon-user"

        TensionEvent.AssigneeRemoved ->
            "icon-user"

        TensionEvent.LabelAdded ->
            "icon-tag"

        TensionEvent.LabelRemoved ->
            "icon-tag"

        TensionEvent.CommentPushed ->
            "icon-message-square"

        TensionEvent.BlobCommitted ->
            "icon-edit-2"

        TensionEvent.BlobPushed ->
            "icon-share"

        TensionEvent.BlobArchived ->
            "icon-archive"

        TensionEvent.BlobUnarchived ->
            "icon-archive"

        TensionEvent.UserJoined ->
            "icon-log-in"

        TensionEvent.UserLeft ->
            "icon-log-out"

        TensionEvent.MemberLinked ->
            "icon-log-in"

        TensionEvent.MemberUnlinked ->
            "icon-log-out"

        TensionEvent.Moved ->
            "arrow-right2 pl-0 pr-0 mr-0"

        TensionEvent.Pinned ->
            "icon-pin"

        TensionEvent.Unpinned ->
            "icon-pin"

        TensionEvent.ProjectAdded ->
            "icon-layout"

        TensionEvent.ProjectRemoved ->
            "icon-layout"

        TensionEvent.ProjectColumnMoved ->
            "arrow-right2 pl-0 pr-0 mr-0"

        _ ->
            ""



--
-- <View Event>
--
--


viewEvent : SessionCommon -> Maybe String -> NodeType.NodeType -> Event -> Html msg
viewEvent session focusid_m nodeType event =
    let
        eventView =
            case event.event_type of
                TensionEvent.Reopened ->
                    viewEventStatus session event TensionStatus.Open

                TensionEvent.Closed ->
                    viewEventStatus session event TensionStatus.Closed

                TensionEvent.TitleUpdated ->
                    viewEventTitle session event

                TensionEvent.TypeUpdated ->
                    viewEventType session event

                TensionEvent.Visibility ->
                    viewEventVisibility session event

                TensionEvent.Authority ->
                    viewEventAuthority session event nodeType

                TensionEvent.AssigneeAdded ->
                    viewEventAssignee session event True

                TensionEvent.AssigneeRemoved ->
                    viewEventAssignee session event False

                TensionEvent.LabelAdded ->
                    viewEventLabel focusid_m session event True

                TensionEvent.LabelRemoved ->
                    viewEventLabel focusid_m session event False

                TensionEvent.BlobPushed ->
                    viewEventPushed session event nodeType

                TensionEvent.BlobArchived ->
                    viewEventArchived session event nodeType True

                TensionEvent.BlobUnarchived ->
                    viewEventArchived session event nodeType False

                TensionEvent.MemberLinked ->
                    viewEventMemberLinked session event nodeType

                TensionEvent.MemberUnlinked ->
                    viewEventMemberUnlinked session event nodeType

                TensionEvent.UserJoined ->
                    viewEventUserJoined session event nodeType

                TensionEvent.UserLeft ->
                    viewEventUserLeft session event nodeType

                TensionEvent.Moved ->
                    viewEventMoved session event

                TensionEvent.Mentioned ->
                    viewEventMentioned session event

                TensionEvent.CommentDeleted ->
                    viewEventCommentDeleted session event

                TensionEvent.Pinned ->
                    viewEventPinned session event True

                TensionEvent.Unpinned ->
                    viewEventPinned session event False

                TensionEvent.ProjectAdded ->
                    viewEventProject focusid_m session event True

                TensionEvent.ProjectRemoved ->
                    viewEventProject focusid_m session event False

                TensionEvent.ProjectColumnMoved ->
                    viewEventProjectColumnMoved focusid_m session event

                _ ->
                    []
    in
    if eventView == [] then
        text ""

    else
        div [ id event.createdAt, class "media p-0 actionComment" ] eventView


viewEventStatus : SessionCommon -> Event -> TensionStatus.TensionStatus -> List (Html msg)
viewEventStatus session event status =
    let
        actionText =
            case status of
                TensionStatus.Open ->
                    T.reopened2

                TensionStatus.Closed ->
                    T.closed2
    in
    [ span [ class "media-left", style "margin-left" "-4px" ] [ A.icon ("icon-alert-circle icon-1half has-text-" ++ statusColor status) ]
    , span [ class "media-content", attribute "style" "padding-top: 4px;margin-left: -4px" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink event.createdBy.username, strong [ class "has-text-evidence" ] [ text actionText ], text (formatDate session.lang session.now event.createdAt) ]
        ]
    ]


viewEventTitle : SessionCommon -> Event -> List (Html msg)
viewEventTitle session event =
    let
        icon =
            A.icon "icon-edit-2"
    in
    [ div [ class "media-left" ] [ icon ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink event.createdBy.username, text T.updated2, span [ class "is-strong" ] [ text T.theSubject ], text (formatDate session.lang session.now event.createdAt) ]
        , span [ class "ml-3" ]
            [ span [ class "is-strong is-crossed" ] [ event.old |> withDefault "" |> text ]
            , span [ class "arrow-right mx-1" ] []
            , span [ class "is-strong" ] [ event.new |> withDefault "" |> text ]
            ]
        ]
    ]


viewEventType : SessionCommon -> Event -> List (Html msg)
viewEventType session event =
    let
        icon =
            A.icon "icon-edit-2"
    in
    [ div [ class "media-left" ] [ icon ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink event.createdBy.username, text T.changed2, span [ class "is-strong" ] [ text T.theType_ ], text (formatDate session.lang session.now event.createdAt) ]
        , span [ class "ml-3" ]
            [ span [ class "is-strong" ] [ event.old |> withDefault "" |> TensionType.fromString |> withDefault TensionType.Operational |> tensionIcon2 ]
            , span [ class "arrow-right mx-1" ] []
            , span [ class "is-strong" ] [ event.new |> withDefault "" |> TensionType.fromString |> withDefault TensionType.Operational |> tensionIcon2 ]
            ]
        ]
    ]


viewEventVisibility : SessionCommon -> Event -> List (Html msg)
viewEventVisibility session event =
    let
        icon =
            A.icon "icon-eye"
    in
    [ div [ class "media-left" ] [ icon ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink event.createdBy.username, text T.changed2, span [ class "is-strong" ] [ text T.theVisibility ], text (formatDate session.lang session.now event.createdAt) ]
        , span [ class "ml-3" ]
            [ span [ class "is-strong" ] [ event.old |> withDefault "" |> text ]
            , span [ class "arrow-right mx-1" ] []
            , span [ class "is-strong" ] [ event.new |> withDefault "" |> text ]
            ]
        ]
    ]


viewEventAuthority : SessionCommon -> Event -> NodeType.NodeType -> List (Html msg)
viewEventAuthority session event nodeType =
    let
        ( icon, eventText ) =
            case nodeType of
                NodeType.Circle ->
                    ( A.icon "icon-shield", T.theGovernance )

                NodeType.Role ->
                    ( A.icon "icon-key", T.theAuthority )
    in
    [ div [ class "media-left" ] [ icon ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink event.createdBy.username, text T.changed2, span [ class "is-strong" ] [ text eventText ], text (formatDate session.lang session.now event.createdAt) ]
        , span [ class "ml-3" ]
            [ span [ class "is-strong" ] [ event.old |> withDefault "" |> text ]
            , span [ class "arrow-right mx-1" ] []
            , span [ class "is-strong" ] [ event.new |> withDefault "" |> text ]
            ]
        ]
    ]


viewEventAssignee : SessionCommon -> Event -> Bool -> List (Html msg)
viewEventAssignee session event isNew =
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
                [ viewUsernameLink event.createdBy.username, strong [ class "has-text-evidence" ] [ text actionText ], viewUsernameLink value, text (formatDate session.lang session.now event.createdAt) ]
        ]
    ]


viewEventLabel : Maybe String -> SessionCommon -> Event -> Bool -> List (Html msg)
viewEventLabel focusid_m session event isNew =
    let
        icon =
            A.icon "icon-tag"

        ( actionText, value ) =
            if isNew then
                ( T.addedTheLabel, withDefault "unknown" event.new )

            else
                ( T.removedTheLabel, withDefault "unknown" event.old )

        label =
            decodeLabel value

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
                [ viewUsernameLink event.createdBy.username, strong [ class "has-text-evidence" ] [ text actionText ], viewLabel "" link label, text (formatDate session.lang session.now event.createdAt) ]
        ]
    ]


viewEventPushed : SessionCommon -> Event -> NodeType.NodeType -> List (Html msg)
viewEventPushed session event nodeType =
    [ div [ class "media-left" ] [ A.icon "icon-share" ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink event.createdBy.username, strong [ class "has-text-evidence" ] [ text T.published2 ], text T.this, textD (nodeType2str nodeType), text (formatDate session.lang session.now event.createdAt) ]
        ]
    ]


viewEventArchived : SessionCommon -> Event -> NodeType.NodeType -> Bool -> List (Html msg)
viewEventArchived session event nodeType isArchived =
    let
        ( icon, txt ) =
            if isArchived then
                ( A.icon "icon-archive", T.archived2 )

            else
                ( i [ class "icon-archive icon-is-slashed" ] [], T.unarchived2 )
    in
    [ div [ class "media-left" ] [ icon ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink event.createdBy.username, strong [ class "has-text-evidence" ] [ text txt ], text T.this, textD (nodeType2str nodeType), text (formatDate session.lang session.now event.createdAt) ]
        ]
    ]


viewEventMemberLinked : SessionCommon -> Event -> NodeType.NodeType -> List (Html msg)
viewEventMemberLinked session event _ =
    [ div [ class "media-left" ] [ A.icon "icon-user-check has-text-success" ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink (withDefault "" event.new), strong [ class "has-text-evidence" ] [ text T.linked2 ], text T.toThisRole, text (formatDate session.lang session.now event.createdAt) ]
        ]
    ]


viewEventMemberUnlinked : SessionCommon -> Event -> NodeType.NodeType -> List (Html msg)
viewEventMemberUnlinked session event nodeType =
    let
        action_txt =
            case nodeType of
                NodeType.Circle ->
                    T.toThisOrganisation

                NodeType.Role ->
                    T.toThisRole
    in
    [ div [ class "media-left" ] [ A.icon "icon-user has-text-danger" ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink (withDefault "" event.old), strong [ class "has-text-evidence" ] [ text T.unlinked2 ], text action_txt, text (formatDate session.lang session.now event.createdAt) ]
        ]
    ]


viewEventUserJoined : SessionCommon -> Event -> NodeType.NodeType -> List (Html msg)
viewEventUserJoined session event _ =
    let
        action_txt =
            T.theOrganisation
    in
    [ div [ class "media-left" ] [ A.icon "icon-log-in" ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink (withDefault "" event.new), strong [ class "has-text-evidence" ] [ text T.joined2 ], text action_txt, text (formatDate session.lang session.now event.createdAt) ]
        ]
    ]


viewEventUserLeft : SessionCommon -> Event -> NodeType.NodeType -> List (Html msg)
viewEventUserLeft session event nodeType =
    let
        action_txt =
            case event.new of
                Just type_ ->
                    case RoleType.fromString type_ of
                        Just RoleType.Guest ->
                            T.theOrganisation

                        Just RoleType.Owner ->
                            T.theOwnerRole

                        _ ->
                            T.this ++ " " ++ decap T.role

                Nothing ->
                    nodeType2str nodeType |> decap
    in
    [ div [ class "media-left" ] [ A.icon "icon-log-out" ]
    , div [ class "media-content" ]
        [ span [] <| List.intersperse (text " ") [ viewUsernameLink (withDefault "" event.old), strong [ class "has-text-evidence" ] [ text T.left2 ], text action_txt, text (formatDate session.lang session.now event.createdAt) ]
        ]
    ]


viewEventMoved : SessionCommon -> Event -> List (Html msg)
viewEventMoved session event =
    [ div [ class "media-left" ] [ span [ class "arrow-right2 pl-0 pr-0 mr-0" ] [] ]
    , div [ class "media-content" ]
        [ span [] <|
            List.intersperse (text " ")
                [ viewUsernameLink event.createdBy.username
                , strong [ class "has-text-evidence" ] [ text T.moved2 ]
                , text T.from
                , event.old |> Maybe.map (\nid -> viewNodeRefShort OverviewBaseUri nid) |> withDefault (text "unknown")
                , text T.to
                , event.new |> Maybe.map (\nid -> viewNodeRefShort OverviewBaseUri nid) |> withDefault (text "unknown")
                , text (formatDate session.lang session.now event.createdAt)
                ]
        ]
    ]


viewEventMentioned : SessionCommon -> Event -> List (Html msg)
viewEventMentioned session event =
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
                        , strong [ class "has-text-evidence" ] [ text (T.mentioned2 session.lexicon) ]
                        , text (formatDate session.lang session.now event.createdAt)
                        ]
                , div [ class "level ml-4 mt-1" ] <|
                    List.singleton <|
                        div [ class "level-left" ] <|
                            [ a
                                [ class "is-strong is-size-6 discrete-link mr-4 level-item"
                                , href ((Route.Tension_Dynamic_Dynamic { param1 = nid2rootid receiverid, param2 = id } |> toHref) ++ "?goto=" ++ goto)
                                ]
                                [ span [ Html.Attributes.title (tensionStatus2str status) ]
                                    [ A.icon ("icon-alert-circle icon-sm marginTensionStatus has-text-" ++ statusColor status) ]
                                , text title
                                ]
                            , a
                                [ class "discrete-link is-discrete level-item"
                                , href (toLink OverviewBaseUri receiverid [])
                                ]
                                [ receiverid |> String.replace "#" "/" |> text ]
                            ]
                ]
            ]

        Nothing ->
            []


viewEventCommentDeleted : SessionCommon -> Event -> List (Html msg)
viewEventCommentDeleted session event =
    [ div [ class "media-left" ] [ A.icon "icon-message-circle" ]
    , div [ class "media-content" ]
        [ span [] <|
            List.intersperse (text " ")
                [ viewUsernameLink event.createdBy.username
                , strong [ class "has-text-evidence" ] [ text T.deletedAComment ]
                , text (formatDate session.lang session.now event.createdAt)
                ]
        ]
    ]


viewEventPinned : SessionCommon -> Event -> Bool -> List (Html msg)
viewEventPinned session event isPinned =
    let
        ( icon, actionText ) =
            if isPinned then
                ( A.icon "icon-pin has-text-success", T.pinned2 )

            else
                ( A.icon "icon-pin", T.unpinned2 )
    in
    [ div [ class "media-left" ] [ icon ]
    , div [ class "media-content" ]
        [ span [] <|
            List.intersperse (text " ")
                [ viewUsernameLink event.createdBy.username
                , strong [ class "has-text-evidence" ] [ text actionText ]
                , text T.thisF
                , textD (T.tension session.lexicon)
                , text (formatDate session.lang session.now event.createdAt)
                ]
        ]
    ]



viewEventProject : Maybe String -> SessionCommon -> Event -> Bool -> List (Html msg)
viewEventProject focusid_m session event isAdded =
    let
        ( actionText, raw ) =
            if isAdded then
                ( T.addedToProject, withDefault "" event.new )

            else
                ( T.removedFromProject, withDefault "" event.old )

        project =
            decodeProjectRef raw

        projectTag =
            viewProjectTag [] [ text project.name ]

        nameNode =
            case focusid_m of
                Just focusid ->
                    if project.id == "" then
                        projectTag

                    else
                        a
                            [ class "discrete-link"
                            , href (Route.Project_Dynamic_Dynamic { param1 = nid2rootid focusid, param2 = shortId project.id } |> toHref)
                            ]
                            [ projectTag ]

                Nothing ->
                    projectTag
    in
    [ div [ class "media-left" ] [ A.icon "icon-layout" ]
    , div [ class "media-content" ]
        [ span [] <|
            List.intersperse (text " ")
                [ viewUsernameLink event.createdBy.username
                , strong [ class "has-text-evidence" ] [ text actionText ]
                , nameNode
                , text (formatDate session.lang session.now event.createdAt)
                ]
        ]
    ]


viewEventProjectColumnMoved : Maybe String -> SessionCommon -> Event -> List (Html msg)
viewEventProjectColumnMoved focusid_m session event =
    let
        old =
            event.old |> withDefault "" |> decodeColumnRef

        new =
            event.new |> withDefault "" |> decodeColumnRef

        colTag col =
            viewProjectColumnTag (ternary (col.color == "") Nothing (Just col.color)) col.name [] []

        viewCol col =
            case ( focusid_m, col.projectId ) of
                ( Just focusid, projectId ) ->
                    if projectId == "" then
                        colTag col

                    else
                        a
                            [ class "discrete-link"
                            , href (Route.Project_Dynamic_Dynamic { param1 = nid2rootid focusid, param2 = shortId projectId } |> toHref)
                            ]
                            [ colTag col ]

                _ ->
                    colTag col
    in
    [ div [ class "media-left" ] [ span [ class "arrow-right2 pl-0 pr-0 mr-0" ] [] ]
    , div [ class "media-content" ]
        [ span [] <|
            List.intersperse (text " ")
                [ viewUsernameLink event.createdBy.username
                , strong [ class "has-text-evidence" ] [ text T.movedColumn ]
                , viewCol old
                , text T.to
                , viewCol new
                , text (formatDate session.lang session.now event.createdAt)
                ]
        ]
    ]



--
-- </ View Event>
--
