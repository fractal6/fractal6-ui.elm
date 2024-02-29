{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2024 Fractale Co

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


module Bulk.Event exposing (..)

import Assets as A
import Bulk exposing (UserState(..))
import Bulk.Codecs exposing (ActionType(..), FractalBaseRoute(..), nid2rootid)
import Bulk.View exposing (byAt, statusColor, viewCircleSimple)
import Dict exposing (Dict)
import Fractal.Enum.ContractType as ContractType
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Generated.Route as Route exposing (toHref)
import Html exposing (Html, a, div, p, small, span, strong, text)
import Html.Attributes exposing (class, classList, href)
import Maybe exposing (withDefault)
import ModelSchema exposing (ContractNotif, EventFragment, EventNotif, UserEvent, Username)
import Session exposing (Conf)
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


viewEventMedia : Conf -> Bool -> Dict String String -> Html msg
viewEventMedia conf inline ev =
    div [ class "content" ]
        [ p [] <|
            [ a
                [ class "discrete-link"
                , href (Dict.get "link" ev |> withDefault "#")
                ]
              <|
                List.intersperse (text " ") <|
                    [ A.icon (Dict.get "icon" ev |> withDefault "")
                    , strong [ class "ml-1" ] [ Dict.get "title" ev |> withDefault "" |> text ]
                    , span [ class "is-discrete" ] [ text T.in_ ]
                    , span [ class "is-strong" ] [ Dict.get "target" ev |> withDefault "" |> text ]

                    --, span [ class "has-text-grey-light pl-1" ] [ text "o/", Dict.get "orga" ev |> withDefault "" |> text ]
                    , text ":"
                    , span [] [ Dict.get "title_" ev |> withDefault "" |> text ]
                    ]
            , small [ class "help", classList [ ( "is-pulled-right", inline ) ] ] [ byAt conf (Username (Dict.get "author" ev |> withDefault "")) (Dict.get "date" ev |> withDefault "") ]
            ]
        ]


viewContractMedia : Conf -> Dict String String -> Html msg
viewContractMedia conf ev =
    div [ class "content" ]
        [ p [] <|
            [ a
                [ class "discrete-link"
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

                    --, span [ class "has-text-grey-light pl-1" ] [ text "o/", Dict.get "orga" ev |> withDefault "" |> text ]
                    ]
            , small [ class "help" ] [ byAt conf (Username (Dict.get "author" ev |> withDefault "")) (Dict.get "date" ev |> withDefault "") ]
            ]
        ]


viewNotifMedia : Conf -> Dict String String -> Html msg
viewNotifMedia conf ev =
    div [ class "content" ]
        [ a [ class "discrete-link", href (Dict.get "link" ev |> withDefault "#") ] <|
            List.intersperse (text " ") <|
                [ A.icon (Dict.get "icon" ev |> withDefault "")
                , Dict.get "title" ev |> withDefault "no input message." |> text
                ]
        , small [ class "help" ] [ byAt conf (Username (Dict.get "author" ev |> withDefault "")) (Dict.get "date" ev |> withDefault "") ]
        ]


eventTypeToText : TensionEvent.TensionEvent -> String
eventTypeToText e =
    case e of
        TensionEvent.Created ->
            T.created_event

        TensionEvent.Closed ->
            T.closed_event

        TensionEvent.Reopened ->
            T.reopened_event

        TensionEvent.CommentPushed ->
            T.commentPushed_event

        TensionEvent.Moved ->
            T.moved_event

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
            T.blobCommitted_event

        TensionEvent.BlobPushed ->
            T.blobCommitted_event

        TensionEvent.MemberLinked ->
            T.memberLinked_event

        TensionEvent.MemberUnlinked ->
            T.memberUnlinked_event

        TensionEvent.UserJoined ->
            T.userJoined_event

        TensionEvent.UserLeft ->
            T.userLeft_event

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


contractEventToText : Maybe NodeType.NodeType -> TensionEvent.TensionEvent -> String
contractEventToText ntm c =
    case c of
        TensionEvent.Moved ->
            case ntm of
                Nothing ->
                    T.moved_contract

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


cev2c : Maybe NodeType.NodeType -> TensionEvent.TensionEvent -> String
cev2c ntm c =
    case c of
        TensionEvent.Moved ->
            case ntm of
                Nothing ->
                    T.moved_contract_success

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


cev2p : Maybe NodeType.NodeType -> TensionEvent.TensionEvent -> String
cev2p ntm c =
    case c of
        TensionEvent.MemberLinked ->
            T.memberLinked_contract_success_ext

        TensionEvent.UserJoined ->
            T.userJoined_contract_success_ext

        _ ->
            cev2c ntm c


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

        _ ->
            ""
