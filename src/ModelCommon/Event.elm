module ModelCommon.Event exposing (..)

import Assets as A
import Dict exposing (Dict)
import Extra exposing (ternary, textH, upH)
import Extra.Date exposing (formatDate)
import Fractal.Enum.BlobType as BlobType
import Fractal.Enum.ContractType as ContractType
import Fractal.Enum.Lang as Lang
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.NodeVisibility as NodeVisibility
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Generated.Route as Route exposing (toHref)
import Global
import Html exposing (Html, a, br, button, div, hr, i, p, small, span, strong, sub, text)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id)
import Html.Events exposing (onClick)
import Identicon
import Maybe exposing (withDefault)
import ModelCommon exposing (UserState(..), getParentFragmentFromRole)
import ModelCommon.Codecs
    exposing
        ( ActionType(..)
        , FractalBaseRoute(..)
        , NodeFocus
        , getTensionCharac
        , nid2rootid
        , uriFromNameid
        , uriFromUsername
        )
import ModelCommon.View exposing (byAt, statusColor)
import ModelSchema exposing (ContractNotif, EmitterOrReceiver, EventNotif, Label, NodeExt, Post, Tension, User, UserCtx, UserEvent, Username)
import String.Extra as SE
import Text as T
import Time


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


viewEventMedia : Lang.Lang -> Time.Posix -> Bool -> Dict String String -> Html msg
viewEventMedia lang now inline ev =
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
                    , span [ class "is-italic" ] [ Dict.get "target" ev |> withDefault "" |> text ]

                    --, span [ class "has-text-grey-light pl-1" ] [ text "o/", Dict.get "orga" ev |> withDefault "" |> text ]
                    , text ":"
                    , span [ class "is-highlight-3" ] [ Dict.get "title_" ev |> withDefault "" |> text ]
                    ]
            , small [ class "help", classList [ ( "is-pulled-right", inline ) ] ] [ byAt lang now (Username (Dict.get "author" ev |> withDefault "")) (Dict.get "date" ev |> withDefault "") ]
            ]
        ]


viewContractMedia : Lang.Lang -> Time.Posix -> Dict String String -> Html msg
viewContractMedia lang now ev =
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
                    , span [ class "is-italic" ] [ Dict.get "target" ev |> withDefault "" |> text ]

                    --, span [ class "has-text-grey-light pl-1" ] [ text "o/", Dict.get "orga" ev |> withDefault "" |> text ]
                    ]
            , small [ class "help" ] [ byAt lang now (Username (Dict.get "author" ev |> withDefault "")) (Dict.get "date" ev |> withDefault "") ]
            ]
        ]


viewNotifMedia : Lang.Lang -> Time.Posix -> Dict String String -> Html msg
viewNotifMedia lang now ev =
    div [ class "content" ]
        [ a [ class "discrete-link", href (Dict.get "link" ev |> withDefault "#") ] <|
            List.intersperse (text " ") <|
                [ A.icon (Dict.get "icon" ev |> withDefault "")
                , Dict.get "title" ev |> withDefault "no input message." |> text
                ]
        , small [ class "help" ] [ byAt lang now (Username (Dict.get "author" ev |> withDefault "")) (Dict.get "date" ev |> withDefault "") ]
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
            "Coordinators Validation"

        ContractType.AnyCoordoTarget ->
            "Coordinator Validation"

        ContractType.AnyCoordoSource ->
            "Coordinator Validation"


contractEventToText : TensionEvent.TensionEvent -> String
contractEventToText c =
    case c of
        TensionEvent.Moved ->
            T.moved_contract

        TensionEvent.MemberLinked ->
            T.memberLinked_contract

        TensionEvent.MemberUnlinked ->
            T.memberUnlinked_contract

        TensionEvent.UserJoined ->
            T.userJoined_contract

        _ ->
            c |> TensionEvent.toString |> SE.humanize


contractToJonction : ContractType.ContractType -> String
contractToJonction c =
    case c of
        ContractType.AnyCandidates ->
            T.as_

        _ ->
            T.to


cev2c : TensionEvent.TensionEvent -> String
cev2c c =
    case c of
        TensionEvent.Moved ->
            T.moved_contract_success

        TensionEvent.MemberLinked ->
            T.memberLinked_contract_success

        TensionEvent.MemberUnlinked ->
            T.memberUnlinked_contract_success

        TensionEvent.UserJoined ->
            T.userJoined_contract_success

        _ ->
            "@TODO contractEventToText"


cev2p : TensionEvent.TensionEvent -> String
cev2p c =
    case c of
        TensionEvent.MemberLinked ->
            T.memberLinked_contract_success_ext

        TensionEvent.UserJoined ->
            T.userJoined_contract_success_ext

        _ ->
            cev2c c


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
