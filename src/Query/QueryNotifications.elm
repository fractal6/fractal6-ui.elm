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


module Query.QueryNotifications exposing
    ( queryNotifCount
    , queryNotifications
    )

import Schema.Enum.ContractStatus as ContractStatus
import Schema.Enum.UserEventOrderable as UserEventOrderable
import Schema.InputObject as Input
import Schema.Object
import Schema.Object.EventCount
import Schema.Object.User
import Schema.Object.UserEvent
import Schema.Query as Query
import Schema.Union
import Schema.Union.EventKind
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Query.QueryNode exposing (contractEventPayload, notifEventPayload, tensionEventPayload)
import Query.QueryUser exposing (usernameFilter)
import RemoteData exposing (RemoteData)



--
-- Query event count
--


notifCountDecoder : Maybe { event_count : Maybe NotifCount } -> Maybe NotifCount
notifCountDecoder data =
    data
        |> Maybe.map
            (\d ->
                d.event_count
            )
        |> withDefault Nothing


queryNotifCount url f msg =
    makeGQLQuery url
        (Query.getUser (usernameFilter f.uctx.username)
            (SelectionSet.map2 (\_ x -> { event_count = x })
                Schema.Object.User.username
                (Schema.Object.User.event_count identity
                    (SelectionSet.map3 NotifCount
                        (Schema.Object.EventCount.unread_events |> SelectionSet.map (\a -> withDefault 0 a))
                        (Schema.Object.EventCount.pending_contracts |> SelectionSet.map (\a -> withDefault 0 a))
                        (Schema.Object.EventCount.assigned_tensions |> SelectionSet.map (\a -> withDefault 0 a))
                    )
                )
            )
        )
        (RemoteData.fromResult >> decodeResponse notifCountDecoder >> msg)



--
-- Query UserEvent
--


userNotificationsDecoder : Maybe UserNotifications -> Maybe UserEvents
userNotificationsDecoder data =
    data
        |> Maybe.map
            (\user ->
                -- If  union type (EventKind)is not a list, the Maybe can be removed...
                user.events
                    |> Maybe.map
                        (\q ->
                            q
                                |> List.filterMap
                                    (\x ->
                                        case x.event of
                                            Just e ->
                                                UserEvent x.id x.isRead e |> Just

                                            Nothing ->
                                                Nothing
                                    )
                        )
            )
        |> withDefault Nothing


queryNotifications url f msg =
    makeGQLQuery url
        (Query.getUser (usernameFilter f.uctx.username)
            (userNotificationsPayload f)
        )
        (RemoteData.fromResult >> decodeResponse userNotificationsDecoder >> msg)


notificationsFilter : NotificationsForm -> Query.QueryUserEventOptionalArguments -> Query.QueryUserEventOptionalArguments
notificationsFilter f a =
    { a
        | first = Present f.first
        , order =
            Input.buildUserEventOrder
                (\x -> { x | desc = Present UserEventOrderable.CreatedAt })
                |> Present
    }


contractFilter a =
    -- @debug: see nested filter dgraph
    { a
        | filter =
            Input.buildEventKindFilter
                (\b ->
                    { b
                        | contractFilter =
                            Input.buildContractFilter
                                (\c ->
                                    { c | status = Present { eq = Present ContractStatus.Open, in_ = Absent } }
                                )
                                |> Present
                    }
                )
                |> Present
        , first = Absent
        , offset = Absent
    }



--
-- Payload
--


type alias UserEvent_ =
    { id : String, isRead : Bool, event : Maybe (List EventKind) }


type alias UserNotifications =
    { events : Maybe (List UserEvent_) }


userNotificationsPayload : NotificationsForm -> SelectionSet UserNotifications Schema.Object.User
userNotificationsPayload f =
    SelectionSet.succeed (\a _ -> UserNotifications a)
        |> with (Schema.Object.User.events (notificationsFilter f) notificationsPayload)
        -- @debug; needs of @isPrivate
        |> with Schema.Object.User.username


notificationsPayload : SelectionSet UserEvent_ Schema.Object.UserEvent
notificationsPayload =
    SelectionSet.succeed UserEvent_
        |> with (Schema.Object.UserEvent.id |> SelectionSet.map decodedId)
        |> with Schema.Object.UserEvent.isRead
        |> with (Schema.Object.UserEvent.event contractFilter eventKindType)


eventKindType : SelectionSet EventKind Schema.Union.EventKind
eventKindType =
    Schema.Union.EventKind.fragments
        { onEvent = SelectionSet.map TensionEvent tensionEventPayload
        , onContract = SelectionSet.map ContractEvent contractEventPayload
        , onNotif = SelectionSet.map NotifEvent notifEventPayload
        }
