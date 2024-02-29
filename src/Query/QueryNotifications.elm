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


module Query.QueryNotifications exposing
    ( queryNotifCount
    , queryNotifications
    )

import Fractal.Enum.ContractStatus as ContractStatus
import Fractal.Enum.UserEventOrderable as UserEventOrderable
import Fractal.InputObject as Input
import Fractal.Object
import Fractal.Object.EventCount
import Fractal.Object.User
import Fractal.Object.UserEvent
import Fractal.Query as Query
import Fractal.Union
import Fractal.Union.EventKind
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
                Fractal.Object.User.username
                (Fractal.Object.User.event_count identity
                    (SelectionSet.map3 NotifCount
                        (Fractal.Object.EventCount.unread_events |> SelectionSet.map (\a -> withDefault 0 a))
                        (Fractal.Object.EventCount.pending_contracts |> SelectionSet.map (\a -> withDefault 0 a))
                        (Fractal.Object.EventCount.assigned_tensions |> SelectionSet.map (\a -> withDefault 0 a))
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


userNotificationsPayload : NotificationsForm -> SelectionSet UserNotifications Fractal.Object.User
userNotificationsPayload f =
    SelectionSet.succeed (\a _ -> UserNotifications a)
        |> with (Fractal.Object.User.events (notificationsFilter f) notificationsPayload)
        -- @debug; needs of @isPrivate
        |> with Fractal.Object.User.username


notificationsPayload : SelectionSet UserEvent_ Fractal.Object.UserEvent
notificationsPayload =
    SelectionSet.succeed UserEvent_
        |> with (Fractal.Object.UserEvent.id |> SelectionSet.map decodedId)
        |> with Fractal.Object.UserEvent.isRead
        |> with (Fractal.Object.UserEvent.event contractFilter eventKindType)


eventKindType : SelectionSet EventKind Fractal.Union.EventKind
eventKindType =
    Fractal.Union.EventKind.fragments
        { onEvent = SelectionSet.map TensionEvent tensionEventPayload
        , onContract = SelectionSet.map ContractEvent contractEventPayload
        , onNotif = SelectionSet.map NotifEvent notifEventPayload
        }
