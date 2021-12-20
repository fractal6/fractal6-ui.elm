module Query.QueryNotifications exposing (queryNotifications)

import Dict exposing (Dict)
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Fractal.Enum.UserEventOrderable as UserEventOrderable
import Fractal.InputObject as Input
import Fractal.Object
import Fractal.Object.AddNodePayload
import Fractal.Object.Contract
import Fractal.Object.Event
import Fractal.Object.Mandate
import Fractal.Object.Node
import Fractal.Object.Tension
import Fractal.Object.User
import Fractal.Object.UserEvent
import Fractal.Query as Query
import Fractal.Scalar
import Fractal.Union
import Fractal.Union.EventKind
import GqlClient exposing (..)
import Graphql.Internal.Builder.Object as Object
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Query.QueryNode exposing (nidFilter, nodeDecoder, nodeOrgaPayload, pNodePayload)
import Query.QueryUser exposing (uctxFilter)
import RemoteData exposing (RemoteData)



{-
   Query UserEvent
-}


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
                                |> List.map
                                    (\x ->
                                        case x.event of
                                            Just e ->
                                                UserEvent x.isRead e |> Just

                                            Nothing ->
                                                Nothing
                                    )
                                |> List.filterMap identity
                        )
            )
        |> withDefault Nothing


queryNotifications url f msg =
    makeGQLQuery url
        (Query.getUser (uctxFilter f.uctx.username)
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



--
-- Payload
--


type alias UserEvent_ =
    { isRead : Bool, event : Maybe (List EventKind) }


type alias UserNotifications =
    { events : Maybe (List UserEvent_) }


userNotificationsPayload : NotificationsForm -> SelectionSet UserNotifications Fractal.Object.User
userNotificationsPayload f =
    SelectionSet.succeed UserNotifications
        |> with (Fractal.Object.User.events (notificationsFilter f) notificationsPayload)


notificationsPayload : SelectionSet UserEvent_ Fractal.Object.UserEvent
notificationsPayload =
    SelectionSet.succeed UserEvent_
        |> with Fractal.Object.UserEvent.isRead
        |> with (Fractal.Object.UserEvent.event identity eventKindType)


eventKindType : SelectionSet EventKind Fractal.Union.EventKind
eventKindType =
    Fractal.Union.EventKind.fragments
        { onEvent = SelectionSet.map TensionEvent tensionEventPayload
        , onContract = SelectionSet.map ContractEvent contractEventPayload
        }


tensionEventPayload : SelectionSet EventNotif Fractal.Object.Event
tensionEventPayload =
    SelectionSet.succeed EventNotif
        |> with (Fractal.Object.Event.id |> SelectionSet.map decodedId)
        |> with (Fractal.Object.Event.createdAt |> SelectionSet.map decodedTime)
        |> with (Fractal.Object.Event.createdBy identity <| SelectionSet.map Username Fractal.Object.User.username)
        |> with Fractal.Object.Event.event_type
        |> with
            (Fractal.Object.Event.tension identity
                (SelectionSet.map (\x -> { receiver = x })
                    (Fractal.Object.Tension.receiver identity pNodePayload)
                )
            )


contractEventPayload : SelectionSet ContractNotif Fractal.Object.Contract
contractEventPayload =
    SelectionSet.succeed ContractNotif
        |> with (Fractal.Object.Contract.id |> SelectionSet.map decodedId)
        |> with (Fractal.Object.Contract.createdAt |> SelectionSet.map decodedTime)
        |> with (Fractal.Object.Contract.createdBy identity <| SelectionSet.map Username Fractal.Object.User.username)
        |> with Fractal.Object.Contract.contract_type
        |> with
            (Fractal.Object.Contract.tension identity
                (SelectionSet.map (\x -> { receiver = x })
                    (Fractal.Object.Tension.receiver identity pNodePayload)
                )
            )
