module Query.QueryNotifications exposing (queryNotifications)

import Dict exposing (Dict)
import Fractal.Enum.ContractStatus as ContractStatus
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
import Fractal.Object.EventFragment
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
import Query.QueryUser exposing (usernameFilter)
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
                                                UserEvent x.id x.isRead e |> Just

                                            Nothing ->
                                                Nothing
                                    )
                                |> List.filterMap identity
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
    SelectionSet.succeed UserNotifications
        |> with (Fractal.Object.User.events (notificationsFilter f) notificationsPayload)


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
        }


tensionEventPayload : SelectionSet EventNotif Fractal.Object.Event
tensionEventPayload =
    SelectionSet.succeed EventNotif
        |> with (Fractal.Object.Event.createdAt |> SelectionSet.map decodedTime)
        |> with (Fractal.Object.Event.createdBy identity <| SelectionSet.map Username Fractal.Object.User.username)
        |> with Fractal.Object.Event.event_type
        |> with
            (Fractal.Object.Event.tension identity
                (SelectionSet.map3 (\a b c -> { id = a, receiver = b, title = c })
                    (Fractal.Object.Tension.id |> SelectionSet.map decodedId)
                    (Fractal.Object.Tension.receiver identity pNodePayload)
                    Fractal.Object.Tension.title
                )
            )


contractEventPayload : SelectionSet ContractNotif Fractal.Object.Contract
contractEventPayload =
    SelectionSet.succeed ContractNotif
        |> with (Fractal.Object.Contract.id |> SelectionSet.map decodedId)
        |> with (Fractal.Object.Contract.createdAt |> SelectionSet.map decodedTime)
        |> with (Fractal.Object.Contract.createdBy identity <| SelectionSet.map Username Fractal.Object.User.username)
        |> with Fractal.Object.Contract.contract_type
        |> with (Fractal.Object.Contract.event identity <| SelectionSet.map (\x -> { event_type = x }) Fractal.Object.EventFragment.event_type)
        |> with
            (Fractal.Object.Contract.tension identity
                (SelectionSet.map2 (\a b -> { id = a, receiver = b })
                    (Fractal.Object.Tension.id |> SelectionSet.map decodedId)
                    (Fractal.Object.Tension.receiver identity pNodePayload)
                )
            )
