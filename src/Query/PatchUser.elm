module Query.PatchUser exposing
    ( markAllAsRead
    , markAsRead
    , patchUser
    , toggleTensionSubscription
    )

import Dict exposing (Dict)
import Extra exposing (ternary)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.InputObject as Input
import Fractal.Mutation as Mutation
import Fractal.Object
import Fractal.Object.Node
import Fractal.Object.Tension
import Fractal.Object.UpdateUserEventPayload
import Fractal.Object.UpdateUserPayload
import Fractal.Object.User
import Fractal.Object.UserEvent
import Fractal.Object.UserRights
import Fractal.Scalar
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..), fromMaybe)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with)
import Maybe exposing (withDefault)
import ModelCommon exposing (UserProfileForm)
import ModelSchema exposing (..)
import Query.QueryUser exposing (IsSubscribe, isSubscribePayload, userFullPayload, userProfilePayload)
import RemoteData exposing (RemoteData)
import String.Extra as SE



{-
   Update settings
-}


type alias UserPatchPayload =
    { user : Maybe (List (Maybe UserFull)) }


userPatchDecoder : Maybe UserPatchPayload -> Maybe UserFull
userPatchDecoder data =
    data
        |> Maybe.andThen
            (\d ->
                d.user
                    |> Maybe.map (List.filterMap identity)
                    |> withDefault []
                    |> List.head
            )


patchUser url form msg =
    makeGQLMutation url
        (Mutation.updateUser
            (userProfileInputEncoder form)
            (SelectionSet.map UserPatchPayload <|
                Fractal.Object.UpdateUserPayload.user identity
                    userFullPayload
            )
        )
        (RemoteData.fromResult >> decodeResponse userPatchDecoder >> msg)


userProfileInputEncoder : UserProfileForm -> Mutation.UpdateUserRequiredArguments
userProfileInputEncoder form =
    let
        inputReq =
            { filter =
                Input.buildUserFilter
                    (\f ->
                        { f | username = { eq = Present form.username, regexp = Absent, in_ = Absent } |> Present }
                    )
            }

        inputOpt =
            \_ ->
                { set =
                    Input.buildUserPatch
                        (\s ->
                            { s
                                | name = fromMaybe (Dict.get "name" form.post)
                                , bio = fromMaybe (Dict.get "bio" form.post)
                                , location = fromMaybe (Dict.get "location" form.post)
                                , lang = fromMaybe form.lang
                                , notifyByEmail = fromMaybe form.notifyByEmail
                            }
                        )
                        |> Present
                , remove = Absent
                }
    in
    { input = Input.buildUpdateUserInput inputReq inputOpt }



{-
   Toggle tension subscriptions
-}


type alias UserIsSubscribe =
    { user : Maybe (List (Maybe IsSubscribe)) }


isSubscribeDecoder : Maybe UserIsSubscribe -> Maybe Bool
isSubscribeDecoder data =
    case data of
        Just d ->
            d.user
                |> Maybe.map
                    (\items ->
                        List.filterMap identity items
                    )
                |> withDefault []
                |> List.head
                |> Maybe.map (\x -> x.subscriptions /= Nothing && x.subscriptions /= Just [])

        Nothing ->
            Nothing


toggleTensionSubscription url username tensionid doSet msg =
    makeGQLMutation url
        (Mutation.updateUser
            (toggleSubscriptionInput username tensionid doSet)
            (SelectionSet.map UserIsSubscribe <|
                Fractal.Object.UpdateUserPayload.user identity (isSubscribePayload tensionid)
            )
        )
        (RemoteData.fromResult >> decodeResponse isSubscribeDecoder >> msg)


toggleSubscriptionInput : String -> String -> Bool -> Mutation.UpdateUserRequiredArguments
toggleSubscriptionInput username tid doSet =
    let
        inputReq =
            { filter =
                Input.buildUserFilter
                    (\ft ->
                        { ft | username = { eq = Present username, regexp = Absent, in_ = Absent } |> Present }
                    )
            }

        patch =
            Input.buildUserPatch
                (\s ->
                    { s
                        | subscriptions =
                            Present
                                [ Input.buildTensionRef (\u -> { u | id = tid |> encodeId |> Present }) ]
                    }
                )
                |> Present

        inputOpt =
            \_ ->
                { set = ternary doSet patch Absent
                , remove = ternary doSet Absent patch
                }
    in
    { input = Input.buildUpdateUserInput inputReq inputOpt }



{-
   Mark notif as read
-}


type alias PatchUserPayload =
    { user : Maybe (List (Maybe IdPayload)) }


userIdDecoder : Maybe PatchUserPayload -> Maybe IdPayload
userIdDecoder data =
    data
        |> Maybe.andThen
            (\d ->
                d.user
                    |> Maybe.map (List.filterMap identity)
                    |> withDefault []
                    |> List.head
            )


markAsRead url eid isRead msg =
    makeGQLMutation url
        (Mutation.updateUserEvent
            (markAsReadInput eid isRead)
            (SelectionSet.map PatchUserPayload <|
                Fractal.Object.UpdateUserEventPayload.userEvent identity
                    -- markAsReadPayload
                    (SelectionSet.map IdPayload
                        (SelectionSet.map decodedId Fractal.Object.UserEvent.id)
                    )
            )
        )
        (RemoteData.fromResult >> decodeResponse userIdDecoder >> msg)


markAsReadInput : String -> Bool -> Mutation.UpdateUserEventRequiredArguments
markAsReadInput eid isRead =
    let
        inputReq =
            { filter =
                Input.buildUserEventFilter
                    (\ft -> { ft | id = Present [ encodeId eid ] })
            }

        patch =
            Input.buildUserEventPatch
                (\s ->
                    { s | isRead = Present isRead }
                )
                |> Present

        inputOpt =
            \_ ->
                { set = patch
                , remove = Absent
                }
    in
    { input = Input.buildUpdateUserEventInput inputReq inputOpt }



{-
   Mark all notif as read
-}


markAllAsRead url username msg =
    makeGQLMutation url
        (Mutation.updateUser
            (markAllAsReadInput username)
            (SelectionSet.map PatchUserPayload <|
                Fractal.Object.UpdateUserPayload.user identity
                    -- markAsReadPayload
                    (SelectionSet.map IdPayload
                        (SelectionSet.map decodedId Fractal.Object.User.id)
                    )
            )
        )
        (RemoteData.fromResult >> decodeResponse userIdDecoder >> msg)


markAllAsReadInput : String -> Mutation.UpdateUserRequiredArguments
markAllAsReadInput username =
    let
        inputReq =
            { filter =
                Input.buildUserFilter
                    (\ft ->
                        { ft | username = { eq = Present username, regexp = Absent, in_ = Absent } |> Present }
                    )
            }

        patch =
            Input.buildUserPatch
                (\s ->
                    { s | markAllAsRead = Present "" }
                )
                |> Present

        inputOpt =
            \_ ->
                { set = patch
                , remove = Absent
                }
    in
    { input = Input.buildUpdateUserInput inputReq inputOpt }