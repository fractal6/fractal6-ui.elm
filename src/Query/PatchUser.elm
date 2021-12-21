module Query.PatchUser exposing (toggleTensionSubscription)

import Dict exposing (Dict)
import Extra exposing (ternary)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.InputObject as Input
import Fractal.Mutation as Mutation
import Fractal.Object
import Fractal.Object.Node
import Fractal.Object.Tension
import Fractal.Object.UpdateUserPayload
import Fractal.Object.User
import Fractal.Object.UserRights
import Fractal.Scalar
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Query.QueryUser exposing (IsSubscribe, isSubscribePayload)
import RemoteData exposing (RemoteData)
import String.Extra as SE



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
