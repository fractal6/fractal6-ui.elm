module Query.QueryUser exposing
    ( IsSubscribe
    , getIsSubscribe
    , isSubscribePayload
    , queryUser
    , queryUserFull
    , queryUserProfile
    , userFullPayload
    , userProfilePayload
    , usernameFilter
    )

import Dict exposing (Dict)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.InputObject as Input
import Fractal.Object
import Fractal.Object.Node
import Fractal.Object.Tension
import Fractal.Object.User
import Fractal.Object.UserRights
import Fractal.Query as Query
import Fractal.Scalar
import GqlClient exposing (..)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import RemoteData exposing (RemoteData)
import String.Extra as SE



{-

   Query User
-}


usernameFilter : String -> Query.GetUserOptionalArguments -> Query.GetUserOptionalArguments
usernameFilter username a =
    { a | username = Present username }


queryUserProfile url username msg =
    makeGQLQuery url
        (Query.getUser
            (usernameFilter username)
            userProfilePayload
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)


queryUserFull url username msg =
    makeGQLQuery url
        (Query.getUser
            (usernameFilter username)
            userFullPayload
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)


userProfilePayload : SelectionSet UserProfile Fractal.Object.User
userProfilePayload =
    SelectionSet.succeed UserProfile
        |> with Fractal.Object.User.username
        |> with Fractal.Object.User.name
        |> with
            (Fractal.Object.User.rights identity <|
                SelectionSet.map3 UserRights
                    Fractal.Object.UserRights.canLogin
                    Fractal.Object.UserRights.canCreateRoot
                    Fractal.Object.UserRights.type_
            )
        |> with
            (Fractal.Object.User.roles identity
                (SelectionSet.map3 UserRole
                    Fractal.Object.Node.name
                    Fractal.Object.Node.nameid
                    (Fractal.Object.Node.role_type |> SelectionSet.map (\x -> withDefault RoleType.Peer x))
                )
                |> SelectionSet.map (\x -> withDefault [] x)
            )
        |> with Fractal.Object.User.notifyByEmail
        |> with Fractal.Object.User.lang
        |> with Fractal.Object.User.bio
        |> with Fractal.Object.User.location



-- @DEBUG:
-- 1. import withFragment does not work ?!?!
-- 2. cant managed to use **extensible record** with selectionset.


userFullPayload : SelectionSet UserFull Fractal.Object.User
userFullPayload =
    SelectionSet.succeed UserFull
        |> with Fractal.Object.User.username
        |> with Fractal.Object.User.name
        |> with
            (Fractal.Object.User.rights identity <|
                SelectionSet.map3 UserRights
                    Fractal.Object.UserRights.canLogin
                    Fractal.Object.UserRights.canCreateRoot
                    Fractal.Object.UserRights.type_
            )
        |> with
            (Fractal.Object.User.roles identity
                (SelectionSet.map3 UserRole
                    Fractal.Object.Node.name
                    Fractal.Object.Node.nameid
                    (Fractal.Object.Node.role_type |> SelectionSet.map (\x -> withDefault RoleType.Peer x))
                )
                |> SelectionSet.map (\x -> withDefault [] x)
            )
        |> with Fractal.Object.User.notifyByEmail
        |> with Fractal.Object.User.lang
        |> with Fractal.Object.User.bio
        |> with Fractal.Object.User.location
        |> with Fractal.Object.User.email



{-
   Query Users
-}


usersDecoder : Maybe (List (Maybe user)) -> Maybe (List user)
usersDecoder data =
    data
        |> Maybe.map
            (\d ->
                d
                    |> List.filterMap identity
                    |> Just
            )
        |> withDefault Nothing


queryUser url userfrag msg =
    makeGQLQuery url
        (Query.queryUser
            (userFilter userfrag)
            userPayload
        )
        (RemoteData.fromResult >> decodeResponse usersDecoder >> msg)


userFilter : String -> Query.QueryUserOptionalArguments -> Query.QueryUserOptionalArguments
userFilter userfrag a =
    let
        userreg =
            "^"
                ++ userfrag
                |> SE.surround "/"
    in
    { a
        | first = Present 30
        , filter =
            Input.buildUserFilter
                (\b ->
                    { b
                        | username = { regexp = Present userreg, eq = Absent, in_ = Absent } |> Present
                        , or =
                            Present
                                [ Input.buildUserFilter
                                    (\c ->
                                        { c | name = { regexp = Present userreg } |> Present }
                                    )
                                    |> Just
                                ]
                    }
                )
                |> Present
    }


userPayload : SelectionSet User Fractal.Object.User
userPayload =
    SelectionSet.succeed User
        |> with Fractal.Object.User.username
        |> with Fractal.Object.User.name



{-
   Check if user is subscribe to the given tension
-}


isSubscribeDecoder : Maybe IsSubscribe -> Maybe Bool
isSubscribeDecoder data =
    data
        |> Maybe.map
            (\d ->
                d.subscriptions /= Nothing && d.subscriptions /= Just []
            )


getIsSubscribe url username tid msg =
    makeGQLQuery url
        (Query.getUser
            (usernameFilter username)
            (isSubscribePayload tid)
        )
        (RemoteData.fromResult >> decodeResponse isSubscribeDecoder >> msg)


type alias IsSubscribe =
    { subscriptions : Maybe (List IdPayload)

    -- @debug; needs of @isPrivate
    , username : String
    }


isSubscribePayload : String -> SelectionSet IsSubscribe Fractal.Object.User
isSubscribePayload tid =
    SelectionSet.map2 IsSubscribe
        (Fractal.Object.User.subscriptions (\a -> { a | filter = Present <| Input.buildTensionFilter (\x -> { x | id = Present [ encodeId tid ] }) })
            (SelectionSet.map IdPayload (Fractal.Object.Tension.id |> SelectionSet.map decodedId))
        )
        Fractal.Object.User.username
