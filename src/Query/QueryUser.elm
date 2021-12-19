module Query.QueryUser exposing (queryUctx, queryUser, uctxFilter)

import Dict exposing (Dict)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.InputObject as Input
import Fractal.Object
import Fractal.Object.Node
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

   Query UserCtx
-}


queryUctx url username msg =
    makeGQLQuery url
        (Query.getUser
            (uctxFilter username)
            uctxPayload
        )
        (RemoteData.fromResult >> decodeResponse identity >> msg)


uctxFilter : String -> Query.GetUserOptionalArguments -> Query.GetUserOptionalArguments
uctxFilter username a =
    { a | username = Present username }


uctxPayload : SelectionSet UserCtx Fractal.Object.User
uctxPayload =
    SelectionSet.succeed UserCtx
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
