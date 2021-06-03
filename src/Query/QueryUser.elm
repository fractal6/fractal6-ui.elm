module Query.QueryUser exposing (queryUctx)

import Dict exposing (Dict)
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.InputObject as Input
import Fractal.Object
import Fractal.Object.Node
import Fractal.Object.NodeCharac
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



{-
   Query UserCtx
-}


type alias UserCtxX =
    { username : String
    , name : Maybe String
    , rights : UserRights
    , roles : List UserRoleX
    }


type alias UserRoleX =
    { name : String
    , nameid : String
    , rootnameid : String
    , role_type : RoleType.RoleType
    , isPrivate : Bool
    }


uctxXDecoder : Maybe UserCtxX -> Maybe UserCtx
uctxXDecoder data =
    data
        |> Maybe.map
            (\u ->
                UserCtx u.username
                    u.name
                    u.rights
                    (List.map
                        (\r -> UserRole r.name r.nameid r.rootnameid r.role_type)
                        u.roles
                    )
            )


queryUctx url username msg =
    makeGQLQuery url
        (Query.getUser
            (uctxFilter username)
            uctxPayload
        )
        (RemoteData.fromResult >> decodeResponse uctxXDecoder >> msg)


uctxFilter : String -> Query.GetUserOptionalArguments -> Query.GetUserOptionalArguments
uctxFilter username a =
    { a | username = Present username }


uctxPayload : SelectionSet UserCtxX Fractal.Object.User
uctxPayload =
    SelectionSet.succeed UserCtxX
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
            (Fractal.Object.User.roles pubFilter
                (SelectionSet.map5 UserRoleX
                    Fractal.Object.Node.name
                    Fractal.Object.Node.nameid
                    Fractal.Object.Node.rootnameid
                    (Fractal.Object.Node.role_type |> SelectionSet.map (\x -> withDefault RoleType.Peer x))
                    Fractal.Object.Node.isPrivate
                )
                |> SelectionSet.map (\x -> withDefault [] x)
            )


pubFilter : Query.QueryNodeOptionalArguments -> Query.QueryNodeOptionalArguments
pubFilter a =
    { a
        | filter =
            Input.buildNodeFilter
                (\b -> { b | isPrivate = Present False })
                |> Present
    }
