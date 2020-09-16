module Components.UserSearchPanel exposing (UserSearchPanel, cancelEdit, create, edit, refresh, view)

import Components.Fa as Fa
import Components.Loading as Loading exposing (viewGqlErrors)
import Components.Text as T
import Dict exposing (Dict)
import Extra exposing (ternary, withMapData, withMaybeData)
import Html exposing (Html, a, br, button, canvas, datalist, div, h1, h2, hr, i, input, label, li, nav, option, p, select, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, name, placeholder, required, rows, selected, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (UserState(..))
import ModelCommon.Codecs exposing (nearestCircleid)
import ModelCommon.View exposing (viewUser)
import ModelSchema exposing (..)
import Time


type alias UserSearchPanel =
    { isEdit : Bool
    , users_data : GqlData (Dict String (List User))
    , user : Maybe User
    }


create : UserState -> GqlData NodesData -> UserSearchPanel
create user nd_d =
    let
        -- Make a dict of user per circle
        ud =
            withMapData
                (\nd ->
                    nd
                        |> Dict.toList
                        |> List.map (\( k, n ) -> Maybe.map (\fs -> ( nearestCircleid k, { username = fs.username, name = fs.name } )) n.first_link)
                        |> List.filterMap identity
                        |> toUserMap
                )
                nd_d

        -- Get the user requesting
        user_m =
            case user of
                LoggedIn uctx ->
                    case ud of
                        Success uud ->
                            uud
                                |> Dict.values
                                |> List.concat
                                |> List.filter (\u -> u.username == uctx.username)
                                |> List.head
                                |> Just

                        _ ->
                            Nothing

                LoggedOut ->
                    Nothing
    in
    { isEdit = False
    , users_data = ud
    , user = user_m |> withDefault Nothing
    }


refresh : UserState -> GqlData NodesData -> UserSearchPanel -> UserSearchPanel
refresh user nd_d usp =
    let
        nd_new =
            create user nd_d
    in
    { usp
        | users_data = nd_new.users_data
        , user = nd_new.user
    }


edit : UserSearchPanel -> UserSearchPanel
edit usp =
    { usp | isEdit = True }


cancelEdit : UserSearchPanel -> UserSearchPanel
cancelEdit usp =
    { usp | isEdit = False }


type alias UserSearchPanelData =
    { selectedUsers : List User
    , targets : List String
    , data : UserSearchPanel
    }


view : UserSearchPanelData -> Html msg
view uspd =
    nav [ id "userSearchPanel", class "panel" ]
        [ case uspd.data.users_data of
            Success ud ->
                let
                    user =
                        Maybe.map (\u -> [ u ]) uspd.data.user |> withDefault []

                    linked_users =
                        List.foldl
                            (\a b ->
                                List.append (Dict.get a ud |> withDefault []) b
                            )
                            []
                            uspd.targets

                    users =
                        user
                            ++ uspd.selectedUsers
                            ++ linked_users
                            |> LE.uniqueBy (\u -> u.username)
                in
                div []
                    [ div [ class "panel-block" ]
                        [ p [ class "control has-icons-left" ]
                            [ input [ class "input autofocus", placeholder "Search users", type_ "text" ] []
                            , span [ class "icon is-left" ] [ i [ attribute "aria-hidden" "true", class "fas fa-search" ] [] ]
                            ]
                        ]
                    , div [ class "selectors" ] <|
                        (users
                            |> List.map
                                (\u ->
                                    let
                                        isActive =
                                            List.member u uspd.selectedUsers

                                        faCls =
                                            ternary isActive "fa-check-square" "fa-square"
                                    in
                                    p
                                        [ class "panel-block"
                                        , classList [ ( "is-active", isActive ) ]
                                        ]
                                        [ span [ class "panel-icon" ] [ Fa.icon0 ("far " ++ faCls) "" ]
                                        , viewUser u.username
                                        , case u.name of
                                            Just name ->
                                                span [ class "has-text-weight-semibold" ] [ text name ]

                                            Nothing ->
                                                span [] []
                                        , span [ class "is-grey-light help" ] [ text u.username ]
                                        ]
                                )
                        )
                    ]

            Loading ->
                div [ class "spinner" ] [ text "" ]

            LoadingSlowly ->
                div [ class "spinner" ] [ text "" ]

            NotAsked ->
                div [] []

            Failure err ->
                viewGqlErrors err
        ]



--- Utils


toUserMap : List ( String, User ) -> Dict String (List User)
toUserMap parameters =
    List.foldl
        (\( k, v ) dict -> Dict.update k (addParam v) dict)
        Dict.empty
        parameters


addParam : User -> Maybe (List User) -> Maybe (List User)
addParam value maybeValues =
    case maybeValues of
        Just values ->
            Just (value :: values)

        Nothing ->
            Just [ value ]
