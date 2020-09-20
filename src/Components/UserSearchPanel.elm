module Components.UserSearchPanel exposing
    ( UserSearchPanel
    , cancelEdit
    , click
    , clickAck
    , create
    , edit
    , setPattern
    , view
    , viewAssigneeSelectors
    , viewSelectors
    )

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
    , user : Maybe User

    -- Form
    , pattern : String
    , assignee : User -- last one clicked/selected
    , isNew : Bool -- toggle select
    , click_result : GqlData IdPayload
    }


create : UserState -> UserSearchPanel
create user =
    let
        -- Get the user requesting
        user_m =
            case user of
                LoggedIn uctx ->
                    Just { username = uctx.username, name = uctx.name }

                LoggedOut ->
                    Nothing
    in
    { isEdit = False
    , user = user_m

    -- Form
    , pattern = ""
    , assignee = User "" Nothing
    , isNew = False
    , click_result = NotAsked
    }


edit : UserSearchPanel -> UserSearchPanel
edit usp =
    { usp | isEdit = True }


cancelEdit : UserSearchPanel -> UserSearchPanel
cancelEdit usp =
    { usp | isEdit = False, click_result = NotAsked, pattern = "" }


setPattern : String -> UserSearchPanel -> UserSearchPanel
setPattern pattern usp =
    { usp | pattern = pattern }


click : User -> Bool -> UserSearchPanel -> UserSearchPanel
click user isNew usp =
    { usp | assignee = user, isNew = isNew }


clickAck : GqlData IdPayload -> UserSearchPanel -> UserSearchPanel
clickAck result usp =
    { usp | click_result = result }


type alias UserSearchPanelData msg =
    { selectedUsers : List User
    , targets : List String
    , users_data : GqlData UsersData
    , lookup : List User
    , tid : String
    , data : UserSearchPanel
    , onChangePattern : String -> msg
    , onUserClick : String -> User -> Bool -> msg
    }


view : UserSearchPanelData msg -> Html msg
view uspd =
    nav [ id "userSearchPanel", class "panel" ]
        [ case uspd.users_data of
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
                        if uspd.data.pattern == "" then
                            user
                                ++ uspd.selectedUsers
                                ++ linked_users
                                |> LE.uniqueBy (\u -> u.username)

                        else
                            LE.uniqueBy (\u -> u.username) uspd.lookup
                in
                div []
                    [ div [ class "panel-block" ]
                        [ p [ class "control has-icons-left" ]
                            [ input
                                [ class "input autofocus"
                                , type_ "text"
                                , placeholder T.searchUsers
                                , value uspd.data.pattern
                                , onInput uspd.onChangePattern
                                ]
                                []
                            , span [ class "icon is-left" ] [ i [ attribute "aria-hidden" "true", class "fas fa-search" ] [] ]
                            ]
                        ]
                    , case uspd.data.click_result of
                        Failure err ->
                            viewGqlErrors err

                        _ ->
                            div [] []
                    , viewAssigneeSelectors users uspd
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


viewAssigneeSelectors : List User -> UserSearchPanelData msg -> Html msg
viewAssigneeSelectors users uspd =
    div [ class "selectors" ] <|
        if users == [] then
            [ p [ class "panel-block" ] [ text T.noResultsFound ] ]

        else
            users
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
                            , onClick (uspd.onUserClick uspd.tid u (isActive == False))
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


viewSelectors i pattern op =
    div [ class "selectors", classList [ ( "spinner", op.users_data == Loading ) ] ] <|
        case op.users_data of
            Success ud ->
                let
                    users =
                        if pattern == "" then
                            -- linked users
                            op.targets
                                |> List.foldl
                                    (\a b ->
                                        List.append (Dict.get a ud |> withDefault []) b
                                    )
                                    []
                                |> LE.uniqueBy (\u -> u.username)

                        else
                            op.lookup
                in
                if users == [] then
                    [ p [ class "panel-block" ] [ text T.noResultsFound ] ]

                else
                    users
                        |> List.map
                            (\u ->
                                p
                                    [ class "panel-block"
                                    , onClick (op.onSelectUser i u.username)
                                    ]
                                    [ viewUser u.username
                                    , case u.name of
                                        Just name ->
                                            span [ class "has-text-weight-semibold" ] [ text name ]

                                        Nothing ->
                                            span [] []
                                    , span [ class "is-grey-light help" ] [ text u.username ]
                                    ]
                            )

            _ ->
                []
