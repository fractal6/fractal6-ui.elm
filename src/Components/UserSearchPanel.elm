module Components.UserSearchPanel exposing (..)

import Components.Fa as Fa
import Components.Loading as Loading exposing (GqlData, RequestResult(..), viewGqlErrors)
import Components.Text as T
import Dict exposing (Dict)
import Extra exposing (ternary, withMapData, withMaybeData)
import Fractal.Enum.TensionEvent as TensionEvent
import Html exposing (Html, a, br, button, canvas, datalist, div, h1, h2, hr, i, input, label, li, nav, option, p, select, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, name, placeholder, required, rows, selected, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (AssigneeForm, UserState(..), initAssigneeForm)
import ModelCommon.Codecs exposing (nearestCircleid)
import ModelCommon.View exposing (viewUser)
import ModelSchema exposing (..)
import Time


type alias UserSearchPanel =
    { isEdit : Bool
    , form : AssigneeForm
    , click_result : GqlData IdPayload
    }


create : UserState -> String -> UserSearchPanel
create user tid =
    { isEdit = False
    , form = initAssigneeForm user tid
    , click_result = NotAsked
    }



-- State control


edit : UserSearchPanel -> UserSearchPanel
edit data =
    { data | isEdit = True }


cancelEdit : UserSearchPanel -> UserSearchPanel
cancelEdit data =
    let
        form =
            data.form
    in
    { data | isEdit = False, click_result = NotAsked, form = { form | pattern = "" } }


click : User -> Bool -> UserSearchPanel -> UserSearchPanel
click user isNew data =
    let
        form =
            data.form
    in
    { data | form = { form | assignee = user, isNew = isNew } }


setClickResult : GqlData IdPayload -> UserSearchPanel -> UserSearchPanel
setClickResult result data =
    { data | click_result = result }



-- Update Form


setEvents : List TensionEvent.TensionEvent -> UserSearchPanel -> UserSearchPanel
setEvents events data =
    let
        f =
            data.form
    in
    { data | form = { f | events_type = Just events } }


post : String -> String -> UserSearchPanel -> UserSearchPanel
post field value data =
    let
        f =
            data.form
    in
    { data | form = { f | post = Dict.insert field value f.post } }


setPattern : String -> UserSearchPanel -> UserSearchPanel
setPattern pattern data =
    let
        form =
            data.form
    in
    { data | form = { form | pattern = pattern } }


type alias UserSearchPanelData msg =
    { selectedUsers : List User
    , targets : List String
    , users_data : GqlData UsersData
    , lookup : List User
    , data : UserSearchPanel
    , onChangePattern : String -> msg
    , onUserClick : User -> Bool -> Time.Posix -> msg
    , onSubmit : (Time.Posix -> msg) -> msg
    }


view : UserSearchPanelData msg -> Html msg
view uspd =
    nav [ id "userSearchPanel", class "panel" ]
        [ case uspd.users_data of
            Success ud ->
                let
                    user =
                        uspd.data.form.uctx |> List.singleton |> List.map (\u -> User u.username u.name)

                    linked_users =
                        List.foldl
                            (\a b ->
                                List.append (Dict.get a ud |> withDefault []) b
                            )
                            []
                            uspd.targets

                    users =
                        if uspd.data.form.pattern == "" then
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
                                [ id "userInput"
                                , class "input autofocus"
                                , type_ "text"
                                , placeholder T.searchUsers
                                , value uspd.data.form.pattern
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
                            , onClick (uspd.onSubmit <| uspd.onUserClick u (isActive == False))
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
