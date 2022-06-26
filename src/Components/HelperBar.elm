module Components.HelperBar exposing (HelperBar, collapse, create, expand, view)

import Array
import Assets as A
import Components.Loading as Loading exposing (GqlData, RequestResult(..))
import Form.NewTension exposing (NewTensionInput(..))
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.NodeVisibility as NodeVisibility
import Fractal.Enum.RoleType as RoleType
import Generated.Route as Route exposing (Route, toHref)
import Html exposing (Html, a, br, button, div, h1, h2, hr, i, input, li, nav, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, placeholder, rows, type_)
import Html.Events exposing (onClick)
import Json.Decode as JD
import Maybe exposing (withDefault)
import ModelCommon exposing (UserState(..), getParentFragmentFromRole)
import ModelCommon.Codecs exposing (DocType(..), FractalBaseRoute(..), NodeFocus, getOrgaRoles, isPending, nid2rootid, nid2type, uriFromNameid)
import ModelCommon.View exposing (action2icon, roleColor, viewRole)
import ModelSchema exposing (LocalGraph, UserRole, getSourceTid)
import Ports
import Text as T exposing (textH, textT)


type HelperBar
    = Expanded
    | Collapsed


create : HelperBar
create =
    Collapsed


expand : HelperBar -> HelperBar
expand hb =
    Expanded


collapse : HelperBar -> HelperBar
collapse hb =
    Collapsed


numberRolesCollapsed : Int
numberRolesCollapsed =
    5


type alias Op msg =
    { baseUri : FractalBaseRoute
    , uriQuery : Maybe String
    , user : UserState
    , path_data : Maybe LocalGraph
    , focus : NodeFocus
    , data : HelperBar
    , onExpand : msg
    , onCollapse : msg
    , onToggleTreeMenu : msg
    }


view : Op msg -> Html msg
view op =
    -- @debug: padding-top overflow column.with is-paddingless
    div [ id "helperBar", class "columns is-centered is-marginless" ]
        [ div [ class "column is-12 is-11-desktop is-10-fullhd is-paddingless" ]
            [ div [ class "ml-3 mb-5 mx-mobile" ] [ viewPathLevel op ]
            , viewNavLevel op
            ]
        ]


viewPathLevel : Op msg -> Html msg
viewPathLevel op =
    let
        ( rootnameid, userCanJoin ) =
            case op.path_data of
                Just path ->
                    ( path.root |> Maybe.map (\r -> r.nameid) |> withDefault ""
                    , path.root |> Maybe.map (\r -> r.userCanJoin == Just True) |> withDefault False
                    )

                Nothing ->
                    ( "", False )
    in
    nav [ class "level" ]
        [ div [ class "level-left" ] [ viewPath op.baseUri op.uriQuery op.path_data op.onToggleTreeMenu ]
        , div [ class "level-right mt-0" ]
            [ A.burger "rolesMenu"
            , div [ id "rolesMenu", class "navbar-menu" ]
                [ case op.user of
                    LoggedIn uctx ->
                        case op.path_data of
                            Just path ->
                                let
                                    roles =
                                        getOrgaRoles [ rootnameid ] uctx.roles
                                in
                                if isPending uctx rootnameid then
                                    -- show Pending button (redicrect to contract)...
                                    div [ id "joinPending", class "button is-small has-text-weight-semibold is-warning joinPendingTrigger" ]
                                        [ textH "Pending invitation" ]

                                else if List.length roles == 0 && userCanJoin then
                                    joinButton

                                else
                                    memberButtons roles { op | baseUri = OverviewBaseUri }

                            Nothing ->
                                div [ class "ph-button-1" ] []

                    LoggedOut ->
                        if userCanJoin then
                            joinButton

                        else
                            text ""
                ]
            ]
        ]


viewNavLevel : Op msg -> Html msg
viewNavLevel op =
    let
        focusid =
            Maybe.map (\x -> x.focus.nameid) op.path_data
                |> withDefault op.focus.nameid
    in
    nav [ class "tabs is-boxed" ]
        [ ul [ class "" ]
            ([ li [ classList [ ( "is-active", op.baseUri == OverviewBaseUri ) ] ]
                [ a [ href (uriFromNameid OverviewBaseUri focusid []) ] [ A.icon1 "icon-sun" "Overview" ] ]
             , li [ classList [ ( "is-active", op.baseUri == TensionsBaseUri ) ] ]
                [ a [ href (uriFromNameid TensionsBaseUri focusid []) ] [ A.icon1 "icon-exchange" "Tensions" ] ]

             --[ a [ href (uriFromNameid TensionsBaseUri focusid) ]
             --    [ div [ class "dropdown is-hoverable" ]
             --        [ div [ class "dropdown-trigger", attribute "aria-haspopup" "true", attribute "aria-controls" "tension-menu" ] [ A.icon1 "icon-exchange" "Tensions" ]
             --        , div [ class "dropdown-menu", id "tension-menu", attribute "role" "menu" ]
             --            [ div [ class "dropdown-content" ]
             --                [ p [ class "dropdown-item" ] [ text "List" ]
             --                ]
             --            ]
             --        ]
             --    ]
             --]
             , li [ classList [ ( "is-active", op.baseUri == MembersBaseUri ) ] ]
                [ a [ href (uriFromNameid MembersBaseUri focusid []) ] [ A.icon1 "icon-user" "Members" ] ]
             ]
                ++ (Maybe.map
                        (\path ->
                            if op.user /= LoggedOut && path.focus.type_ == NodeType.Circle then
                                [ li [ class "is-vbar-2" ] []
                                , li [ classList [ ( "is-active", op.baseUri == SettingsBaseUri ) ] ]
                                    [ a [ href (uriFromNameid SettingsBaseUri focusid []) ] [ A.icon1 "icon-settings" "Settings" ] ]
                                ]

                            else
                                []
                        )
                        op.path_data
                        |> withDefault []
                   )
            )

        -- "Create tension" button in the tab bar.
        --, div
        --    ([ class "button is-small is-link2 is-rounded is-pulled-right"
        --     , attribute "style" "bottom:-5px;"
        --     ]
        --        ++ (case op.path_data of
        --                Just p ->
        --                    [ onClick (op.onCreateTension p) ]
        --                Nothing ->
        --                    []
        --           )
        --    )
        --    [ A.icon1 "icon-send" "Create tension" ]
        ]


viewPath : FractalBaseRoute -> Maybe String -> Maybe LocalGraph -> msg -> Html msg
viewPath baseUri uriQuery maybePath onToggleTreeMenu =
    div
        [ class "breadcrumb has-arrow-separato"
        , attribute "aria-label" "breadcrumbs"
        ]
    <|
        --A.icon0 "icon-layers icon-lg" ,
        case maybePath of
            Just g ->
                let
                    q =
                        uriQuery |> Maybe.map (\uq -> "?" ++ uq) |> Maybe.withDefault ""
                in
                [ span [ onClick onToggleTreeMenu ] [ A.icon0 ("button-light is-link has-text-weight-bold " ++ action2icon { doc_type = NODE g.focus.type_ }) ] ]
                    ++ [ g.path
                            |> List.indexedMap
                                (\i p ->
                                    if i < (List.length g.path - 1) then
                                        li []
                                            [ a [ href (uriFromNameid baseUri p.nameid [ getSourceTid p ] ++ q) ]
                                                [ div [] [ text p.name ] ]
                                            ]

                                    else
                                        li []
                                            [ a [ class "has-text-weight-semibold", href (uriFromNameid baseUri p.nameid [ getSourceTid p ] ++ q) ] [ text p.name ]
                                            , a
                                                [ class "stealth-link tag is-rounded ml-1 has-border"
                                                , attribute "style" "weight: 500 !important;padding: 10px 10px;"
                                                , href (toHref (Route.Tension_Dynamic_Dynamic_Action { param1 = nid2rootid p.nameid, param2 = getSourceTid p }))
                                                ]
                                                [ text (NodeVisibility.toString g.focus.visibility) ]
                                            ]
                                )
                            |> ul [ attribute "style" "display: inline-flex;" ]
                       ]

            Nothing ->
                [ div [ class "ph-line is-1" ] [] ]


joinButton : Html msg
joinButton =
    div [ id "join", class "button is-small has-text-weight-semibold is-primary joinTrigger" ]
        [ textH T.joinOrga ]


memberButtons : List UserRole -> Op msg -> Html msg
memberButtons roles_ op =
    let
        roles =
            case op.data of
                Expanded ->
                    roles_

                Collapsed ->
                    List.take numberRolesCollapsed roles_

        roleMoreLen =
            List.length roles_ - List.length roles

        lastButton =
            case op.data of
                Expanded ->
                    div [ class "button is-small", onClick op.onCollapse ] [ A.icon "icon-chevrons-left" ]

                Collapsed ->
                    if roleMoreLen > 0 then
                        div [ class "button has-font-weight-semibold is-small", onClick op.onExpand ]
                            [ text ("+" ++ String.fromInt roleMoreLen)
                            , A.icon "icon-chevrons-right icon-padding-left"
                            ]

                    else
                        div [] []
    in
    roles
        |> List.indexedMap
            (\i r ->
                if r.role_type == RoleType.Member then
                    [ text "" ]

                else
                    [ viewRole (uriFromNameid op.baseUri r.nameid []) r

                    --++ [ span [ class "is-vbar-1" ] [] ]
                    ]
            )
        |> List.concat
        |> List.reverse
        |> List.append [ lastButton ]
        |> List.reverse
        |> div [ class "buttons" ]
