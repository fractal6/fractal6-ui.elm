{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2024 Fractale Co

   This file is part of Fractale.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU Affero General Public License as
   published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Affero General Public License for more details.

   You should have received a copy of the GNU Affero General Public License
   along with Fractale.  If not, see <http://www.gnu.org/licenses/>.
-}


module Bulk.View exposing (..)

import Assets as A
import Bulk exposing (UserState(..), getParentFragmentFromRole)
import Bulk.Codecs exposing (ActionType(..), DocType(..), FractalBaseRoute(..), NodeFocus, TensionCharac, eor2ur, getOrgaRoles, getTensionCharac, nid2rootid, nid2type, toLink)
import Extra exposing (colorAttr, ternary, upH)
import Extra.Date exposing (formatDate)
import Extra.Events exposing (onClickPos)
import Fractal.Enum.BlobType as BlobType
import Fractal.Enum.Lang as Lang
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.NodeVisibility as NodeVisibility
import Fractal.Enum.ProjectStatus as ProjectStatus
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Generated.Route as Route exposing (toHref)
import Html exposing (Html, a, br, div, hr, span, text)
import Html.Attributes exposing (attribute, class, classList, href, id, style, target, title)
import Html.Events exposing (onClick)
import Identicon
import List.Extra as LE
import Markdown exposing (renderMarkdown)
import Maybe exposing (withDefault)
import ModelSchema exposing (EmitterOrReceiver, Label, Node, NodeExt, PinTension, RoleExtCommon, Tension, TensionLight, User, UserCommon, UserRoleCommon, UserView, Username)
import Session exposing (CommonMsg, Session)
import String.Extra as SE
import String.Format as Format
import Text as T



{-
   @DEBUG: Factor this file to appropriate places in Component.{Tension, Node, User ,Color...}
-}
--
-- Tension
--


mediaTension : CommonMsg msg -> Session -> String -> Tension -> Bool -> Bool -> String -> Html msg
mediaTension commonOp session focusid tension showStatus showRecip size =
    let
        n_comments =
            withDefault 0 tension.n_comments

        rootnameid =
            nid2rootid tension.receiver.nameid
    in
    div
        [ class ("media mediaBox is-hoverable " ++ size) ]
        [ div [ class "media-left mr-3" ]
            [ div
                [ class "tooltip is-left has-tooltip-arrow"
                , attribute "data-tooltip" (tensionType2str tension.type_)
                , style "width" "10px"
                ]
                [ tensionIcon tension.type_ ]
            ]
        , div [ class "media-content" ]
            [ div [ class "content mb-1" ]
                [ a
                    [ class ("has-text-weight-semibold is-human discrete-link " ++ size)
                    , href (Route.Tension_Dynamic_Dynamic { param1 = rootnameid, param2 = tension.id } |> toHref)
                    ]
                    [ text tension.title ]
                , case tension.labels of
                    Just labels ->
                        viewLabels (Just focusid) labels

                    Nothing ->
                        text ""
                ]
            , span [ class "level is-smaller2 is-mobile" ]
                [ div [ class "level-left" ]
                    [ if showStatus then
                        span
                            [ class "tooltip has-tooltip-arrow has-tooltip-right"
                            , attribute "data-tooltip" (tensionStatus2str tension.status)
                            ]
                            [ A.icon ("icon-alert-circle icon-sm marginTensionStatus has-text-" ++ statusColor tension.status) ]

                      else
                        text ""
                    , if showRecip then
                        viewTensionDateAndUser session "has-text-weight-light" tension.createdAt tension.createdBy

                      else
                        span [ class "has-text-weight-light" ] [ text (T.authoredBy ++ " "), viewUsernameLink tension.createdBy.username ]
                    ]
                , div [ class "level-right" ] []
                ]
            ]
        , div [ class "media-right wrapped-container-33" ]
            [ ternary showRecip (viewCircleTarget commonOp "is-small" tension.receiver) (text "")
            , br [] []
            , span [ class "level is-mobile icons-list" ]
                [ case tension.action of
                    Just action ->
                        let
                            tc =
                                getTensionCharac action
                        in
                        a
                            [ class "level-item discrete-link tooltip has-tooltip-arrow"
                            , classList [ ( "has-text-warning", tc.action_type == ARCHIVE ) ]
                            , attribute "data-tooltip" ("1 " ++ action2str action ++ " " ++ T.attached)
                            , href (Route.Tension_Dynamic_Dynamic_Action { param1 = rootnameid, param2 = tension.id } |> toHref)
                            ]
                            [ A.icon0 (action2icon tc ++ " icon-sm") ]

                    Nothing ->
                        div [ class "level-item" ] []
                , if n_comments > 1 then
                    a
                        [ class "level-right is-pulled-right discrete-link tooltip has-tooltip-arrow"
                        , attribute "data-tooltip" (String.fromInt (n_comments - 1) ++ " comments")
                        , href (Route.Tension_Dynamic_Dynamic { param1 = rootnameid, param2 = tension.id } |> toHref)
                        ]
                        [ A.icon0 "icon-message-square icon-sm", text (String.fromInt (n_comments - 1)) ]

                  else
                    text ""
                ]
            ]
        ]


viewTensionLight : TensionLight -> Html msg
viewTensionLight t =
    span []
        [ span [ class "mr-2" ] [ tensionIcon t.type_ ]
        , a
            [ class "is-human discrete-link mr-2"
            , href (Route.Tension_Dynamic_Dynamic { param1 = "", param2 = t.id } |> toHref)
            , target "_blank"
            ]
            [ text t.title ]
        , case t.labels of
            Just labels ->
                viewLabels Nothing labels

            Nothing ->
                text ""
        ]


viewCircleTarget : CommonMsg msg -> String -> EmitterOrReceiver -> Html msg
viewCircleTarget commonOp cls er =
    case nid2type er.nameid of
        NodeType.Circle ->
            span [ class ("tag has-border-light tag-circle is-rounded is-wrapped " ++ cls) ] [ viewNodeRef False OverviewBaseUri er ]

        NodeType.Role ->
            viewRole ("is-tiny is-wrapped " ++ cls) False False Nothing (Just <| toLink OverviewBaseUri er.nameid []) (\_ _ _ -> commonOp.noMsg) (eor2ur er)


viewCircleSimple : String -> Html msg
viewCircleSimple nameid =
    span [ class "tag has-border-light is-rounded is-wrapped" ] [ String.split "#" nameid |> LE.last |> withDefault "" |> text ]


viewTensionArrow : Bool -> String -> EmitterOrReceiver -> EmitterOrReceiver -> Html msg
viewTensionArrow t_blank cls emitter receiver =
    span [ class cls ]
        [ span [ class "is-small is-light is-inverted is-static has-text-weight-light" ]
            [ viewNodeRef t_blank OverviewBaseUri emitter ]
        , span [ class "arrow-right" ] []
        , span [ class "is-small is-light is-inverted is-static" ]
            [ viewNodeRef t_blank OverviewBaseUri receiver ]
        ]


viewPinnedTensions : Int -> Session -> NodeFocus -> List PinTension -> Html msg
viewPinnedTensions size session focus pins =
    List.foldl
        (\a b ->
            let
                pad =
                    pins |> List.drop (a * size) |> List.take size |> List.map Just
            in
            b ++ [ pad ++ List.repeat (size - List.length pad) Nothing ]
        )
        []
        (List.range 0 (List.length pins - 1))
        |> LE.transpose
        |> List.map (List.filterMap identity)
        |> List.filter (\x -> x /= [])
        |> List.map
            (\x ->
                div [ class "tile is-parent is-vertical", classList [ ( "is-" ++ String.fromInt (12 // size), True ) ] ] <|
                    List.map (\y -> div [ class "tile_ is-children_" ] [ viewPin session focus y ]) x
            )
        |> div [ class "tile is-ancestor pinnedTile" ]


viewPin : Session -> NodeFocus -> PinTension -> Html msg
viewPin session focus tension =
    div [ class "box media mediaBox p-4", style "width" "100%" ]
        [ div [ class "media-left mr-3" ]
            [ div
                [ class "tooltip is-left has-tooltip-arrow"
                , attribute "data-tooltip" (tensionType2str tension.type_)
                , style "width" "10px"
                ]
                [ tensionIcon tension.type_ ]
            ]
        , div [ class "media-content" ]
            [ a
                [ class "has-text-weight-semibold is-human discrete-link "
                , href (Route.Tension_Dynamic_Dynamic { param1 = focus.rootnameid, param2 = tension.id } |> toHref)
                ]
                [ text tension.title ]
            , span [ class "level is-smaller2 is-mobile mt-2" ]
                [ div [ class "level-left" ]
                    [ span
                        [ class "tooltip has-tooltip-arrow has-tooltip-right"
                        , attribute "data-tooltip" (tensionStatus2str tension.status)
                        ]
                        [ A.icon ("icon-alert-circle icon-sm marginTensionStatus has-text-" ++ statusColor tension.status) ]
                    , viewTensionDateAndUser session "has-text-weight-light" tension.createdAt tension.createdBy
                    ]
                ]
            ]
        ]



--
-- Labels
--


{-| 1st parameter String is the current nameid
used to make labels clickable.
-}
viewLabels : Maybe String -> List Label -> Html msg
viewLabels nid_m labels =
    if List.length labels == 0 then
        text ""

    else
        let
            to_link_m name =
                Maybe.map
                    (\nid ->
                        toLink TensionsBaseUri nid [] ++ ("?l=" ++ name)
                    )
                    nid_m
        in
        span [ class "labelsList" ]
            (List.map
                (\label ->
                    viewLabel "" (to_link_m label.name) label
                )
                labels
            )


viewLabel : String -> Maybe String -> Label -> Html msg
viewLabel cls link_m label =
    let
        color =
            label.color
                |> Maybe.map (\c -> [ colorAttr c ])
                |> withDefault []

        a_or_span =
            case link_m of
                Just _ ->
                    a

                Nothing ->
                    span

        link =
            withDefault "#" link_m
    in
    a_or_span
        ([ class ("tag is-rounded " ++ cls)
         , href link
         ]
            ++ color
        )
        [ text label.name ]


tensionStatus2str : TensionStatus.TensionStatus -> String
tensionStatus2str s =
    case s of
        TensionStatus.Open ->
            T.openTension

        TensionStatus.Closed ->
            T.closedTension


tensionType2str : TensionType.TensionType -> String
tensionType2str s =
    case s of
        TensionType.Operational ->
            T.operational

        TensionType.Governance ->
            T.governance

        TensionType.Help ->
            T.help

        TensionType.Alert ->
            T.alert

        TensionType.Announcement ->
            T.announcement


tensionType2descr : TensionType.TensionType -> String
tensionType2descr s =
    case s of
        TensionType.Operational ->
            T.operationalHint

        TensionType.Governance ->
            T.governanceHint

        TensionType.Help ->
            T.helpHint

        TensionType.Alert ->
            T.alertHint

        TensionType.Announcement ->
            T.announcementHint


tensionType2notif : TensionType.TensionType -> String
tensionType2notif s =
    case s of
        TensionType.Operational ->
            T.operationalNotif

        TensionType.Governance ->
            T.governanceNotif

        TensionType.Help ->
            T.helpNotif

        TensionType.Alert ->
            T.alertNotif

        TensionType.Announcement ->
            T.announcementNotif


statusColor : TensionStatus.TensionStatus -> String
statusColor s =
    case s of
        TensionStatus.Open ->
            "success"

        TensionStatus.Closed ->
            "danger"


statusColorReverse : TensionStatus.TensionStatus -> String
statusColorReverse s =
    case s of
        TensionStatus.Open ->
            "danger"

        TensionStatus.Closed ->
            "success"


tensionTypeColor : String -> TensionType.TensionType -> String
tensionTypeColor elt tt =
    case tt of
        TensionType.Operational ->
            "has-" ++ elt ++ "-success"

        TensionType.Governance ->
            "has-" ++ elt ++ "-info"

        TensionType.Help ->
            "has-" ++ elt ++ "-warning"

        TensionType.Alert ->
            "has-" ++ elt ++ "-danger"

        TensionType.Announcement ->
            "has-" ++ elt ++ "-announce"


tensionTypeIcon : TensionType.TensionType -> String
tensionTypeIcon tt =
    case tt of
        TensionType.Operational ->
            "icon-circle1 icon-xs"

        TensionType.Governance ->
            "icon-circle1 icon-xs"

        TensionType.Help ->
            "icon-question"

        TensionType.Alert ->
            "icon-radio"

        TensionType.Announcement ->
            "icon-announce"


tensionIcon : TensionType.TensionType -> Html msg
tensionIcon type_ =
    span [ class <| String.join " " <| [ "is-inline-flex", tensionTypeColor "text" type_ ] ] [ A.icon (tensionTypeIcon type_) ]


tensionIcon2 : TensionType.TensionType -> Html msg
tensionIcon2 type_ =
    span [ class <| String.join " " <| [ "is-inline-flex", tensionTypeColor "text" type_ ] ] [ A.icon1 (tensionTypeIcon type_) (tensionType2str type_) ]


tensionIcon3 : TensionType.TensionType -> Html msg
tensionIcon3 type_ =
    span [ class "is-inline-flex" ] [ A.icon1 (tensionTypeIcon type_ ++ " " ++ tensionTypeColor "text" type_) (tensionType2str type_) ]



--
-- Users
--


viewUsernameLink : String -> Html msg
viewUsernameLink username =
    a [ href (toLink UsersBaseUri username []) ] [ text username ]


viewUsers : Bool -> List User -> Html msg
viewUsers isLinked users =
    span [ class "usersList" ] (List.map (\u -> viewUser isLinked u.username) users)


viewUser0 : String -> Html msg
viewUser0 username =
    span [ class "mr-2", title username ]
        [ a [ href (toLink UsersBaseUri username []) ]
            [ getAvatar0 username ]
        ]


viewUser : Bool -> String -> Html msg
viewUser isLinked_ username =
    let
        isLinked =
            if String.contains "@" username then
                False

            else
                isLinked_
    in
    if isLinked then
        span [ class "mr-2", title username ]
            [ a [ href (toLink UsersBaseUri username []) ]
                [ getAvatar1 username ]
            ]

    else
        span [ class "mr-2", title username ] [ getAvatar1 username ]


viewUser2 : String -> Html msg
viewUser2 username =
    span [ class "mr-2", title username ]
        [ a [ href (toLink UsersBaseUri username []) ]
            [ getAvatar2 username ]
        ]


viewUserFull : Int -> Bool -> Bool -> UserView a -> Html msg
viewUserFull size isLinked_ isBoxed user =
    let
        isLinked =
            if String.contains "@" user.username then
                False

            else
                isLinked_

        ( pad, avatar ) =
            case size of
                0 ->
                    ( "p-1", getAvatar0 )

                1 ->
                    ( "p-2", getAvatar1 )

                2 ->
                    ( "", getAvatar2 )

                3 ->
                    ( "", getAvatar3 )

                _ ->
                    ( "p-1", getAvatar1 )

        ( ob, lk ) =
            if isLinked then
                ( a, [ href (toLink UsersBaseUri user.username []) ] )

            else
                ( span, [] )
    in
    ob
        (ternary isBoxed
            ([ title user.username, class ("box is-light field " ++ pad), attribute "style" "display:inline;" ] ++ lk)
            ([] ++ lk)
        )
        [ span [ class "mr-2", attribute "style" (ternary isBoxed "position:relative;top:6px;" "") ]
            [ avatar user.username ]
        , span [ attribute "style" (ternary isBoxed "" "position:relative;top:-4px;") ] <|
            case user.name of
                Just name ->
                    [ span [ class "is-name" ] [ text name ]
                    , span [ class "is-username ml-2" ] [ text user.username ]
                    ]

                Nothing ->
                    [ span [ class "is-username" ] [ text user.username ] ]
        ]


viewOrga0 : Bool -> String -> Html msg
viewOrga0 isLinked nameid =
    let
        rid =
            nid2rootid nameid
    in
    if isLinked then
        a
            [ class "image circleBase circle1 is-orga"
            , href (toLink OverviewBaseUri rid [])
            ]
            [ getAvatarOrga rid ]

    else
        span
            [ class "image circleBase circle1 is-orga" ]
            [ getAvatarOrga rid ]


viewOrga : Bool -> String -> Html msg
viewOrga isLinked nameid =
    let
        rid =
            nid2rootid nameid
    in
    if isLinked then
        a
            [ class "image circleBase circle2 is-orga"
            , href (toLink OverviewBaseUri rid [])
            ]
            [ getAvatarOrga rid ]

    else
        span
            [ class "image circleBase circle2 is-orga" ]
            [ getAvatarOrga rid ]


viewRoleExt : CommonMsg msg -> String -> Maybe String -> RoleExtCommon a -> Html msg
viewRoleExt commonOp cls link_m r =
    viewRole cls False False Nothing link_m (\_ _ _ -> commonOp.noMsg) { nameid = "", name = r.name, color = r.color, role_type = r.role_type }


viewRole : String -> Bool -> Bool -> Maybe ( Session, String ) -> Maybe String -> (String -> String -> Maybe ( Int, Int ) -> msg) -> UserRoleCommon a -> Html msg
viewRole cls_ hasTooltip isSelf now_m link_m msg r =
    -- link and msg are mutually exclusive
    let
        cls =
            if hasTooltip then
                String.split " " cls_
                    ++ String.split " " "tooltip has-tooltip-arrow is-multiline has-tooltip-text-left"

            else
                String.split " " cls_

        since =
            case now_m of
                Just ( session, uri ) ->
                    T.sinceThe ++ " " ++ formatDate session.lang session.now uri

                Nothing ->
                    ""

        color =
            case r.color of
                Just c ->
                    [ colorAttr c ]

                Nothing ->
                    --( "is-" ++ roleColor r.role_type, [] )
                    [ colorAttr (roleColor r.role_type) ]

        a_or_span =
            case link_m of
                Just _ ->
                    a

                Nothing ->
                    span

        link =
            withDefault "#" link_m
    in
    a_or_span
        ([ class "button buttonRole is-small"
         , classList (List.map (\x -> ( x, True )) cls)
         , attribute (ternary hasTooltip "data-tooltip" "data-void")
            (ternary isSelf T.youPlay T.theyPlay
                |> Format.namedValue "role" (upH r.name)
                |> Format.namedValue "circle" (getParentFragmentFromRole r)
                |> Format.namedValue "since" since
            )
         , onClickPos (msg "actionPanelHelper" r.nameid)
         , href link
         ]
            ++ color
        )
        [ A.icon1 (role2icon r) (upH r.name) ]


viewProfileC : UserCommon a -> Html msg
viewProfileC user =
    div [ attribute "style" "max-width:333px;" ]
        [ div [ class "content" ] [ getAvatar3 user.username ]
        , div [ class "content" ]
            [ case user.name of
                Just name ->
                    div [ class "is-strong is-size-4" ] [ text name ]

                Nothing ->
                    text ""
            , div [ class "is-discrete is-size-6" ] [ text ("@" ++ user.username) ]
            ]
        , case user.bio of
            Nothing ->
                text ""

            Just "" ->
                text ""

            Just bio ->
                div [ class "content" ] [ text bio ]
        , case user.location of
            Nothing ->
                text ""

            Just "" ->
                text ""

            Just location ->
                div [ class "content" ] [ A.icon1 "icon-map-pin" location ]
        ]



--
-- Date and authors
--


viewOpenedDate : Session -> String -> Html msg
viewOpenedDate session date =
    span [] <|
        List.intersperse (text " ") <|
            [ text T.authored
            , text (formatDate session.lang session.now date)
            ]


viewUpdated : Session -> String -> Html msg
viewUpdated session date =
    span [ class "is-discrete" ] <|
        List.intersperse (text " ") <|
            [ text " ·"
            , text T.edited
            , text (formatDate session.lang session.now date)
            ]


viewCommentedDate : Session -> String -> Html msg
viewCommentedDate session date =
    span [ class "is-discrete" ] <|
        List.intersperse (text " ") <|
            [ text T.commented
            , text (formatDate session.lang session.now date)
            ]


viewTensionDateAndUser : Session -> String -> String -> Username -> Html msg
viewTensionDateAndUser session cls createdAt createdBy =
    span [ class cls ] <|
        List.intersperse (text " ") <|
            [ viewOpenedDate session createdAt
            , text T.by
            , viewUsernameLink createdBy.username
            ]


viewTensionDateAndUserC : Session -> String -> Username -> Html msg
viewTensionDateAndUserC session createdAt createdBy =
    span [] <|
        List.intersperse (text " ") <|
            [ viewUsernameLink createdBy.username
            , viewCommentedDate session createdAt
            ]


byAt : Session -> Username -> String -> Html msg
byAt session createdBy createdAt =
    span [] <|
        List.intersperse (text " ") <|
            [ text T.by
            , viewUsernameLink createdBy.username
            , text (formatDate session.lang session.now createdAt)
            ]


counter : Int -> Html msg
counter c =
    span
        [ class "tag is-normal is-rounded has-background-counter ml-2"
        , attribute "style" "height: 1.25rem;"
        ]
        [ text (String.fromInt c) ]



--
-- Node
--


viewNodeDescr : Bool -> Node -> TensionCharac -> Html msg
viewNodeDescr inPanel node tc =
    let
        cls =
            if inPanel then
                "level"

            else
                "is-flex"
    in
    div [] <|
        case tc.doc_type of
            NODE NodeType.Circle ->
                [ div [ class "is-mobile mb-3", classList [ ( cls, True ) ] ] <|
                    [ span [ class "level-left mr-4" ]
                        [ span [ class "tag tagHint" ] [ A.icon1 (action2icon tc) (SE.humanize (action2str tc.action)) ] ]
                    , span [ class "level-item mr-4" ]
                        [ span [ class "tag tagHint" ] [ A.icon1 (auth2icon tc) (auth2val node tc) ] ]
                    , span [ class "level-right" ]
                        [ span [ class "tag tagHint" ] [ A.icon1 (visibility2icon node.visibility) (NodeVisibility.toString node.visibility) ] ]
                    ]
                ]

            NODE NodeType.Role ->
                [ div [ class "is-mobile mb-3 is-flex" ] <|
                    [ span [ class "level-left mr-4" ]
                        [ span [ class "tag tagHint" ] [ A.icon1 (action2icon tc) (SE.humanize (action2str tc.action)) ] ]
                    , span [ class "level-item" ]
                        [ span [ class "tag tagHint" ] [ A.icon1 (auth2icon tc) (auth2val node tc) ] ]
                    ]
                ]

            MD ->
                [ div [ class "help is-italic" ] [ text T.notImplemented ] ]


viewNodeRef : Bool -> FractalBaseRoute -> EmitterOrReceiver -> Html msg
viewNodeRef t_blank baseUri n =
    let
        ref =
            if n.role_type == Just RoleType.Member then
                "#"

            else
                toLink baseUri n.nameid []

        more =
            if t_blank then
                [ target "_blank" ]

            else
                []
    in
    a ([ href ref, class "is-wrapped" ] ++ more) [ text n.name ]


viewNodeRefShort : FractalBaseRoute -> String -> Html msg
viewNodeRefShort baseUri nid =
    let
        ref =
            toLink baseUri nid []

        name =
            nid |> String.split "#" |> LE.last |> withDefault nid
    in
    a [ href ref ] [ text name ]


mediaOrga : CommonMsg msg -> Maybe (UserCommon a) -> NodeExt -> Html msg
mediaOrga commonOp user_m root =
    div [ class "media mediaBox box pb-3" ]
        [ div [ class "media-left" ] [ viewOrga True root.nameid ]
        , div [ class "media-content" ]
            ([ div [ class "columns" ]
                [ div [ class "column is-8" ]
                    [ a [ href (toLink OverviewBaseUri root.nameid []) ] [ text root.name ]
                    , case root.about of
                        Just ab ->
                            renderMarkdown "is-human pt-1" ab

                        Nothing ->
                            text ""
                    ]
                , span [ class "column is-4" ]
                    [ div [ class "field is-grouped is-grouped-multiline is-pulled-right" ]
                        [ div [ class "control" ]
                            [ div [ class "tags has-addons" ]
                                [ span [ class "tag" ] [ A.icon1 "icon-users" (String.fromInt root.n_members) ]
                                , span [ class "tag" ] [ A.icon1 "icon-eye" (String.fromInt root.n_watchers) ]
                                ]
                            ]
                        ]
                    ]
                ]
             , div [ id "icons", class "level is-mobile" ]
                [ div [ class "level-left" ]
                    [ if root.visibility == NodeVisibility.Private then
                        span [ class "level-item" ] [ A.icon "icon-lock" ]

                      else
                        text ""
                    ]
                ]
             ]
                ++ (case user_m of
                        Just user ->
                            let
                                roles =
                                    getOrgaRoles [ root.nameid ] user.roles |> List.filter (\r -> r.role_type /= RoleType.Member)
                            in
                            [ ternary (List.length roles > 0) (hr [ class "has-background-border-light mb-3" ] []) (text "")
                            , div [ class "buttons is-inline" ] <|
                                (roles
                                    |> List.map
                                        (\r ->
                                            if r.role_type == RoleType.Guest then
                                                viewRole "" True False Nothing (Just <| toLink MembersBaseUri r.nameid []) (\_ _ _ -> commonOp.noMsg) r

                                            else
                                                viewRole "" True False Nothing (Just <| toLink OverviewBaseUri r.nameid []) (\_ _ _ -> commonOp.noMsg) r
                                        )
                                )
                            ]

                        Nothing ->
                            []
                   )
            )
        ]


roleColor : RoleType.RoleType -> String
roleColor rt =
    case rt of
        RoleType.Owner ->
            "orange"

        RoleType.Member ->
            "primary"

        RoleType.Coordinator ->
            "orange"

        RoleType.Peer ->
            "primary"

        RoleType.Bot ->
            "white-dimmed"

        RoleType.Guest ->
            "blue-grey"

        RoleType.Pending ->
            "turquoise"

        RoleType.Retired ->
            "purple"


colorFromRole : { r | role_type : RoleType.RoleType } -> String
colorFromRole r =
    case r.role_type of
        RoleType.Owner ->
            "var(--orange-frac6)"

        RoleType.Member ->
            "var(--primary)"

        RoleType.Coordinator ->
            "var(--orange-frac6)"

        RoleType.Peer ->
            "var(--primary)"

        RoleType.Bot ->
            "var(--white-dimmed)"

        RoleType.Guest ->
            "var(--primary)"

        RoleType.Pending ->
            "var(--warning)"

        RoleType.Retired ->
            "var(--warning)"


role2icon : { r | role_type : RoleType.RoleType } -> String
role2icon r =
    case r.role_type of
        RoleType.Coordinator ->
            "icon-king"

        RoleType.Owner ->
            "icon-queen"

        _ ->
            "ml--1"


nodeType2str : NodeType.NodeType -> String
nodeType2str x =
    case x of
        NodeType.Circle ->
            T.circle

        NodeType.Role ->
            T.role


projectStatus2str : ProjectStatus.ProjectStatus -> String
projectStatus2str s =
    case s of
        ProjectStatus.Open ->
            T.openProject

        ProjectStatus.Closed ->
            T.closedProject


lang2str : Lang.Lang -> String
lang2str x =
    case x of
        Lang.En ->
            "English"

        Lang.Fr ->
            "Français"



--
-- Avatar
--


getAvatar0 : String -> Html msg
getAvatar0 username =
    span [ class "image circleBaseInline circle00" ] [ Identicon.identicon "14px" username ]


getAvatar1 : String -> Html msg
getAvatar1 username =
    span [ class "image circleBaseInline circle0" ] [ Identicon.identicon "18px" username ]


getAvatar2 : String -> Html msg
getAvatar2 username =
    span [ class "image circleBaseInline circle1" ] [ Identicon.identicon "22px" username ]


getAvatar3 : String -> Html msg
getAvatar3 username =
    span [ class "image circleBaseInline circle3" ] [ Identicon.identicon "108px" username ]


getAvatarOrga : String -> Html msg
getAvatarOrga name =
    let
        initial =
            name
                |> String.slice 0 1
                |> String.toUpper
    in
    span []
        [ text initial
        , name
            |> String.slice 1 3
            |> String.toLower
            |> text
        ]



--
-- Action
--


visibility2icon : NodeVisibility.NodeVisibility -> String
visibility2icon visibility =
    case visibility of
        NodeVisibility.Public ->
            "icon-globe"

        NodeVisibility.Private ->
            "icon-lock"

        NodeVisibility.Secret ->
            "icon-key"


visibility2descr : NodeVisibility.NodeVisibility -> String
visibility2descr visibility =
    case visibility of
        NodeVisibility.Public ->
            T.visibilityPublic

        NodeVisibility.Private ->
            T.visibilityPrivate

        NodeVisibility.Secret ->
            T.visibilitySeccret


visibility2extra : NodeVisibility.NodeVisibility -> String
visibility2extra visibility =
    case visibility of
        NodeVisibility.Public ->
            T.visibilityPublicExtra

        NodeVisibility.Private ->
            T.visibilityPrivateExtra

        NodeVisibility.Secret ->
            T.notImplemented


auth2str : TensionCharac -> String
auth2str tc =
    case tc.doc_type of
        NODE nt ->
            case nt of
                NodeType.Circle ->
                    T.governance

                NodeType.Role ->
                    T.authority

        MD ->
            T.notImplemented


action2str : TensionAction.TensionAction -> String
action2str action =
    case action of
        TensionAction.NewCircle ->
            T.circle

        TensionAction.EditCircle ->
            T.circle

        TensionAction.ArchivedCircle ->
            T.circle

        TensionAction.NewRole ->
            T.role

        TensionAction.EditRole ->
            T.role

        TensionAction.ArchivedRole ->
            T.role

        TensionAction.NewMd ->
            T.document

        TensionAction.EditMd ->
            T.document

        TensionAction.ArchivedMd ->
            T.document


auth2val : Node -> TensionCharac -> String
auth2val node tc =
    case tc.doc_type of
        NODE nt ->
            case nt of
                NodeType.Circle ->
                    NodeMode.toString node.mode

                NodeType.Role ->
                    Maybe.map RoleType.toString node.role_type |> withDefault T.unknown

        MD ->
            T.notImplemented


auth2icon : TensionCharac -> String
auth2icon tc =
    case tc.doc_type of
        NODE nt ->
            case nt of
                NodeType.Circle ->
                    "icon-shield"

                NodeType.Role ->
                    "icon-key"

        MD ->
            T.notImplemented


action2icon : { x | doc_type : DocType } -> String
action2icon x =
    case x.doc_type of
        NODE nt ->
            case nt of
                NodeType.Circle ->
                    "icon-git-branch"

                NodeType.Role ->
                    "icon-leaf"

        MD ->
            "icon-markdown"


archiveActionToggle : Maybe TensionAction.TensionAction -> Maybe TensionAction.TensionAction
archiveActionToggle action_m =
    action_m
        |> Maybe.map
            (\action ->
                case action of
                    TensionAction.EditCircle ->
                        Just TensionAction.ArchivedCircle

                    TensionAction.ArchivedCircle ->
                        Just TensionAction.EditCircle

                    TensionAction.EditRole ->
                        Just TensionAction.ArchivedRole

                    TensionAction.ArchivedRole ->
                        Just TensionAction.EditRole

                    TensionAction.EditMd ->
                        Just TensionAction.ArchivedMd

                    TensionAction.ArchivedMd ->
                        Just TensionAction.EditMd

                    TensionAction.NewCircle ->
                        Nothing

                    TensionAction.NewRole ->
                        Nothing

                    TensionAction.NewMd ->
                        Nothing
            )
        |> withDefault Nothing



--
-- Blob
--


blobTypeStr : BlobType.BlobType -> String
blobTypeStr btype =
    case btype of
        BlobType.OnNode ->
            T.onNode_blob

        BlobType.OnAbout ->
            T.onAbout_blob

        BlobType.OnMandate ->
            T.onMandate_blob

        BlobType.OnAboutAndMandate ->
            T.onAboutAndMandate_blob

        BlobType.OnDoc ->
            T.onDoc_blob



--
-- Utils
--


helperButton : String -> String -> Html msg
helperButton cls content =
    span [ class ("is-helper dropdown " ++ cls) ]
        [ div [ class "dropdown-trigger" ] [ A.icon "icon-help-circle is-sm button-light is-h" ]
        , div [ class "dropdown-menu", attribute "style" "menu" ]
            [ div [ class "dropdown-content" ]
                [ div [ class "dropdown-item" ]
                    [ renderMarkdown "" content ]
                ]
            ]
        ]


viewGoRoot : String -> msg -> Html msg
viewGoRoot cls cmd =
    let
        combinedClasses =
            "help-label button-light is-goroot " ++ cls
    in
    span [ class combinedClasses, onClick cmd ]
        [ A.icon "arrow-up", text T.goRoot ]
