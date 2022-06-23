module ModelCommon.View exposing (..)

import Assets as A
import Dict exposing (Dict)
import Extra exposing (colorAttr, ternary)
import Extra.Date exposing (formatDate)
import Fractal.Enum.BlobType as BlobType
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.NodeVisibility as NodeVisibility
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Generated.Route as Route exposing (toHref)
import Global
import Html exposing (Html, a, br, button, div, hr, i, p, span, sub, text)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id)
import Html.Events exposing (onClick)
import Html.Lazy as Lazy
import Identicon
import Maybe exposing (withDefault)
import ModelCommon exposing (UserState(..), getParentFragmentFromRole)
import ModelCommon.Codecs
    exposing
        ( ActionType(..)
        , DocType(..)
        , FractalBaseRoute(..)
        , NodeFocus
        , TensionCharac
        , getOrgaRoles
        , getTensionCharac
        , nid2rootid
        , uriFromNameid
        , uriFromUsername
        )
import ModelSchema
    exposing
        ( EmitterOrReceiver
        , Label
        , Node
        , NodeExt
        , PNode
        , Post
        , RoleExt
        , RoleExtFull
        , Tension
        , User
        , UserCommon
        , UserRole
        , UserRoleExtended
        , Username
        , shrinkNode
        )
import Text as T exposing (textH, textT, upH)
import Time



{-
   @DEBUG: Factor this file to appropriate places in Component.{Tension, Node, User ,Color...}
-}
{-
   Tension
-}


statusColor : TensionStatus.TensionStatus -> String
statusColor s =
    case s of
        TensionStatus.Open ->
            "success"

        TensionStatus.Closed ->
            "danger"


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


tensionIcon : TensionType.TensionType -> Html msg
tensionIcon type_ =
    let
        cls =
            ""
    in
    span [ class <| String.join " " <| [ cls, tensionTypeColor "text" type_ ] ] [ A.icon (tensionTypeIcon type_) ]


tensionIcon2 : TensionType.TensionType -> Html msg
tensionIcon2 type_ =
    let
        cls =
            ""
    in
    span [ class <| String.join " " <| [ cls, tensionTypeColor "text" type_ ] ] [ A.icon1 (tensionTypeIcon type_) (TensionType.toString type_) ]


mediaTension : Time.Posix -> NodeFocus -> Tension -> Bool -> Bool -> String -> (String -> msg) -> Html msg
mediaTension now focus tension showStatus showRecip size navigate =
    Lazy.lazy7 mediaTension_ now focus tension showStatus showRecip size navigate


mediaTension_ : Time.Posix -> NodeFocus -> Tension -> Bool -> Bool -> String -> (String -> msg) -> Html msg
mediaTension_ now focus tension showStatus showRecip size navigate =
    let
        n_comments =
            --tension.comments_agg |> Maybe.map (\x -> withDefault 0 x.count) |> withDefault 0
            withDefault 0 tension.n_comments

        labels_m =
            tension.labels |> Maybe.map (\ls -> ternary (List.length ls == 0) Nothing (Just ls)) |> withDefault Nothing
    in
    div
        [ class ("media mediaBox is-hoverable " ++ size)

        --, onClick (Route.Tension_Dynamic_Dynamic { param1 = focus.rootnameid, param2 = tension.id } |> toHref |> navigate)
        --, href (Route.Tension_Dynamic_Dynamic { param1 = focus.rootnameid, param2 = tension.id } |> toHref)
        ]
        [ div [ class "media-left mr-3" ]
            [ div
                [ class "tooltip has-tooltip-arrow"
                , attribute "data-tooltip" (TensionType.toString tension.type_)
                ]
                [ tensionIcon tension.type_ ]
            ]
        , div [ class "media-content" ]
            [ div [ class "content mb-1" ]
                [ a
                    [ class ("has-text-weight-semibold is-human discrete-link " ++ size)
                    , href (Route.Tension_Dynamic_Dynamic { param1 = focus.rootnameid, param2 = tension.id } |> toHref)
                    ]
                    [ text tension.title ]
                , case labels_m of
                    Just labels ->
                        viewLabels labels

                    Nothing ->
                        text ""
                ]
            , span [ class "level is-smaller2 is-mobile" ]
                [ div [ class "level-left" ]
                    [ if showStatus then
                        span
                            [ class "tooltip has-tooltip-arrow has-tooltip-right"
                            , attribute "data-tooltip" (TensionStatus.toString tension.status)
                            ]
                            [ A.icon ("icon-alert-circle icon-sm marginTensionStatus has-text-" ++ statusColor tension.status) ]

                      else
                        text ""
                    , if showRecip then
                        viewTensionDateAndUser now "has-text-weight-light" tension.createdAt tension.createdBy

                      else
                        span [ class "has-text-weight-light" ] [ text (T.by ++ " "), viewUsernameLink tension.createdBy.username ]
                    ]
                , div [ class "level-right" ]
                    []
                ]
            ]
        , div [ class "media-right" ]
            [ ternary showRecip (viewCircleTarget "is-pulled-right" tension.receiver) (text "")
            , br [] []
            , span [ class "level icons-list" ]
                [ case tension.action of
                    Just action ->
                        let
                            tc =
                                getTensionCharac action
                        in
                        a
                            [ class "level-item discrete-link tooltip has-tooltip-arrow"
                            , classList [ ( "has-text-warning", tc.action_type == ARCHIVE ) ]
                            , attribute "data-tooltip" ("1 " ++ action2str action ++ " attached")
                            , href (Route.Tension_Dynamic_Dynamic_Action { param1 = focus.rootnameid, param2 = tension.id } |> toHref)
                            ]
                            [ A.icon0 (action2icon tc ++ " icon-sm") ]

                    Nothing ->
                        text ""
                , if n_comments > 1 then
                    a
                        [ class "level-item discrete-link tooltip has-tooltip-arrow "
                        , attribute "data-tooltip" (String.fromInt (n_comments - 1) ++ " comments")
                        , href (Route.Tension_Dynamic_Dynamic { param1 = focus.rootnameid, param2 = tension.id } |> toHref)
                        ]
                        [ A.icon0 "icon-message-square icon-sm", text (String.fromInt (n_comments - 1)) ]

                  else
                    text ""
                ]
            ]
        ]


viewJoinNeeded : NodeFocus -> Html msg
viewJoinNeeded focus =
    div [ class "box has-background-primary has-text-light" ]
        [ p []
            [ button [ class "button is-small joinTrigger" ] [ text "Join" ]
            , text " this organisation to participate to this conversation."
            ]
        ]


viewCircleTarget : String -> EmitterOrReceiver -> Html msg
viewCircleTarget cls er =
    span [ class ("tag has-border-light tag-circl is-rounded " ++ cls) ] [ viewNodeRef OverviewBaseUri er ]


viewTensionArrow : String -> EmitterOrReceiver -> EmitterOrReceiver -> Html msg
viewTensionArrow cls emitter receiver =
    span [ class cls ]
        [ span [ class "is-small is-light is-inverted is-static has-text-weight-light" ]
            [ viewNodeRef OverviewBaseUri emitter ]
        , span [ class "arrow-right" ] []
        , span [ class "is-small is-light is-inverted is-static" ]
            [ viewNodeRef OverviewBaseUri receiver ]
        ]


viewTensionArrowB : String -> EmitterOrReceiver -> EmitterOrReceiver -> Html msg
viewTensionArrowB cls emitter receiver =
    span [ class cls ]
        [ span [ class "is-small is-inverted is-hovered button", attribute "style" "margin-top: -3px !important;" ]
            [ viewNodeRef OverviewBaseUri emitter ]
        , span [ class "arrow-right" ] []
        , span [ class "is-small is-inverted is-hovered button", attribute "style" "margin-top: -3px !important;" ]
            [ viewNodeRef OverviewBaseUri receiver ]
        ]



{-
   Labels
-}


viewLabels : List Label -> Html msg
viewLabels labels =
    span [ class "labelsList" ] (List.map (\label -> viewLabel "" label) labels)


viewLabel : String -> Label -> Html msg
viewLabel cls label =
    let
        color =
            label.color
                |> Maybe.map (\c -> [ colorAttr c ])
                |> withDefault []
    in
    span ([ class ("tag is-rounded " ++ cls) ] ++ color) [ text label.name ]



{-
   Users
-}


viewUsernameLink : String -> Html msg
viewUsernameLink username =
    a [ href (uriFromUsername UsersBaseUri username) ] [ text username ]


viewUsers : List User -> Html msg
viewUsers users =
    span [ class "usersList" ] (List.map (\u -> viewUser True u.username) users)


viewUser0 : String -> Html msg
viewUser0 username =
    span [ class "mr-2" ]
        [ a [ href (uriFromUsername UsersBaseUri username) ]
            [ getAvatar0 username ]
        ]


viewUser : Bool -> String -> Html msg
viewUser isLinked username =
    if isLinked then
        span [ class "mr-2" ]
            [ a [ href (uriFromUsername UsersBaseUri username) ]
                [ getAvatar1 username ]
            ]

    else
        span [ class "mr-2" ] [ getAvatar1 username ]


viewUser2 : String -> Html msg
viewUser2 username =
    span [ class "mr-2" ]
        [ a [ href (uriFromUsername UsersBaseUri username) ]
            [ getAvatar2 username ]
        ]


viewUserFull : Int -> Bool -> Bool -> User -> Html msg
viewUserFull size isLinked isBoxed user =
    let
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
                ( a, [ href (uriFromUsername UsersBaseUri user.username) ] )

            else
                ( span, [] )
    in
    ob (ternary isBoxed ([ class ("box is-light field " ++ pad), attribute "style" "display:inline;" ] ++ lk) ([] ++ lk))
        [ span [ class "mr-2", attribute "style" (ternary isBoxed "position:relative;top:6px;" "") ]
            [ avatar user.username ]
        , span [ attribute "style" (ternary isBoxed "" "position:relative;top:-4px;") ]
            [ case user.name of
                Just name ->
                    span [] [ span [ class "is-name" ] [ text name ], span [ class "is-username ml-2" ] [ text user.username ] ]

                Nothing ->
                    span [ class "is-username" ] [ text user.username ]
            ]
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
            , href (uriFromNameid OverviewBaseUri rid)
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
            , href (uriFromNameid OverviewBaseUri rid)
            ]
            [ getAvatarOrga rid ]

    else
        span
            [ class "image circleBase circle2 is-orga" ]
            [ getAvatarOrga rid ]


viewMemberRole : Time.Posix -> FractalBaseRoute -> UserRoleExtended -> Html msg
viewMemberRole now baseUri r =
    a
        [ class ("button buttonRole is-small tooltip has-tooltip-arrow has-tooltip-bottom is-" ++ roleColor r.role_type)
        , attribute "data-tooltip" ([ r.name, "of", getParentFragmentFromRole r, "since the", formatDate now r.createdAt ] |> String.join " ")
        , href <| uriFromNameid baseUri r.nameid
        ]
        [ if r.role_type == RoleType.Guest then
            textH T.guest

          else if r.role_type == RoleType.Member then
            textH T.member

          else if r.role_type == RoleType.Owner then
            textH T.owner

          else
            -- Peer
            text r.name
        ]


viewRole : FractalBaseRoute -> UserRole -> Html msg
viewRole baseUri r =
    a
        [ class ("button buttonRole is-small tooltip has-tooltip-arrow has-tooltip-bottom is-" ++ roleColor r.role_type)
        , attribute "data-tooltip" (r.name ++ " of " ++ getParentFragmentFromRole r)
        , href <| uriFromNameid baseUri r.nameid
        ]
        [ if r.role_type == RoleType.Coordinator then
            span [ class "is-queen" ] []

          else if r.role_type == RoleType.Owner then
            span [ class "is-king" ] []

          else
            text ""
        , text r.name
        ]


viewRoleExt : String -> RoleExt -> Html msg
viewRoleExt cls r =
    let
        color =
            r.color
                |> Maybe.map (\c -> [ colorAttr c ])
                |> withDefault []
    in
    span
        ([ class ("button buttonRole " ++ cls) ] ++ color)
        [ if r.role_type == RoleType.Coordinator then
            span [ class "is-queen" ] []

          else if r.role_type == RoleType.Owner then
            span [ class "is-king" ] []

          else
            text ""
        , text r.name
        ]


viewRoleExt2 : String -> RoleExtFull -> Html msg
viewRoleExt2 cls r =
    let
        color =
            r.color
                |> Maybe.map (\c -> [ colorAttr c ])
                |> withDefault []
    in
    span
        ([ class ("button buttonRole " ++ cls) ] ++ color)
        [ if r.role_type == RoleType.Coordinator then
            span [ class "is-queen" ] []

          else if r.role_type == RoleType.Owner then
            span [ class "is-king" ] []

          else
            text ""
        , text r.name
        ]


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



{-
   Date and authors
-}


viewOpenedDate : Time.Posix -> String -> Html msg
viewOpenedDate now date =
    span [] <|
        List.intersperse (text " ") <|
            [ span [] [ text T.opened ]
            , text (formatDate now date)
            ]


viewUpdated : Time.Posix -> String -> Html msg
viewUpdated now date =
    span [ class "is-discrete" ] <|
        List.intersperse (text " ") <|
            [ text " ·"
            , span [] [ text T.edited ]
            , text (formatDate now date)
            ]


viewCommentedDate : Time.Posix -> String -> Html msg
viewCommentedDate now date =
    span [ class "is-discrete" ] <|
        List.intersperse (text " ") <|
            [ span [] [ text T.commented ]
            , text (formatDate now date)
            ]


viewTensionDateAndUser : Time.Posix -> String -> String -> Username -> Html msg
viewTensionDateAndUser now cls createdAt createdBy =
    span [ class cls ] <|
        List.intersperse (text " ") <|
            [ viewOpenedDate now createdAt
            , text T.by
            , viewUsernameLink createdBy.username
            ]


viewTensionDateAndUserC : Time.Posix -> String -> Username -> Html msg
viewTensionDateAndUserC now createdAt createdBy =
    span [] <|
        List.intersperse (text " ") <|
            [ viewUsernameLink createdBy.username
            , viewCommentedDate now createdAt
            ]


byAt : Time.Posix -> Username -> String -> Html msg
byAt now createdBy createdAt =
    span [] <|
        List.intersperse (text " ") <|
            [ text "by"
            , viewUsernameLink createdBy.username
            , text (formatDate now createdAt)
            ]


atBy : Time.Posix -> String -> String -> Username -> Html msg
atBy now cls createdAt createdBy =
    span [ class cls ] <|
        List.intersperse (text " ") <|
            [ text (formatDate now createdAt)
            , text "by"
            , viewUsernameLink createdBy.username
            ]



{-
   Node
-}


viewNodeRef : FractalBaseRoute -> EmitterOrReceiver -> Html msg
viewNodeRef baseUri n =
    let
        ref =
            if n.role_type == Just RoleType.Member then
                "#"

            else
                uriFromNameid baseUri n.nameid
    in
    a [ href ref ] [ n.name |> text ]


viewNodeRefShort : FractalBaseRoute -> String -> Html msg
viewNodeRefShort baseUri nid =
    let
        ref =
            uriFromNameid baseUri nid

        name =
            nid |> String.split "#" |> List.reverse |> List.head |> withDefault nid
    in
    a [ href ref ] [ name |> text ]


viewOrgaMedia : Maybe (UserCommon a) -> NodeExt -> Html msg
viewOrgaMedia user_m root =
    Lazy.lazy2 viewOrgaMedia_ user_m root


viewOrgaMedia_ : Maybe (UserCommon a) -> NodeExt -> Html msg
viewOrgaMedia_ user_m root =
    let
        n_members =
            root.orga_agg |> Maybe.map (\agg -> withDefault 0 agg.n_members) |> withDefault 0

        n_guests =
            root.orga_agg |> Maybe.map (\agg -> withDefault 0 agg.n_guests) |> withDefault 0
    in
    div [ class "media mediaBox box" ]
        [ div [ class "media-left" ] [ viewOrga True root.nameid ]
        , div [ class "media-content" ]
            ([ div [ class "columns" ]
                [ div [ class "column is-8" ]
                    [ a [ href (uriFromNameid OverviewBaseUri root.nameid) ] [ text root.name ]
                    , case root.about of
                        Just ab ->
                            p [ class "is-human pt-1" ] [ text ab ]

                        Nothing ->
                            text ""
                    ]
                , span [ class "column is-4" ]
                    [ div [ class "field is-grouped is-grouped-multiline is-pulled-right" ]
                        [ div [ class "control" ]
                            [ div [ class "tags has-addons" ]
                                [ span [ class "tag is-light" ] [ text "member" ]
                                , span [ class "tag is-white" ] [ text (String.fromInt n_members) ]
                                ]
                            ]
                        , div [ class "control" ]
                            [ div [ class "tags has-addons" ]
                                [ span [ class "tag is-light" ] [ text "guest" ]
                                , span [ class "tag is-white" ] [ text (String.fromInt n_guests) ]
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
                            [ ternary (List.length roles > 0) (hr [ class "has-background-border-light" ] []) (text "")
                            , div [ class "buttons" ] <|
                                (roles
                                    |> List.map (\r -> viewRole OverviewBaseUri r)
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
            "link"

        RoleType.Guest ->
            "primary"

        RoleType.Pending ->
            "warning"

        RoleType.Retired ->
            "warning"



{-
   Avatar
-}


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



{-
   Action
-}


auth2str : TensionCharac -> String
auth2str tc =
    case tc.doc_type of
        NODE nt ->
            case nt of
                NodeType.Circle ->
                    upH T.governance

                NodeType.Role ->
                    upH T.authority

        MD ->
            T.notImplemented


action2str : TensionAction.TensionAction -> String
action2str action =
    case action of
        TensionAction.NewCircle ->
            upH T.circle

        TensionAction.EditCircle ->
            upH T.circle

        TensionAction.ArchivedCircle ->
            upH T.circle

        TensionAction.NewRole ->
            upH T.role

        TensionAction.EditRole ->
            upH T.role

        TensionAction.ArchivedRole ->
            upH T.role

        TensionAction.NewMd ->
            upH T.document

        TensionAction.EditMd ->
            upH T.document

        TensionAction.ArchivedMd ->
            upH T.document


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



{-
   Blob
-}


blobTypeStr : BlobType.BlobType -> String
blobTypeStr btype =
    case btype of
        BlobType.OnNode ->
            "Document created"

        BlobType.OnAbout ->
            "Description edited"

        BlobType.OnMandate ->
            "Mandate edited"

        BlobType.OnDoc ->
            "File edited"


type alias FormText =
    { title : String
    , added : String
    , name_help : String
    , about_help : String
    , message_help : String
    , ph_purpose : String
    , ph_responsabilities : String
    , ph_domains : String
    , ph_policies : String
    , submit : String
    , close_submit : String
    , firstLink_help : String
    , tension_added : String
    , noFirstLinks : String
    }


emptyFormText : FormText
emptyFormText =
    FormText "" "" "" "" "" "" "" "" "" "" "" "" "" ""


getTensionText : FormText
getTensionText =
    FormText T.newTension T.tensionAdded T.tensionTitleHelp "" T.tensionMessageHelp "" "" "" "" T.tensionSubmit "" "" "" ""


getNodeTextFromNodeType : NodeType.NodeType -> FormText
getNodeTextFromNodeType type_ =
    case type_ of
        NodeType.Circle ->
            FormText T.newCircle T.circleAdded T.circleNameHelp T.circleAboutHelp T.circleMessageHelp T.phCirclePurpose T.phCircleResponsabilities T.phCircleDomains T.phCirclePolicies T.tensionSubmit T.tensionCircleCloseSubmit T.firstLinkCircleMessageHelp T.tensionCircleAdded T.noFirstLinksCircle

        NodeType.Role ->
            FormText T.newRole T.roleAdded T.roleNameHelp T.roleAboutHelp T.roleMessageHelp T.phRolePurpose T.phRoleResponsabilities T.phRoleDomains T.phRolePolicies T.tensionSubmit T.tensionRoleCloseSubmit T.firstLinkRoleMessageHelp T.tensionRoleAdded T.noFirstLinksRole
