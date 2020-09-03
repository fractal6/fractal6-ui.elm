module Components.Node exposing (nodeFragmentFromOrga, updateNodeForm, viewNodeDoc)

import Components.Fa as Fa
import Components.Loading as Loading exposing (viewGqlErrors)
import Components.Markdown exposing (renderMarkdown)
import Components.Text as T
import Dict
import Extra exposing (ternary, withMaybeData)
import Form.NewCircle exposing (getFirstLinks, nodeAboutInputView, nodeLinksInputView, nodeMandateInputView)
import Fractal.Enum.BlobType as BlobType
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Generated.Route as Route exposing (Route, toHref)
import Html exposing (Html, a, br, button, canvas, datalist, div, h1, h2, hr, i, input, label, li, nav, option, p, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, name, placeholder, required, rows, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Maybe exposing (withDefault)
import ModelCommon exposing (TensionPatchForm)
import ModelCommon.Uri exposing (FractalBaseRoute(..), NodeFocus, uriFromUsername)
import ModelCommon.View exposing (actionNameStr, getAvatar, getNodeTextFromNodeType)
import ModelSchema exposing (..)
import Query.PatchTension exposing (PatchTensionPayloadID)
import Time


type alias EditMsgs msg1 msg2 msg3 msg4 msg5 =
    { editBlob : BlobType.BlobType -> msg1
    , changeNode : String -> String -> msg2
    , cancelBlob : msg3
    , submit : (Time.Posix -> msg4) -> msg5
    , submitBlob : TensionPatchForm -> Bool -> Time.Posix -> msg4
    , isBlobEdit : Bool
    , form : TensionPatchForm
    , result : GqlData PatchTensionPayloadID
    }


type alias OrgaNodeData =
    { data : GqlData String -- payload
    , node : NodeFragment
    , isLazy : Bool
    , source : FractalBaseRoute
    , focus : NodeFocus
    }


{-| doEditView : Maybe (EditMsgs msg1 msg2) -> BlobType.BlobType -> Html msg
-}
doEditView edit btype =
    case edit of
        Just doEdit ->
            if doEdit.isBlobEdit && Just btype == doEdit.form.blob_type then
                span [] []

            else
                span
                    [ class "button has-text-weight-normal is-pulled-right is-small"
                    , onClick (doEdit.editBlob btype)
                    ]
                    [ Fa.icon0 "fas fa-pen" "" ]

        Nothing ->
            span [] []


{-| viewNodeDoc :OrgaNodeData data -> Maybe (EditMsgs msg1 msg2) -> Html msg
-}
viewNodeDoc data edit hasBeenPushed =
    div [ id "DocContainer", class "hero is-small is-light" ]
        [ div [ class "hero-body" ]
            [ case data.data of
                Failure err ->
                    if data.node.role_type == Just RoleType.Guest then
                        let
                            fs =
                                data.node.first_link |> withDefault "[Unknown]"
                        in
                        div [] [ [ "No mandate for Guest ", fs, "." ] |> String.join "" |> text ]

                    else
                        viewGqlErrors err

                LoadingSlowly ->
                    div [ class "spinner" ] []

                Success tid ->
                    viewNodeDoc_ tid data edit hasBeenPushed

                other ->
                    div [] []
            ]
        ]


viewNodeDoc_ tid data edit hasBeenPushed =
    let
        node =
            data.node

        txt =
            getNodeTextFromNodeType (node.type_ |> withDefault NodeType.Role)

        --
        blobTypeEdit =
            edit
                |> Maybe.map (\e -> ternary e.isBlobEdit e.form.blob_type Nothing)
                |> withDefault Nothing

        isLinksHidden =
            if node.type_ == Just NodeType.Circle && data.source == TensionBaseUri then
                hasBeenPushed

            else
                False

        isLoading =
            edit
                |> Maybe.map (\e -> e.result == LoadingSlowly)
                |> withDefault False
    in
    div [ classList [ ( "is-lazy", data.isLazy ) ] ]
        [ if blobTypeEdit == Just BlobType.OnAbout then
            edit
                |> Maybe.map
                    (\e ->
                        let
                            isSendable =
                                node.name /= e.form.node.name || node.about /= e.form.node.about
                        in
                        div []
                            [ nodeAboutInputView e.form.node e.changeNode txt.name_help txt.about_help
                            , blobButtonsView e isSendable isLoading
                            ]
                    )
                |> withDefault (div [] [])

          else
            div [ class "aboutDoc" ]
                [ div [ class "columns is-variable is-mobile" ]
                    [ div [ class "column is-9 subtitle is-5" ]
                        [ span []
                            [ span [ class "fa-stack", attribute "style" "font-size: 0.6em;" ]
                                [ i [ class "fas fa-info fa-stack-1x" ] []
                                , i [ class "far fa-circle fa-stack-2x" ] []
                                ]
                            , span [ class "nodeName" ] [ text "\u{00A0}", text " ", text (node.name |> withDefault "") ]
                            ]
                        ]
                    , case data.source of
                        OverviewBaseUri ->
                            div [ class "column is-3 buttonBar" ]
                                [ span
                                    [ class "is-pulled-right field has-addons docButtons" ]
                                    [ a
                                        [ class "control"
                                        , href (Route.Tension_Dynamic_Dynamic_Action { param1 = data.focus.rootnameid, param2 = tid } |> toHref)
                                        ]
                                        [ div [ class "button is-small is-rounded" ] [ Fa.icon0 "fas fa-eye" "" ] ]
                                    , a
                                        [ class "control"
                                        , href (Route.Tension_Dynamic_Dynamic_Action { param1 = data.focus.rootnameid, param2 = tid } |> toHref)
                                        , (Route.Tension_Dynamic_Dynamic_Action { param1 = data.focus.rootnameid, param2 = tid } |> toHref) ++ "?v=edit" |> href
                                        ]
                                        [ div [ class "button is-small" ] [ Fa.icon0 "fas fa-pen" "" ] ]
                                    , a
                                        [ class "control"
                                        , (Route.Tension_Dynamic_Dynamic_Action { param1 = data.focus.rootnameid, param2 = tid } |> toHref) ++ "?v=history" |> href
                                        ]
                                        [ div [ class "button is-small is-rounded" ] [ Fa.icon0 "fas fa-history" "" ] ]
                                    ]
                                ]

                        _ ->
                            div [ class "column buttonEdit" ] [ doEditView edit BlobType.OnAbout ]
                    ]
                , case node.about of
                    Just ab ->
                        p [ class "column is-fullwidth" ] [ text ab ]

                    Nothing ->
                        div [] []
                ]
        , hr [ class "has-background-grey-light" ] []
        , if blobTypeEdit == Just BlobType.OnFirstLink then
            edit
                |> Maybe.map
                    (\e ->
                        let
                            isSendable =
                                node.first_link /= e.form.node.first_link || node.role_type /= e.form.node.role_type
                        in
                        div []
                            [ nodeLinksInputView e.form.node e.changeNode txt.firstLink_help
                            , blobButtonsView e isSendable isLoading
                            ]
                    )
                |> withDefault (div [] [])

          else if isLinksHidden then
            div [] []

          else
            let
                links =
                    getFirstLinks node
            in
            div [ class "linksDoc" ]
                [ div [ class "subtitle is-5" ]
                    [ Fa.icon "fas fa-users fa-sm" T.linksH
                    , links
                        |> List.map
                            (\l ->
                                span [] [ a [ class "image circleBaseInline circle0", href (uriFromUsername UsersBaseUri l) ] [ getAvatar l ] ]
                            )
                        |> span [ attribute "style" "margin-left:20px;" ]
                    , doEditView edit BlobType.OnFirstLink
                    ]
                , if List.length links == 0 then
                    span [ class "is-italic" ] [ text T.noFirstLinks ]

                  else
                    div [] []
                ]
        , ternary isLinksHidden (div [] []) (hr [ class "has-background-grey-light" ] [])
        , if blobTypeEdit == Just BlobType.OnMandate then
            edit
                |> Maybe.map
                    (\e ->
                        let
                            isSendable =
                                node.mandate /= e.form.node.mandate
                        in
                        div []
                            [ nodeMandateInputView e.form.node e.changeNode txt
                            , blobButtonsView e isSendable isLoading
                            ]
                    )
                |> withDefault (div [] [])

          else
            case node.mandate of
                Just mandate ->
                    div [ class "mandateDoc" ]
                        [ div [ class "subtitle is-5" ]
                            [ Fa.icon "fas fa-scroll fa-sm" T.mandateH, doEditView edit BlobType.OnMandate ]
                        , viewMandateSection T.purposeH (Just mandate.purpose)
                        , viewMandateSection T.responsabilitiesH mandate.responsabilities
                        , viewMandateSection T.domainsH mandate.domains
                        , viewMandateSection T.policiesH mandate.policies
                        ]

                Nothing ->
                    div [ class "is-italic" ]
                        [ text "No mandate for this circle."
                        , doEditView edit BlobType.OnMandate
                        ]
        ]


viewMandateSection : String -> Maybe String -> Html msg
viewMandateSection name maybePara =
    case maybePara of
        Just para ->
            div [ class "message" ]
                [ div [ class "message-header" ] [ text name ]
                , p [ class "message-body" ] [ renderMarkdown para "is-dark" ]
                ]

        Nothing ->
            div [] []


blobButtonsView edit isSendable isLoading =
    div []
        [ case edit.result of
            Failure err ->
                viewGqlErrors err

            _ ->
                div [] []
        , div [ class "field is-grouped is-grouped-right" ]
            [ div [ class "control" ]
                [ div [ class "buttons" ]
                    [ button
                        [ class "button has-text-weight-semibold is-danger"
                        , onClick edit.cancel
                        ]
                        [ text T.cancel ]
                    , button
                        [ class "button has-text-weight-semibold"
                        , classList [ ( "is-success", isSendable ), ( "is-loading", isLoading ) ]
                        , disabled (not isSendable)
                        , onClick (edit.submit <| edit.submitBlob edit.form False)
                        ]
                        [ text T.saveChanges ]
                    ]
                ]
            ]
        ]


nodeFragmentFromOrga : Maybe Node -> GqlData NodeData -> List NodeId -> NodesData -> NodeFragment
nodeFragmentFromOrga node_m nodeData c ndata =
    let
        children =
            node_m
                |> Maybe.map
                    (\node ->
                        case node.type_ of
                            NodeType.Circle ->
                                c
                                    |> List.map (\n -> Dict.get n.nameid ndata)
                                    |> List.filterMap identity
                                    |> List.filter (\n -> n.role_type == Just RoleType.Coordinator)
                                    |> List.map
                                        (\n ->
                                            { name = Just n.name
                                            , nameid = Just n.nameid
                                            , type_ = Just n.type_
                                            , isPrivate = Just n.isPrivate
                                            , charac = Just n.charac
                                            , role_type = n.role_type
                                            , about = Nothing
                                            , mandate = Nothing
                                            , first_link = n.first_link |> Maybe.map (\u -> u.username)
                                            }
                                        )
                                    |> Just

                            --|> List.map (\n -> n.first_link)
                            --|> List.filterMap identity
                            NodeType.Role ->
                                Nothing
                    )
    in
    { name = Maybe.map (\n -> n.name) node_m
    , nameid = Maybe.map (\n -> n.nameid) node_m
    , type_ = Maybe.map (\n -> n.type_) node_m
    , isPrivate = Maybe.map (\n -> n.isPrivate) node_m
    , charac = Maybe.map (\n -> n.charac) node_m
    , role_type = Maybe.map (\n -> n.role_type) node_m |> withDefault Nothing
    , about = Maybe.map (\n -> n.about) (withMaybeData nodeData) |> withDefault Nothing
    , mandate = Maybe.map (\n -> n.mandate) (withMaybeData nodeData) |> withDefault Nothing
    , first_link = Maybe.map (\n -> n.first_link |> Maybe.map (\u -> u.username)) node_m |> withDefault Nothing
    , children = children |> withDefault Nothing
    }


{-| updateNodeForm : String -> String -> TensionForm/Patch -> TensionForm/Patch
-}
updateNodeForm field value form =
    let
        node =
            form.node

        mandate =
            node.mandate |> withDefault initMandate
    in
    case field of
        -- Node data
        "role_type" ->
            { form | node = { node | role_type = value |> RoleType.fromString |> withDefault RoleType.Peer |> Just } }

        "nameid" ->
            { form | node = { node | nameid = Just value } }

        "first_link" ->
            -- @DEBUG: Multiple invitations not implemented
            { form | node = { node | first_link = Just value, children = Nothing } }

        "about" ->
            { form | node = { node | about = Just value } }

        -- Mandate data
        "purpose" ->
            { form | node = { node | mandate = Just { mandate | purpose = value } } }

        "responsabilities" ->
            { form | node = { node | mandate = Just { mandate | responsabilities = Just value } } }

        "domains" ->
            { form | node = { node | mandate = Just { mandate | domains = Just value } } }

        "policies" ->
            { form | node = { node | mandate = Just { mandate | policies = Just value } } }

        -- Various
        "name" ->
            case form.action of
                Nothing ->
                    { form | node = { node | name = Just value } }

                Just action ->
                    if List.member action [ TensionAction.NewRole, TensionAction.NewCircle ] then
                        let
                            newPost =
                                Dict.insert "title" ("[" ++ actionNameStr action ++ "] " ++ value) form.post

                            newData =
                                { node
                                    | name = Just value
                                    , nameid = makeNewNodeId value
                                }
                        in
                        { form | post = newPost, node = newData }

                    else
                        { form | node = { node | name = Just value } }

        other ->
            -- title, message...
            { form | post = Dict.insert field value form.post }


makeNewNodeId : String -> Maybe String
makeNewNodeId name =
    name
        |> String.toLower
        |> String.trim
        |> String.map
            (\c ->
                if List.member c [ ' ', '/', '=', '?', '#', '&', '?', '|', '%', '$', '\\' ] then
                    '-'

                else if List.member c [ '@', '(', ')', '<', '>', '[', ']', '{', '}', '"', '`', '\'' ] then
                    '_'

                else
                    c
            )
        |> Just
