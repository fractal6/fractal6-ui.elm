module Components.NodeDoc exposing
    ( NodeDoc
    , cancelEdit
    , create
    , edit
    , getFirstLinks
    , nodeAboutInputView
    , nodeFragmentFromOrga
    , nodeLinksInputView
    , nodeMandateInputView
    , updateForm
    , updateNodeForm
    , view
    )

import Components.Doc exposing (ActionView(..))
import Components.Fa as Fa
import Components.Loading as Loading exposing (viewGqlErrors)
import Components.Markdown exposing (renderMarkdown)
import Components.Text as T
import Dict
import Extra exposing (ternary, withMaybeData)
import Fractal.Enum.BlobType as BlobType
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Html exposing (Html, a, br, button, canvas, datalist, div, h1, h2, hr, i, input, label, li, nav, option, p, select, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, name, placeholder, required, rows, selected, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Maybe exposing (withDefault)
import ModelCommon exposing (TensionPatchForm)
import ModelCommon.Codecs exposing (FractalBaseRoute(..), NodeFocus, getTensionCharac, uriFromUsername)
import ModelCommon.View exposing (FormText, actionNameStr, getAvatar, getNodeTextFromNodeType, roleColor)
import ModelSchema exposing (..)
import Time


type alias NodeDoc =
    { isBlobEdit : Bool
    }


create : NodeDoc
create =
    { isBlobEdit = False }


edit : NodeDoc -> NodeDoc
edit nd =
    { nd | isBlobEdit = True }


updateForm : String -> String -> Maybe TensionAction.TensionAction -> TensionPatchForm -> TensionPatchForm
updateForm field value action form =
    updateNodeForm field value { form | action = action }


cancelEdit : NodeDoc -> NodeDoc
cancelEdit nd =
    { nd | isBlobEdit = False }


type alias TensionNodeData msg =
    { onBlobEdit : BlobType.BlobType -> msg
    , onChangeNode : String -> String -> msg
    , onCancelBlob : msg
    , onSubmitBlob : Bool -> Time.Posix -> msg
    , onSubmit : (Time.Posix -> msg) -> msg
    , form : TensionPatchForm
    , result : GqlData PatchTensionPayloadID
    , tension : TensionHead
    , data : NodeDoc
    }


type alias OrgaNodeData msg =
    { data : GqlData String -- payload
    , node : NodeFragment
    , isLazy : Bool
    , source : FractalBaseRoute
    , focus : NodeFocus
    , hasBeenPushed : Bool
    , toolbar : Maybe (Html msg)
    }


view : OrgaNodeData msg -> Maybe (TensionNodeData msg) -> Html msg
view data tdata_m =
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
                    view_ tid data tdata_m

                other ->
                    div [] []
            ]
        ]


view_ : String -> OrgaNodeData msg -> Maybe (TensionNodeData msg) -> Html msg
view_ tid data tdata_m =
    let
        isLinksHidden =
            if data.node.type_ == Just NodeType.Circle && data.source == TensionBaseUri then
                data.hasBeenPushed

            else
                False

        txt =
            getNodeTextFromNodeType (data.node.type_ |> withDefault NodeType.Role)

        -- Function of TensionNodeData
        blobTypeEdit =
            tdata_m
                |> Maybe.map (\e -> ternary e.data.isBlobEdit e.form.blob_type Nothing)
                |> withDefault Nothing

        isLoading =
            tdata_m
                |> Maybe.map (\e -> e.result == LoadingSlowly)
                |> withDefault False
    in
    div [ classList [ ( "is-lazy", data.isLazy ) ] ]
        [ if blobTypeEdit == Just BlobType.OnAbout then
            tdata_m
                |> Maybe.map
                    (\e ->
                        let
                            isSendable =
                                data.node.name /= e.form.node.name || data.node.about /= e.form.node.about

                            isNew =
                                e.tension.action
                                    |> Maybe.map (\a -> (getTensionCharac a).action_type == "new")
                                    |> withDefault True
                        in
                        div []
                            [ nodeAboutInputView e.form.node isNew e.onChangeNode txt.name_help txt.about_help
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
                            , span [ class "content nodeName" ] [ text "\u{00A0}", text " ", text (data.node.name |> withDefault "") ]
                            ]
                        ]
                    , case data.toolbar of
                        Just tb ->
                            -- from OverviewBaseUri: show toolbar that is links to the tension id.
                            div [ class "column is-3 buttonsToolbar" ]
                                [ tb ]

                        Nothing ->
                            div [ class "column buttonEdit" ] [ doEditView tdata_m BlobType.OnAbout ]
                    ]
                , case data.node.about of
                    Just ab ->
                        p [ class "column is-fullwidth" ] [ text ab ]

                    Nothing ->
                        div [] []
                ]
        , hr [ class "has-background-grey-light" ] []
        , if blobTypeEdit == Just BlobType.OnFirstLink then
            tdata_m
                |> Maybe.map
                    (\e ->
                        let
                            isSendable =
                                data.node.first_link /= e.form.node.first_link || data.node.role_type /= e.form.node.role_type
                        in
                        div []
                            [ nodeLinksInputView e.form.node e.onChangeNode txt.firstLink_help
                            , blobButtonsView e isSendable isLoading
                            ]
                    )
                |> withDefault (div [] [])

          else if isLinksHidden then
            div [] []

          else
            let
                links =
                    getFirstLinks data.node
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
                    , doEditView tdata_m BlobType.OnFirstLink
                    ]
                , if List.length links == 0 then
                    span [ class "is-italic" ] [ text T.noFirstLinks ]

                  else
                    div [] []
                ]
        , ternary isLinksHidden (div [] []) (hr [ class "has-background-grey-light" ] [])
        , if blobTypeEdit == Just BlobType.OnMandate then
            tdata_m
                |> Maybe.map
                    (\e ->
                        let
                            isSendable =
                                data.node.mandate /= e.form.node.mandate
                        in
                        div [ class "mandateEdit" ]
                            [ nodeMandateInputView e.form.node e.onChangeNode txt
                            , blobButtonsView e isSendable isLoading
                            ]
                    )
                |> withDefault (div [] [])

          else
            case data.node.mandate of
                Just mandate ->
                    div [ class "mandateDoc" ]
                        [ div [ class "subtitle is-5" ]
                            [ Fa.icon "fas fa-scroll fa-sm" T.mandateH, doEditView tdata_m BlobType.OnMandate ]
                        , viewMandateSection T.purposeH (Just mandate.purpose)
                        , viewMandateSection T.responsabilitiesH mandate.responsabilities
                        , viewMandateSection T.domainsH mandate.domains
                        , viewMandateSection T.policiesH mandate.policies
                        ]

                Nothing ->
                    div [ class "is-italic" ]
                        [ text "No mandate for this circle."
                        , doEditView tdata_m BlobType.OnMandate
                        ]
        ]



---- Template view


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



-- Input view


nodeAboutInputView : NodeFragment -> Bool -> (String -> String -> msg) -> String -> String -> Html msg
nodeAboutInputView node isNew changePostMsg nameHelpText aboutHelpText =
    div [ class "field" ]
        [ div [ class "field " ]
            [ div [ class "control" ]
                [ input
                    [ class "input autofocus followFocus"
                    , attribute "data-nextfocus" "aboutField"
                    , type_ "text"
                    , placeholder "Name*"
                    , value (node.name |> withDefault "")
                    , onInput <| changePostMsg "name"
                    , required True
                    ]
                    []
                ]
            , p [ class "help-label" ] [ text nameHelpText ]
            ]
        , div [ class "field" ]
            [ div [ class "control" ]
                [ input
                    [ id "aboutField"
                    , class "input followFocus"
                    , attribute "data-nextfocus" "textAreaModal"
                    , type_ "text"
                    , placeholder "About"
                    , value (node.about |> withDefault "")
                    , onInput <| changePostMsg "about"
                    ]
                    []
                ]
            , p [ class "help-label" ] [ text aboutHelpText ]
            , br [] []
            ]
        , if isNew then
            div [ class "box has-background-grey-light is-paddingless" ]
                [ div [ class "field is-horizontal" ]
                    [ div [ class "field-label is-small has-text-grey-darker" ] [ text "Name ID" ]
                    , div [ class "field-body control" ]
                        [ input
                            [ class "input is-small"
                            , type_ "text"
                            , value (node.nameid |> withDefault "")
                            , onInput <| changePostMsg "nameid"
                            ]
                            []
                        ]
                    ]

                --, p [ class "help-label is-pulled-left", attribute "style" "margin-top: 4px !important;" ] [ text T.autoFieldMessageHelp ]
                ]

          else
            div [] []
        ]


nodeLinksInputView : NodeFragment -> (String -> String -> msg) -> String -> Html msg
nodeLinksInputView node changePostMsg firstLink_help =
    let
        nodeType =
            node.type_ |> withDefault NodeType.Role

        roleType =
            node.role_type |> withDefault RoleType.Peer

        firstLinks =
            getFirstLinks node
    in
    div []
        (List.indexedMap
            (\i uname ->
                div [ class "field is-horizontal" ]
                    [ div [ class "field-label is-small has-text-grey-darker control" ]
                        [ case nodeType of
                            NodeType.Circle ->
                                let
                                    r =
                                        RoleType.Coordinator
                                in
                                div [ class ("select is-" ++ roleColor r) ]
                                    [ select
                                        [ class "has-text-dark" --, onInput <| sd.changePostMsg "role_type"
                                        ]
                                        [ option [ selected True, value (RoleType.toString r) ] [ RoleType.toString r |> text ] ]
                                    ]

                            NodeType.Role ->
                                div [ class ("select is-" ++ roleColor roleType) ]
                                    [ RoleType.list
                                        |> List.filter (\r -> r /= RoleType.Guest && r /= RoleType.Member)
                                        |> List.map
                                            (\r ->
                                                option [ selected (roleType == r), value (RoleType.toString r) ] [ RoleType.toString r |> text ]
                                            )
                                        |> select [ class "has-text-dark", onInput <| changePostMsg "role_type" ]
                                    ]
                        ]
                    , div [ class "field-body control" ]
                        [ input
                            [ class "input is-small"
                            , type_ "text"
                            , value ("@" ++ uname)
                            , onInput <| changePostMsg "first_link"
                            ]
                            []
                        ]
                    ]
            )
            firstLinks
            ++ [ p [ class "help-label", attribute "style" "margin-top: 4px !important;" ] [ text firstLink_help ] ]
        )


nodeMandateInputView : NodeFragment -> (String -> String -> msg) -> FormText -> Html msg
nodeMandateInputView node changePostMsg txt =
    div [ class "field" ]
        [ div [ class "field" ]
            [ div [ class "label" ] [ text T.purposeH ]
            , div [ class "control" ]
                [ textarea
                    [ id "textAreaModal"
                    , class "textarea"
                    , rows 5
                    , placeholder (txt.ph_purpose ++ "*")
                    , value (node.mandate |> Maybe.map (\m -> m.purpose) |> withDefault "")
                    , onInput <| changePostMsg "purpose"
                    , required True
                    ]
                    []
                ]
            ]
        , div [ class "field" ]
            [ div [ class "label" ] [ text T.responsabilitiesH ]
            , div [ class "control" ]
                [ textarea
                    [ class "textarea"
                    , rows 5
                    , placeholder txt.ph_responsabilities
                    , value (node.mandate |> Maybe.map (\m -> m.responsabilities |> withDefault "") |> withDefault "")
                    , onInput <| changePostMsg "responsabilities"
                    ]
                    []
                ]
            ]
        , div [ class "field" ]
            [ div [ class "label" ] [ text T.domainsH ]
            , div [ class "control" ]
                [ textarea
                    [ class "textarea"
                    , rows 5
                    , placeholder txt.ph_domains
                    , value (node.mandate |> Maybe.map (\m -> m.domains |> withDefault "") |> withDefault "")
                    , onInput <| changePostMsg "domains"
                    ]
                    []
                ]
            ]
        , div [ class "field" ]
            [ div [ class "label" ] [ text T.policiesH ]
            , div [ class "control" ]
                [ textarea
                    [ class "textarea"
                    , rows 5
                    , placeholder txt.ph_policies
                    , value (node.mandate |> Maybe.map (\m -> m.policies |> withDefault "") |> withDefault "")
                    , onInput <| changePostMsg "policies"
                    ]
                    []
                ]
            ]
        ]



---- Components view


doEditView : Maybe (TensionNodeData msg) -> BlobType.BlobType -> Html msg
doEditView tdata_m btype =
    case tdata_m of
        Just e ->
            if e.data.isBlobEdit && Just btype == e.form.blob_type then
                span [] []

            else
                span
                    [ class "button has-text-weight-normal is-pulled-right is-small"
                    , onClick (e.onBlobEdit btype)
                    ]
                    [ Fa.icon0 "fas fa-pen" "" ]

        Nothing ->
            span [] []


blobButtonsView : TensionNodeData msg -> Bool -> Bool -> Html msg
blobButtonsView tdata isSendable isLoading =
    div []
        [ case tdata.result of
            Failure err ->
                viewGqlErrors err

            _ ->
                div [] []
        , div [ class "field is-grouped is-grouped-right" ]
            [ div [ class "control" ]
                [ div [ class "buttons" ]
                    [ button
                        [ class "button has-text-weight-semibold is-danger"
                        , onClick tdata.onCancelBlob
                        ]
                        [ text T.cancel ]
                    , button
                        [ class "button has-text-weight-semibold"
                        , classList [ ( "is-success", isSendable ), ( "is-loading", isLoading ) ]
                        , disabled (not isSendable)
                        , onClick (tdata.onSubmit <| tdata.onSubmitBlob False)
                        ]
                        [ text T.saveChanges ]
                    ]
                ]
            ]
        ]



--- Utils


getFirstLinks : NodeFragment -> List String
getFirstLinks node =
    let
        fs =
            case node.type_ |> withDefault NodeType.Role of
                NodeType.Role ->
                    node.first_link |> withDefault "" |> String.split "@" |> List.filter (\x -> x /= "")

                NodeType.Circle ->
                    case node.children of
                        Just children ->
                            children
                                |> List.map (\c -> c.first_link)
                                |> List.filterMap identity

                        Nothing ->
                            node.first_link |> withDefault "" |> String.split "@" |> List.filter (\x -> x /= "")
    in
    ternary (fs == []) [ "" ] fs


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
