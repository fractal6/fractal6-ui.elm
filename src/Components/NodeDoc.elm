module Components.NodeDoc exposing
    ( InputData
    , NodeDoc
    , cancelEdit
    , cancelUser
    , create
    , edit
    , getFirstLinks
    , getInputData
    , nodeAboutInputView
    , nodeFragmentFromOrga
    , nodeLinksInputView
    , nodeMandateInputView
    , selectUser
    , updateForm
    , updateNodeForm
    , updateUserPattern
    , updateUserRole
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
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (TensionPatchForm, UserForm)
import ModelCommon.Codecs exposing (FractalBaseRoute(..), NodeFocus, getTensionCharac, uriFromUsername)
import ModelCommon.View exposing (FormText, actionNameStr, getAvatar, getNodeTextFromNodeType, roleColor, viewUser)
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


cancelEdit : NodeDoc -> NodeDoc
cancelEdit nd =
    { nd | isBlobEdit = False }


{-|

    reset action and title (Tension title can't be change in this function)

-}
updateForm : String -> String -> Maybe TensionAction.TensionAction -> TensionPatchForm -> TensionPatchForm
updateForm field value action form =
    let
        oldAction =
            form.action

        f =
            updateNodeForm field value { form | action = action }
    in
    { f | action = oldAction, post = Dict.remove "title" f.post }


updateUserPattern : Int -> String -> List UserForm -> List UserForm
updateUserPattern pos pattern users =
    LE.updateAt pos (\x -> { x | pattern = pattern }) users


updateUserRole : Int -> String -> List UserForm -> List UserForm
updateUserRole pos r users =
    LE.updateAt pos (\x -> { x | role_type = RoleType.fromString r |> withDefault RoleType.Peer }) users


selectUser : Int -> String -> List UserForm -> List UserForm
selectUser pos username users =
    LE.updateAt pos (\x -> { x | username = username }) users


cancelUser : Int -> List UserForm -> List UserForm
cancelUser pos users =
    --LE.removeAt pos users
    LE.updateAt pos (\x -> { x | username = "" }) users


type alias OrgaNodeData msg =
    { node : NodeFragment
    , isLazy : Bool
    , source : FractalBaseRoute
    , focus : NodeFocus
    , hasBeenPushed : Bool
    , toolbar : Maybe (Html msg)
    , data : GqlData String
    }


type alias TensionNodeData msg =
    { form : TensionPatchForm
    , result : GqlData PatchTensionPayloadID
    , tension : TensionHead

    --, userLookup : UserLookup
    , data : NodeDoc
    , onBlobEdit : BlobType.BlobType -> msg
    , onChangeNode : String -> String -> msg
    , onChangeUserPattern : Int -> String -> msg
    , onChangeUserRole : Int -> String -> msg
    , onSelectUser : Int -> String -> msg
    , onCancelUser : Int -> msg
    , onCancelBlob : msg
    , onSubmitBlob : Bool -> Time.Posix -> msg
    , onSubmit : (Time.Posix -> msg) -> msg
    }


{-| Ensure compatibility with TensionForm (user in Form.NewTenion)
-}
type alias InputData msg =
    { node : NodeFragment
    , users : List UserForm

    --, txt : FormText
    , onChangeNode : String -> String -> msg
    , onChangeUserPattern : Int -> String -> msg
    , onChangeUserRole : Int -> String -> msg
    , onSelectUser : Int -> String -> msg
    , onCancelUser : Int -> msg
    }


getInputData e =
    { node = e.form.node
    , users = e.form.users
    , onChangeNode = e.onChangeNode
    , onChangeUserPattern = e.onChangeUserPattern
    , onChangeUserRole = e.onChangeUserRole
    , onSelectUser = e.onSelectUser
    , onCancelUser = e.onCancelUser
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
                            [ nodeAboutInputView isNew txt (getInputData e)
                            , blobButtonsView isSendable isLoading e
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
                            [ nodeLinksInputView txt (getInputData e)
                            , blobButtonsView isSendable isLoading e
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
                        |> List.map (\l -> viewUser l)
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
                            [ nodeMandateInputView txt (getInputData e)
                            , blobButtonsView isSendable isLoading e
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


nodeAboutInputView : Bool -> FormText -> InputData msg -> Html msg
nodeAboutInputView isNew txt ipd =
    let
        node =
            ipd.node
    in
    div [ class "field" ]
        [ div [ class "field " ]
            [ div [ class "control" ]
                [ input
                    [ class "input autofocus followFocus"
                    , attribute "data-nextfocus" "aboutField"
                    , type_ "text"
                    , placeholder "Name*"
                    , value (node.name |> withDefault "")
                    , onInput <| ipd.onChangeNode "name"
                    , required True
                    ]
                    []
                ]
            , p [ class "help-label" ] [ text txt.name_help ]
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
                    , onInput <| ipd.onChangeNode "about"
                    ]
                    []
                ]
            , p [ class "help-label" ] [ text txt.about_help ]
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
                            , onInput <| ipd.onChangeNode "nameid"
                            ]
                            []
                        ]
                    ]

                --, p [ class "help-label is-pulled-left", attribute "style" "margin-top: 4px !important;" ] [ text T.autoFieldMessageHelp ]
                ]

          else
            div [] []
        ]


nodeLinksInputView : FormText -> InputData msg -> Html msg
nodeLinksInputView txt ipd =
    let
        nodeType =
            ipd.node.type_ |> withDefault NodeType.Role

        --roleType =
        --    node.role_type |> withDefault RoleType.Peer
        --firstLinks =
        --    getFirstLinks node
    in
    div []
        (List.indexedMap
            (\i u ->
                let
                    rt =
                        u.role_type

                    rtStr =
                        RoleType.toString rt

                    userSelected =
                        u.username /= ""
                in
                div [ class "field is-horizontal" ]
                    [ div [ class "field-label is-small has-text-grey-darker control" ]
                        [ case nodeType of
                            NodeType.Circle ->
                                div [ class ("select is-" ++ roleColor rt) ]
                                    [ select [ class "has-text-dark" ]
                                        [ option [ selected True, value rtStr ] [ text rtStr ] ]
                                    ]

                            NodeType.Role ->
                                div [ class ("select is-" ++ roleColor rt) ]
                                    [ RoleType.list
                                        |> List.filter (\r -> r /= RoleType.Guest && r /= RoleType.Member)
                                        |> List.map
                                            (\r ->
                                                option [ selected (r == rt), value (RoleType.toString r) ] [ text (RoleType.toString r) ]
                                            )
                                        |> select [ class "has-text-dark", onInput (ipd.onChangeUserRole i) ]
                                    ]
                        ]
                    , div [ class "field-body" ]
                        [ div [ class "tagsinput field is-grouped is-grouped-multiline input" ]
                            [ if userSelected then
                                div [ class "control" ]
                                    [ div [ class "tags has-addons" ]
                                        [ span [ class "tag is-primary" ] [ text u.username ]
                                        , span [ class "tag is-delete is-light", onClick (ipd.onCancelUser i) ] []
                                        ]
                                    ]

                              else
                                span [] []
                            , input
                                [ type_ "text"
                                , value u.pattern
                                , onInput (ipd.onChangeUserPattern i)
                                , disabled userSelected
                                ]
                                []
                            ]
                        ]
                    ]
            )
            ipd.users
            ++ [ p [ class "help-label", attribute "style" "margin-top: 4px !important;" ] [ text txt.firstLink_help ] ]
        )


nodeMandateInputView : FormText -> InputData msg -> Html msg
nodeMandateInputView txt ipd =
    let
        node =
            ipd.node
    in
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
                    , onInput <| ipd.onChangeNode "purpose"
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
                    , onInput <| ipd.onChangeNode "responsabilities"
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
                    , onInput <| ipd.onChangeNode "domains"
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
                    , onInput <| ipd.onChangeNode "policies"
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


blobButtonsView : Bool -> Bool -> TensionNodeData msg -> Html msg
blobButtonsView isSendable isLoading tdata =
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
        "nameid" ->
            { form | node = { node | nameid = Just value } }

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
