module Components.NodeDoc exposing (..)

import Components.Doc exposing (ActionView(..))
import Components.Fa as Fa
import Components.Loading as Loading exposing (GqlData, RequestResult(..), viewGqlErrors, withMaybeData)
import Components.Markdown exposing (renderMarkdown)
import Components.UserSearchPanel as UserSearchPanel
import Dict
import Extra exposing (ternary)
import Fractal.Enum.BlobType as BlobType
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Html exposing (Html, a, br, button, canvas, datalist, div, h1, h2, hr, i, input, label, li, nav, option, p, select, span, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, name, placeholder, required, rows, selected, size, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import List.Extra as LE
import Maybe exposing (withDefault)
import ModelCommon exposing (TensionPatchForm, UserForm, UserState(..), initTensionPatchForm)
import ModelCommon.Codecs exposing (ActionType(..), FractalBaseRoute(..), NodeFocus, nodeIdCodec, uriFromNameid, uriFromUsername)
import ModelCommon.View exposing (FormText, actionNameStr, getAvatar, getNodeTextFromNodeType, roleColor, viewUser)
import ModelSchema exposing (..)
import Text as T
import Time


type alias NodeDoc =
    { form : TensionPatchForm
    , result : GqlData PatchTensionPayloadID
    , isBlobEdit : Bool
    , isLookupOpen : Bool
    , doAddResponsabilities : Bool
    , doAddDomains : Bool
    , doAddPolicies : Bool
    }


create : String -> UserState -> NodeDoc
create tid user =
    { form = initTensionPatchForm tid user
    , result = NotAsked
    , isBlobEdit = False
    , isLookupOpen = False
    , doAddResponsabilities = False
    , doAddDomains = False
    , doAddPolicies = False
    }


initBlob : BlobType.BlobType -> NodeFragment -> NodeDoc -> NodeDoc
initBlob blobType nf data =
    let
        form =
            data.form

        links =
            getFirstLinks nf

        users =
            if List.length links == 0 then
                [ { username = "", role_type = RoleType.Peer, pattern = "" } ]

            else
                links

        newForm =
            { form
                | blob_type = Just blobType
                , node = nf
                , users = users
            }
    in
    { data | form = newForm, result = NotAsked }



-- State Controls


edit : NodeDoc -> NodeDoc
edit data =
    { data | isBlobEdit = True }


cancelEdit : NodeDoc -> NodeDoc
cancelEdit data =
    { data | isBlobEdit = False }


setForm : TensionPatchForm -> NodeDoc -> NodeDoc
setForm form data =
    { data | form = form }


setResult : GqlData PatchTensionPayloadID -> NodeDoc -> NodeDoc
setResult result data =
    { data | result = result }


addResponsabilities : NodeDoc -> NodeDoc
addResponsabilities data =
    { data | doAddResponsabilities = True }


addDomains : NodeDoc -> NodeDoc
addDomains data =
    { data | doAddDomains = True }


addPolicies : NodeDoc -> NodeDoc
addPolicies data =
    { data | doAddPolicies = True }



-- Update Form


setEvents : List TensionEvent.TensionEvent -> NodeDoc -> NodeDoc
setEvents events data =
    let
        f =
            data.form

        newForm =
            { f | events_type = Just events }
    in
    { data | form = newForm }


post : String -> String -> NodeDoc -> NodeDoc
post field value data =
    let
        f =
            data.form

        newForm =
            { f | post = Dict.insert field value f.post }
    in
    { data | form = newForm }


postNode : String -> String -> Maybe TensionAction.TensionAction -> NodeDoc -> NodeDoc
postNode field value action data =
    -- reset action and title (Tension title can't be change in this function)
    let
        form_ =
            data.form

        oldAction =
            form_.action

        form =
            updateNodeForm field value { form_ | action = action }

        newForm =
            { form | action = oldAction, post = Dict.remove "title" form.post }
    in
    { data | form = newForm }



-- User Lookup


updateUserPattern : Int -> String -> NodeDoc -> NodeDoc
updateUserPattern pos pattern data =
    let
        f =
            data.form

        newForm =
            { f | users = updateUserPattern_ pos pattern f.users }
    in
    { data | form = newForm }


updateUserRole : Int -> String -> NodeDoc -> NodeDoc
updateUserRole pos role data =
    let
        f =
            data.form

        newForm =
            { f | users = updateUserRole_ pos role f.users }
    in
    { data | form = newForm }


selectUser : Int -> String -> NodeDoc -> NodeDoc
selectUser pos username data =
    let
        f =
            data.form

        newForm =
            { f | users = selectUser_ pos username f.users }
    in
    { data | form = newForm, isLookupOpen = False }


cancelUser : Int -> NodeDoc -> NodeDoc
cancelUser pos data =
    let
        f =
            data.form

        newForm =
            { f | users = cancelUser_ pos f.users }
    in
    { data | form = newForm, isLookupOpen = False }


openLookup : NodeDoc -> NodeDoc
openLookup data =
    { data | isLookupOpen = True }


closeLookup : NodeDoc -> NodeDoc
closeLookup data =
    { data | isLookupOpen = False }


type alias OrgaNodeData msg =
    { node : NodeFragment
    , isLazy : Bool
    , source : FractalBaseRoute
    , hasBeenPushed : Bool
    , toolbar : Maybe (Html msg)
    , receiver : String
    , data : GqlData String
    }


type alias Op msg =
    { lookup : List User
    , users_data : GqlData UsersData
    , targets : List String
    , data : NodeDoc

    -- Blob control
    , onBlobEdit : BlobType.BlobType -> msg
    , onCancelBlob : msg
    , onSubmitBlob : NodeDoc -> Time.Posix -> msg
    , onSubmit : (Time.Posix -> msg) -> msg

    -- Doc change
    , onChangeNode : String -> String -> msg
    , onAddResponsabilities : msg
    , onAddDomains : msg
    , onAddPolicies : msg

    -- User search and change
    , onChangeUserPattern : Int -> String -> msg
    , onChangeUserRole : Int -> String -> msg
    , onSelectUser : Int -> String -> msg
    , onCancelUser : Int -> msg
    , onShowLookupFs : msg
    , onCancelLookupFs : msg
    }


view : OrgaNodeData msg -> Maybe (Op msg) -> Html msg
view data op_m =
    div [ id "DocContainer", class "hero is-small is-light" ]
        [ div [ class "hero-body" ]
            [ case data.data of
                Failure err ->
                    viewGqlErrors err

                LoadingSlowly ->
                    div [ class "spinner" ] []

                Success tid ->
                    view_ tid data op_m

                other ->
                    text ""
            ]
        ]


view_ : String -> OrgaNodeData msg -> Maybe (Op msg) -> Html msg
view_ tid data op_m =
    let
        type_ =
            data.node.type_ |> withDefault NodeType.Role

        isLinksHidden =
            if type_ == NodeType.Circle && data.source == TensionBaseUri then
                data.hasBeenPushed

            else
                False

        txt =
            getNodeTextFromNodeType type_

        links =
            getFirstLinks data.node

        -- Function of Op
        blobTypeEdit =
            op_m
                |> Maybe.map (\op -> ternary op.data.isBlobEdit op.data.form.blob_type Nothing)
                |> withDefault Nothing

        isLoading =
            op_m
                |> Maybe.map (\op -> op.data.result == LoadingSlowly)
                |> withDefault False
    in
    div [ classList [ ( "is-lazy", data.isLazy ) ] ]
        [ if blobTypeEdit == Just BlobType.OnAbout then
            op_m
                |> Maybe.map
                    (\op ->
                        let
                            isSendable =
                                data.node.name /= op.data.form.node.name || data.node.about /= op.data.form.node.about
                        in
                        div []
                            [ nodeAboutInputView data.hasBeenPushed data.source txt op.data.form.node op
                            , blobButtonsView isSendable isLoading op
                            ]
                    )
                |> withDefault (text "")

          else
            div [ class "aboutDoc" ]
                [ div [ class "media subtitle" ]
                    [ div [ class "media-left" ]
                        [ span [ class "fa-stack", attribute "style" "font-size: 0.6em;" ]
                            [ i [ class "fas fa-info fa-stack-1x" ] []
                            , i [ class "far fa-circle fa-stack-2x" ] []
                            ]
                        ]
                    , div [ class "media-content nodeName" ]
                        [ (if data.hasBeenPushed && data.source == TensionBaseUri then
                            let
                                nameid =
                                    data.node.nameid
                                        |> Maybe.map (\nid -> nodeIdCodec data.receiver nid type_)
                                        |> withDefault ""
                            in
                            a [ nameid |> uriFromNameid OverviewBaseUri |> href ]

                           else
                            span []
                          )
                            [ text "\u{00A0}", text " ", text (data.node.name |> withDefault "") ]
                        ]
                    , case data.toolbar of
                        Just tb ->
                            -- from OverviewBaseUri: show toolbar that is links to the tension id.
                            div [ class "media-right is-marginless buttonsToolbar" ] [ tb ]

                        Nothing ->
                            div [ class "media-right is-marginless buttonEdit" ] [ doEditView op_m BlobType.OnAbout ]
                    ]
                , case data.node.about of
                    Just ab ->
                        p [ class "column is-fullwidth" ] [ text ab ]

                    Nothing ->
                        text ""
                ]
        , hr [ class "has-background-grey-light" ] []
        , if blobTypeEdit == Just BlobType.OnMandate then
            op_m
                |> Maybe.map
                    (\op ->
                        let
                            isSendable =
                                data.node.mandate /= op.data.form.node.mandate
                        in
                        div [ class "mandateEdit" ]
                            [ nodeMandateInputView txt op.data.form.node op
                            , blobButtonsView isSendable isLoading op
                            ]
                    )
                |> withDefault (text "")

          else
            case data.node.mandate of
                Just mandate ->
                    div [ class "mandateDoc" ]
                        [ div [ class "subtitle is-5" ]
                            [ Fa.icon "fas fa-scroll fa-sm" T.mandateH, doEditView op_m BlobType.OnMandate ]
                        , viewMandateSection T.purposeH (Just mandate.purpose)
                        , viewMandateSection T.responsabilitiesH mandate.responsabilities
                        , viewMandateSection T.domainsH mandate.domains
                        , viewMandateSection T.policiesH mandate.policies
                        ]

                Nothing ->
                    case data.node.role_type of
                        Just RoleType.Guest ->
                            a [ class "is-size-6", href "https://doc.fractale.co/role/guest" ] [ text "https://doc.fractale.co/role/guest" ]

                        _ ->
                            div [ class "is-italic" ]
                                [ text "No description for this node."
                                , doEditView op_m BlobType.OnMandate
                                ]
        , ternary isLinksHidden (text "") (hr [ class "has-background-grey-light" ] [])
        , if blobTypeEdit == Just BlobType.OnFirstLink then
            op_m
                |> Maybe.map
                    (\op ->
                        let
                            isSendable =
                                op.data.form.users /= links
                        in
                        div []
                            [ nodeLinksInputView txt op.data.form op.data op
                            , blobButtonsView isSendable isLoading op
                            ]
                    )
                |> withDefault (text "")

          else if isLinksHidden then
            text ""

          else
            let
                links_ =
                    List.filter (\x -> x.username /= "") links
            in
            div [ class "linksDoc" ]
                [ div [ class "subtitle is-5" ]
                    [ Fa.icon "fas fa-users fa-sm" T.linksH
                    , links_
                        |> List.map (\l -> viewUser True l.username)
                        |> span [ attribute "style" "margin-left:20px;" ]
                    , doEditView op_m BlobType.OnFirstLink
                    ]
                , if List.length links_ == 0 then
                    span [ class "is-italic" ] [ text txt.noFirstLinks ]

                  else
                    text ""
                ]
        ]



--- Template view


viewMandateSection : String -> Maybe String -> Html msg
viewMandateSection name maybePara =
    case maybePara of
        Just para ->
            div [ class "message" ]
                [ div [ class "message-header" ] [ text name ]
                , p [ class "message-body" ] [ renderMarkdown "is-dark" para ]
                ]

        Nothing ->
            text ""



--- Input view


nodeAboutInputView hasBeenPushed source txt node op =
    div [ class "field" ]
        [ div [ class "field " ]
            [ div [ class "control" ]
                [ input
                    [ class "input autofocus followFocus"
                    , attribute "data-nextfocus" "aboutField"
                    , type_ "text"
                    , placeholder "Name*"
                    , value (node.name |> withDefault "")
                    , onInput <| op.onChangeNode "name"
                    , required True
                    ]
                    []
                , if hasBeenPushed == False then
                    div [ class "subForm" ]
                        [ div [ class "field is-horizontal" ]
                            [ div [ class "field-body control" ]
                                [ div [] [ text "URL" ]
                                , input [ class "input", size 13, disabled True, value "https://fractale.co/o/" ] []
                                , input
                                    [ class "input"
                                    , type_ "text"
                                    , value (node.nameid |> withDefault "")
                                    , onInput <| op.onChangeNode "nameid"
                                    ]
                                    []
                                ]
                            ]
                        ]

                  else
                    text ""
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
                    , onInput <| op.onChangeNode "about"
                    ]
                    []
                ]
            , p [ class "help-label" ] [ text txt.about_help ]
            , br [] []
            ]
        ]


nodeLinksInputView txt form data op =
    let
        nodeType =
            form.node.type_ |> withDefault NodeType.Role
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
                div []
                    [ div [ class "field is-horizontal" ]
                        [ div [ class "field-label is-md has-text-grey-darker control" ]
                            [ case nodeType of
                                NodeType.Circle ->
                                    div [ class ("select is-" ++ roleColor rt) ]
                                        [ select [ class "has-text-dark" ]
                                            [ option [ selected True, value rtStr ] [ text rtStr ] ]
                                        ]

                                NodeType.Role ->
                                    div [ class ("select is-" ++ roleColor rt) ]
                                        [ [ RoleType.Coordinator, RoleType.Peer ]
                                            -- @debug: separate concerns in role type ?
                                            |> List.map
                                                (\r ->
                                                    option [ selected (r == rt), value (RoleType.toString r) ]
                                                        [ text (RoleType.toString r) ]
                                                )
                                            |> select [ class "has-text-dark", onInput (op.onChangeUserRole i) ]
                                        ]
                            ]
                        , div [ class "field-body" ]
                            [ div
                                [ class "tagsinput field is-grouped is-grouped-multiline input" ]
                                [ if userSelected then
                                    div [ class "control" ]
                                        [ div [ class "tags has-addons" ]
                                            [ span [ class "tag is-primary" ] [ text u.username ]
                                            , span [ class "tag is-delete is-light", onClick (op.onCancelUser i) ] []
                                            ]
                                        ]

                                  else
                                    span [] []
                                , input
                                    [ type_ "text"
                                    , class "is-expanded"
                                    , placeholder (ternary (u.username == "") T.searchUsers "")
                                    , value u.pattern
                                    , onInput (op.onChangeUserPattern i)
                                    , onClick op.onShowLookupFs
                                    , disabled userSelected
                                    ]
                                    []
                                ]
                            ]
                        ]
                    , if data.isLookupOpen then
                        div
                            [ id "userSearchPanel", class "panel in-horizon sidePanel" ]
                            [ UserSearchPanel.viewSelectors i u.pattern op ]

                      else
                        span [] []
                    ]
            )
            form.users
            ++ [ p [ class "help-label", attribute "style" "margin-top: 4px !important;" ] [ text txt.firstLink_help ] ]
        )


nodeMandateInputView txt node op =
    let
        purpose =
            node.mandate |> Maybe.map (\m -> m.purpose) |> withDefault ""

        responsabilities =
            node.mandate |> Maybe.map (\m -> m.responsabilities |> withDefault "") |> withDefault ""

        domains =
            node.mandate |> Maybe.map (\m -> m.domains |> withDefault "") |> withDefault ""

        policies =
            node.mandate |> Maybe.map (\m -> m.policies |> withDefault "") |> withDefault ""

        showResponsabilities =
            op.data.doAddResponsabilities || responsabilities /= ""

        showDomains =
            op.data.doAddDomains || domains /= ""

        showPolicies =
            op.data.doAddPolicies || policies /= ""
    in
    div [ class "" ]
        [ div [ class "field" ]
            [ div [ class "label" ]
                [ text (T.purposeH ++ " *") ]
            , div [ class "control" ]
                [ textarea
                    [ id "textAreaModal"
                    , class "textarea"
                    , rows 5
                    , placeholder txt.ph_purpose
                    , value purpose
                    , onInput <| op.onChangeNode "purpose"
                    , required True
                    ]
                    []
                ]
            ]
        , if showResponsabilities then
            div [ class "field" ]
                [ div [ class "label" ] [ text T.responsabilitiesH ]
                , div [ class "control" ]
                    [ textarea
                        [ class "textarea"
                        , rows 5
                        , placeholder txt.ph_responsabilities
                        , value responsabilities
                        , onInput <| op.onChangeNode "responsabilities"
                        ]
                        []
                    ]
                ]

          else
            text ""
        , if showDomains then
            div [ class "field" ]
                [ div [ class "label" ] [ text T.domainsH ]
                , div [ class "control" ]
                    [ textarea
                        [ class "textarea"
                        , rows 5
                        , placeholder txt.ph_domains
                        , value domains
                        , onInput <| op.onChangeNode "domains"
                        ]
                        []
                    ]
                ]

          else
            text ""
        , if showPolicies then
            div [ class "field" ]
                [ div [ class "label" ] [ text T.policiesH ]
                , div [ class "control" ]
                    [ textarea
                        [ class "textarea"
                        , rows 5
                        , placeholder txt.ph_policies
                        , value policies
                        , onInput <| op.onChangeNode "policies"
                        ]
                        []
                    ]
                ]

          else
            text ""
        , if showResponsabilities == False then
            span [ class "pr-2" ]
                [ div [ class "button is-small is-success", onClick op.onAddResponsabilities ]
                    [ Fa.icon "fas fa-plus" "", text T.addResponsabilities ]
                ]

          else
            text ""
        , if showDomains == False then
            span [ class "pr-2" ]
                [ div [ class "button is-small is-success", onClick op.onAddDomains ]
                    [ Fa.icon "fas fa-plus" "", text T.addDomains ]
                ]

          else
            text ""
        , if showPolicies == False then
            span [ class "pr-2" ]
                [ div [ class "button is-small is-success", onClick op.onAddPolicies ]
                    [ Fa.icon "fas fa-plus" "", text T.addPolicies ]
                ]

          else
            text ""
        ]



---- Components view


doEditView : Maybe (Op msg) -> BlobType.BlobType -> Html msg
doEditView op_m btype =
    case op_m of
        Just op ->
            if op.data.isBlobEdit && Just btype == op.data.form.blob_type then
                span [] []

            else
                span
                    [ class "button has-text-weight-normal is-pulled-right is-small"
                    , onClick (op.onBlobEdit btype)
                    ]
                    [ Fa.icon0 "fas fa-pen" "" ]

        Nothing ->
            span [] []


blobButtonsView : Bool -> Bool -> Op msg -> Html msg
blobButtonsView isSendable isLoading op =
    div []
        [ case op.data.result of
            Failure err ->
                viewGqlErrors err

            _ ->
                text ""
        , div [ class "field is-grouped is-grouped-right" ]
            [ div [ class "control" ]
                [ div [ class "buttons" ]
                    [ button
                        [ class "button has-text-weight-semibold"
                        , onClick op.onCancelBlob
                        ]
                        [ text T.cancel ]
                    , button
                        [ class "button is-success has-text-weight-semibold"
                        , classList [ ( "is-loading", isLoading ) ]
                        , disabled (not isSendable)
                        , onClick (op.onSubmit <| op.onSubmitBlob op.data)
                        ]
                        [ text T.saveChanges ]
                    ]
                ]
            ]
        ]



--- Utils


getFirstLinks : NodeFragment -> List UserForm
getFirstLinks node =
    case node.type_ |> withDefault NodeType.Role of
        NodeType.Role ->
            node.first_link
                |> Maybe.map
                    (\uname ->
                        { username = uname
                        , role_type = withDefault RoleType.Peer node.role_type
                        , pattern = ""
                        }
                    )
                |> List.singleton
                |> List.filterMap identity

        NodeType.Circle ->
            node.children
                |> Maybe.map
                    (\children ->
                        children
                            |> List.map
                                (\c ->
                                    c.first_link
                                        |> Maybe.map
                                            (\uname ->
                                                { username = uname
                                                , role_type = withDefault RoleType.Peer node.role_type
                                                , pattern = ""
                                                }
                                            )
                                )
                            |> List.filterMap identity
                    )
                |> withDefault []


nodeFragmentFromOrga : Maybe Node -> GqlData NodeData -> List EmitterOrReceiver -> NodesData -> NodeFragment
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
            withDefault initMandate node.mandate
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
            { form | node = { node | mandate = Just { mandate | responsabilities = ternary (value == "") Nothing (Just value) } } }

        "domains" ->
            { form | node = { node | mandate = Just { mandate | domains = ternary (value == "") Nothing (Just value) } } }

        "policies" ->
            { form | node = { node | mandate = Just { mandate | policies = ternary (value == "") Nothing (Just value) } } }

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
                                    | name = Just (String.trim value)
                                    , nameid = Just (makeNewNodeId value)
                                }
                        in
                        { form | post = newPost, node = newData }

                    else
                        { form | node = { node | name = Just value } }

        other ->
            -- title, message...
            { form | post = Dict.insert field value form.post }


makeNewNodeId : String -> String
makeNewNodeId name =
    name
        |> String.trim
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



-- User Lookup utilities


updateUserPattern_ : Int -> String -> List UserForm -> List UserForm
updateUserPattern_ pos pattern users =
    LE.updateAt pos (\x -> { x | pattern = pattern }) users


updateUserRole_ : Int -> String -> List UserForm -> List UserForm
updateUserRole_ pos r users =
    LE.updateAt pos (\x -> { x | role_type = RoleType.fromString r |> withDefault RoleType.Peer }) users


selectUser_ : Int -> String -> List UserForm -> List UserForm
selectUser_ pos username users =
    LE.updateAt pos (\x -> { x | username = username, pattern = "" }) users


cancelUser_ : Int -> List UserForm -> List UserForm
cancelUser_ pos users =
    --LE.removeAt pos users
    LE.updateAt pos (\x -> { x | username = "" }) users
