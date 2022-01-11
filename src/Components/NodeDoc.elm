module Components.NodeDoc exposing (..)

import Assets as A
import Components.DocToolBar exposing (ActionView(..))
import Components.Loading as Loading exposing (GqlData, RequestResult(..), viewGqlErrors, withDefaultData, withMaybeData)
import Dict
import Extra exposing (cleanDup, ternary)
import Extra.Date exposing (formatDate)
import Fractal.Enum.BlobType as BlobType
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Generated.Route as Route exposing (Route, toHref)
import Html exposing (Html, a, br, button, canvas, datalist, div, h1, h2, hr, i, input, label, li, nav, option, p, select, span, table, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, name, placeholder, required, rows, selected, size, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import List.Extra as LE
import Markdown exposing (renderMarkdown)
import Maybe exposing (withDefault)
import ModelCommon exposing (Ev, TensionPatchForm, UserForm, UserState(..), initTensionPatchForm)
import ModelCommon.Codecs exposing (ActionType(..), FractalBaseRoute(..), NodeFocus, nid2rootid, nodeIdCodec, uriFromNameid, uriFromUsername)
import ModelCommon.View exposing (FormText, actionNameStr, blobTypeStr, byAt, getNodeTextFromNodeType, roleColor, viewUser)
import ModelSchema exposing (..)
import String.Extra as SE
import Text as T exposing (textH, textT, upH)
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

        newForm =
            { form | blob_type = Just blobType, node = nf }
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


setEvents : List Ev -> NodeDoc -> NodeDoc
setEvents events data =
    let
        f =
            data.form

        newForm =
            { f | events = events }
    in
    { data | form = newForm }


updatePost : String -> String -> NodeDoc -> NodeDoc
updatePost field value data =
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
    , users_data : GqlData UsersDict
    , targets : List String
    , data : NodeDoc

    -- Blob control
    , onBlobEdit : BlobType.BlobType -> msg
    , onCancelBlob : msg
    , onSubmitBlob : NodeDoc -> Time.Posix -> msg
    , onSubmit : (Time.Posix -> msg) -> msg

    -- Doc change
    , onChangePost : String -> String -> msg
    , onAddResponsabilities : msg
    , onAddDomains : msg
    , onAddPolicies : msg

    -- User search and change
    , onChangeUserPattern : Int -> String -> msg
    , onSelectUser : Int -> String -> msg
    , onCancelUser : Int -> msg
    , onShowLookupFs : msg
    , onCancelLookupFs : msg
    }


view : OrgaNodeData msg -> Maybe (Op msg) -> Html msg
view data op_m =
    div [ id "DocContainer", class "box" ]
        [ case data.data of
            Failure err ->
                viewGqlErrors err

            LoadingSlowly ->
                div [ class "spinner" ] []

            Success tid ->
                view_ tid data op_m

            _ ->
                text ""
        ]


view_ : String -> OrgaNodeData msg -> Maybe (Op msg) -> Html msg
view_ tid data op_m =
    let
        type_ =
            withDefault NodeType.Role data.node.type_

        --isLinksHidden =
        --    if type_ == NodeType.Circle && data.source == TensionBaseUri then
        --        data.hasBeenPushed
        --    else
        --        False
        txt =
            getNodeTextFromNodeType type_

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
            div [ class "aboutDoc" ] [ viewAboutSection (doEditView op_m BlobType.OnAbout) data ]
        , hr [ class "has-background-border-light" ] []
        , if blobTypeEdit == Just BlobType.OnMandate then
            op_m
                |> Maybe.map
                    (\op ->
                        let
                            isSendable =
                                data.node.mandate /= op.data.form.node.mandate
                        in
                        div []
                            [ nodeMandateInputView txt op.data.form.node op
                            , blobButtonsView isSendable isLoading op
                            ]
                    )
                |> withDefault (text "")

          else
            div [ class "mandateDoc" ] [ viewMandateSection (doEditView op_m BlobType.OnMandate) data ]
        ]



--- Template view


viewAboutSection : Html msg -> OrgaNodeData msg -> Html msg
viewAboutSection editView data =
    let
        type_ =
            withDefault NodeType.Role data.node.type_
    in
    div []
        [ div [ class "media subtitle" ]
            [ div [ class "media-left" ]
                [ A.icon "icon-info icon-1half" ]
            , div [ class "media-content nodeName" ]
                [ if data.source == TensionBaseUri && data.hasBeenPushed then
                    let
                        nameid =
                            data.node.nameid
                                |> Maybe.map (\nid -> nodeIdCodec data.receiver nid type_)
                                |> withDefault ""
                    in
                    span []
                        [ textH T.about
                        , text T.space_
                        , a
                            [ class "is-discrete-2"
                            , nameid |> uriFromNameid OverviewBaseUri |> href
                            ]
                            [ withDefault "" data.node.name |> text ]
                        ]

                  else if data.source == OverviewBaseUri then
                    let
                        nameid =
                            data.node.nameid
                                |> Maybe.map (\nid -> nodeIdCodec data.receiver nid type_)
                                |> withDefault ""
                    in
                    span []
                        [ textH T.about
                        , text T.space_
                        , a
                            [ class "is-discrete-2"
                            , Route.Tension_Dynamic_Dynamic_Action { param1 = nid2rootid nameid, param2 = withDefaultData "" data.data } |> toHref |> href
                            ]
                            [ withDefault "" data.node.name |> text ]
                        ]

                  else
                    span [] [ textH T.about ]
                ]
            , case data.toolbar of
                Just tb ->
                    -- from OverviewBaseUri: show toolbar that is linked to the tension id.
                    div [ class "media-right is-marginless is-small" ] [ tb ]

                Nothing ->
                    div [ class "media-right is-marginless buttonEdit" ] [ editView ]
            ]
        , case data.node.about of
            Just ab ->
                p [ class "column is-fullwidth pt-0 pb-0 is-human" ] [ text ab ]

            Nothing ->
                text ""
        ]


viewMandateSection : Html msg -> OrgaNodeData msg -> Html msg
viewMandateSection editView data =
    div []
        [ div [ class "media subtitle" ]
            [ div [ class "media-left" ]
                [ A.icon "icon-book-open icon-1half" ]
            , div [ class "media-content" ]
                [ textH T.mandate ]
            , div [ class "media-right is-marginless buttonEdit" ] [ editView ]
            ]
        , case data.node.mandate of
            Just mandate ->
                div []
                    [ viewMandateSubSection (upH T.purpose) (Just mandate.purpose)
                    , viewMandateSubSection (upH T.responsabilities) mandate.responsabilities
                    , viewMandateSubSection (upH T.domains) mandate.domains
                    , viewMandateSubSection (upH T.policies) mandate.policies
                    ]

            Nothing ->
                case data.node.role_type of
                    Just RoleType.Guest ->
                        a [ class "is-size-6", href "https://doc.fractale.co/role/guest" ] [ text "https://doc.fractale.co/role/guest" ]

                    _ ->
                        div [ class "is-italic" ]
                            [ text "No description for this node."
                            , editView
                            ]

        --, p [ class "column is-fullwidth" ] []
        ]


viewMandateSubSection : String -> Maybe String -> Html msg
viewMandateSubSection name maybePara =
    case maybePara of
        Just para ->
            div [ class "subSection" ]
                [ div [ class "label" ] [ text name ]
                , p [ class "mt-3" ] [ renderMarkdown "is-human" para ]
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
                    , placeholder (upH T.name)
                    , value (node.name |> withDefault "")
                    , onInput <| op.onChangePost "name"
                    , required True
                    ]
                    []
                , if hasBeenPushed == False then
                    let
                        ref =
                            List.head op.targets |> withDefault "" |> String.replace "#" "/"
                    in
                    div [ class "subForm" ]
                        [ div [ class "field is-horizontal" ]
                            [ div [ class "field-body control" ]
                                [ div [] [ text "URL" ]
                                , input
                                    [ class "input px-2"
                                    , disabled True
                                    , value "https://fractale.co/o/"

                                    -- @debug: show the full url (user ref var) when hoovering
                                    , attribute "style" "width: 11.4em"
                                    ]
                                    []
                                , input
                                    [ class "input"
                                    , type_ "text"
                                    , value (node.nameid |> withDefault "")
                                    , onInput <| op.onChangePost "nameid"
                                    ]
                                    []
                                ]
                            ]
                        ]

                  else
                    text ""
                ]
            , p [ class "help-label" ] [ textH txt.name_help ]
            ]
        , div [ class "field" ]
            [ div [ class "control" ]
                [ input
                    [ id "aboutField"
                    , class "input followFocus"
                    , attribute "data-nextfocus" "textAreaModal"
                    , type_ "text"
                    , placeholder (upH T.aboutOpt)
                    , value (node.about |> withDefault "")
                    , onInput <| op.onChangePost "about"
                    ]
                    []
                ]
            , p [ class "help-label" ] [ textH txt.about_help ]
            , br [] []
            ]
        ]


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
                [ textH T.purpose ]
            , div [ class "control" ]
                [ textarea
                    [ id "textAreaModal"
                    , class "textarea"
                    , rows 5
                    , placeholder (upH txt.ph_purpose)
                    , value purpose
                    , onInput <| op.onChangePost "purpose"
                    , required True
                    ]
                    []
                ]
            ]
        , if showResponsabilities then
            div [ class "field" ]
                [ div [ class "label" ] [ textH T.responsabilities ]
                , div [ class "control" ]
                    [ textarea
                        [ class "textarea"
                        , rows 5
                        , placeholder (upH txt.ph_responsabilities)
                        , value responsabilities
                        , onInput <| op.onChangePost "responsabilities"
                        ]
                        []
                    ]
                ]

          else
            text ""
        , if showDomains then
            div [ class "field" ]
                [ div [ class "label" ] [ textH T.domains ]
                , div [ class "control" ]
                    [ textarea
                        [ class "textarea"
                        , rows 5
                        , placeholder (upH txt.ph_domains)
                        , value domains
                        , onInput <| op.onChangePost "domains"
                        ]
                        []
                    ]
                ]

          else
            text ""
        , if showPolicies then
            div [ class "field" ]
                [ div [ class "label" ] [ textH T.policies ]
                , div [ class "control" ]
                    [ textarea
                        [ class "textarea"
                        , rows 5
                        , placeholder (upH txt.ph_policies)
                        , value policies
                        , onInput <| op.onChangePost "policies"
                        ]
                        []
                    ]
                ]

          else
            text ""
        , if showResponsabilities == False then
            span [ class "pr-2" ]
                [ div [ class "button is-small is-success", onClick op.onAddResponsabilities ]
                    [ A.icon1 "icon-plus" "", textH T.addResponsabilities ]
                ]

          else
            text ""
        , if showDomains == False then
            span [ class "pr-2" ]
                [ div [ class "button is-small is-success", onClick op.onAddDomains ]
                    [ A.icon1 "icon-plus" "", textH T.addDomains ]
                ]

          else
            text ""
        , if showPolicies == False then
            span [ class "pr-2" ]
                [ div [ class "button is-small is-success", onClick op.onAddPolicies ]
                    [ A.icon1 "icon-plus" "", textH T.addPolicies ]
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
                    [ A.icon "icon-edit-2" ]

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
                        [ class "button"
                        , onClick op.onCancelBlob
                        ]
                        [ textH T.cancel ]
                    , button
                        [ class "button is-success"
                        , classList [ ( "is-loading", isLoading ) ]
                        , disabled (not isSendable)
                        , onClick (op.onSubmit <| op.onSubmitBlob op.data)
                        ]
                        [ textH T.saveChanges ]
                    ]
                ]
            ]
        ]



-- Versions view


viewVersions : Time.Posix -> GqlData TensionBlobs -> Html msg
viewVersions now blobsData =
    case blobsData of
        Success tblobs ->
            let
                headers =
                    []
            in
            table [ class "table is-fullwidth" ]
                [ thead []
                    [ tr [] (headers |> List.map (\x -> th [] [ textH x ]))
                    ]
                , tblobs.blobs
                    |> withDefault []
                    |> List.indexedMap (\i d -> viewVerRow now i d)
                    |> List.concat
                    |> tbody []
                ]

        Failure err ->
            viewGqlErrors err

        LoadingSlowly ->
            div [ class "spinner" ] []

        _ ->
            text ""


viewVerRow : Time.Posix -> Int -> Blob -> List (Html msg)
viewVerRow now i blob =
    [ tr [ class "mediaBox is-hoverable", classList [ ( "is-active", i == 0 ) ] ]
        [ td [] [ span [] [ text (blobTypeStr blob.blob_type) ], text T.space_, byAt now blob.createdBy blob.createdAt ]
        , td []
            [ case blob.pushedFlag of
                Just flag ->
                    div
                        [ class "tooltip has-tooltip-arrow"
                        , attribute "style" "cursor: inherit;"
                        , attribute "data-tooltip" (upH T.published ++ " " ++ formatDate now flag)
                        ]
                        [ A.icon "icon-flag" ]

                Nothing ->
                    text ""
            ]
        ]
    ]



--- Utils


nodeFragmentFromOrga : Maybe Node -> GqlData NodeData -> List EmitterOrReceiver -> NodesDict -> NodeFragment
nodeFragmentFromOrga node_m nodeData children_eo ndata =
    let
        children =
            children_eo
                |> List.map (\n -> Dict.get n.nameid ndata)
                |> List.filterMap identity
                |> List.filter (\n -> n.role_type == Just RoleType.Coordinator)
                |> List.map node2SubNodeFragment
                |> Just
    in
    node2NodeFragment node_m children (withMaybeData nodeData)


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
            { form | node = { node | nameid = Just (makeNewNodeId value) } }

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
                                ternary (value == "")
                                    (Dict.insert "title" value form.post)
                                    (Dict.insert "title" ("[" ++ actionNameStr action ++ "] " ++ value) form.post)

                            newData =
                                { node
                                    | name = Just value
                                    , nameid = Just (makeNewNodeId value)
                                }
                        in
                        { form | post = newPost, node = newData }

                    else
                        { form | node = { node | name = Just value } }

        _ ->
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
        |> cleanDup "-"
        |> cleanDup "_"



-- User Lookup utilities


updateUserPattern_ : Int -> String -> List UserForm -> List UserForm
updateUserPattern_ pos pattern users =
    LE.updateAt pos (\x -> { x | pattern = pattern }) users


selectUser_ : Int -> String -> List UserForm -> List UserForm
selectUser_ pos username users =
    LE.updateAt pos (\x -> { x | username = username, pattern = "" }) users


cancelUser_ : Int -> List UserForm -> List UserForm
cancelUser_ pos users =
    --LE.removeAt pos users
    LE.updateAt pos (\x -> { x | username = "" }) users
