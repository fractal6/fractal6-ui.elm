module Components.NodeDoc exposing (..)

import Assets as A
import Components.Loading as Loading exposing (GqlData, RequestResult(..), isFailure, isSuccess, loadingSpin, viewGqlErrors, withDefaultData, withMaybeData)
import Dict
import Extra exposing (ternary)
import Extra.Date exposing (formatDate)
import Fractal.Enum.BlobType as BlobType
import Fractal.Enum.NodeMode as NodeMode
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.NodeVisibility as NodeVisibility
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Generated.Route as Route exposing (Route, toHref)
import Html exposing (Html, a, br, button, canvas, datalist, div, h1, h2, hr, i, input, label, li, nav, option, p, select, span, table, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, list, name, placeholder, required, rows, selected, size, spellcheck, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseEnter)
import Html.Lazy as Lazy
import List.Extra as LE
import Markdown exposing (renderMarkdown)
import Maybe exposing (withDefault)
import ModelCommon exposing (Ev, TensionForm, UserForm, UserState(..), initTensionForm)
import ModelCommon.Codecs exposing (ActionType(..), FractalBaseRoute(..), NodeFocus, isBaseMember, isTensionBaseUri, nameidEncoder, nid2rootid, nodeIdCodec, uriFromNameid, uriFromUsername)
import ModelCommon.View exposing (FormText, action2str, blobTypeStr, byAt, getNodeTextFromNodeType, roleColor, viewUser)
import ModelSchema exposing (..)
import String.Extra as SE
import Text as T exposing (textH, textT, upH)
import Time



{- NodeDoc

   Shared object for view and input around a Node object (and NodeFragment),
   and controlled by a tension source (TensionForm).
   Viewing and editing node name, mandate and changing a role authority or circle
   governance for examples.
-}


type alias NodeDoc =
    { form : TensionForm
    , result : GqlData PatchTensionPayloadID
    , mode : NodeView
    , doAddResponsabilities : Bool
    , doAddDomains : Bool
    , doAddPolicies : Bool
    }


type NodeView
    = NodeView
    | NodeEdit
    | NodeVersions
    | NoView


init : String -> NodeView -> UserState -> NodeDoc
init tid mode user =
    { form = initTensionForm tid user
    , result = NotAsked
    , mode = mode
    , doAddResponsabilities = False
    , doAddDomains = False
    , doAddPolicies = False
    }


initBlob : NodeFragment -> NodeDoc -> NodeDoc
initBlob nf data =
    let
        form =
            data.form
    in
    { data | form = { form | node = nf }, result = NotAsked }


nodeViewEncoder : NodeView -> String
nodeViewEncoder x =
    case x of
        NodeView ->
            ""

        NodeEdit ->
            "edit"

        NodeVersions ->
            "history"

        NoView ->
            "noview"


nodeViewDecoder : String -> NodeView
nodeViewDecoder x =
    case x of
        "edit" ->
            NodeEdit

        "history" ->
            NodeVersions

        "noview" ->
            NoView

        _ ->
            NodeView



-- Global method


getNodeView : NodeDoc -> NodeView
getNodeView data =
    data.mode



-- State Controls


changeMode : NodeView -> NodeDoc -> NodeDoc
changeMode mode data =
    { data | mode = mode }


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


setForm : TensionForm -> NodeDoc -> NodeDoc
setForm form data =
    { data | form = form }


setUctx : UserCtx -> NodeDoc -> NodeDoc
setUctx uctx data =
    let
        form =
            data.form
    in
    { data | form = { form | uctx = uctx } }


setId : String -> NodeDoc -> NodeDoc
setId tid data =
    let
        form =
            data.form
    in
    { data | form = { form | id = tid } }


setTensionType : TensionType.TensionType -> NodeDoc -> NodeDoc
setTensionType type_ data =
    let
        f =
            data.form
    in
    { data | form = { f | type_ = Just type_ } }


setSource : UserRole -> NodeDoc -> NodeDoc
setSource source data =
    let
        f =
            data.form
    in
    { data | form = { f | source = source } }


setTarget : PNode -> NodeDoc -> NodeDoc
setTarget target data =
    let
        f =
            data.form
    in
    { data | form = { f | target = target } }


setSourceShort : String -> NodeDoc -> NodeDoc
setSourceShort nameid data =
    let
        f =
            data.form

        newForm =
            -- only nameid is used
            { f | source = { nameid = nameid, name = "", role_type = RoleType.Bot } }
    in
    { data | form = newForm }


setTargetShort : String -> NodeDoc -> NodeDoc
setTargetShort nameid data =
    let
        f =
            data.form

        newForm =
            -- only nameid is used
            { f | target = initPNode |> (\x -> { x | nameid = nameid }) }
    in
    { data | form = newForm }


setStatus : TensionStatus.TensionStatus -> NodeDoc -> NodeDoc
setStatus status data =
    let
        f =
            data.form
    in
    { data | form = { f | status = Just status } }


setUsers : List UserForm -> NodeDoc -> NodeDoc
setUsers users data =
    let
        f =
            data.form
    in
    { data | form = { f | users = users } }


setEvents : List Ev -> NodeDoc -> NodeDoc
setEvents events data =
    let
        f =
            data.form
    in
    { data | form = { f | events = events } }


setLabels : List Label -> NodeDoc -> NodeDoc
setLabels labels data =
    let
        f =
            data.form
    in
    { data | form = { f | labels = labels } }


addLabel : Label -> NodeDoc -> NodeDoc
addLabel label data =
    let
        f =
            data.form
    in
    { data | form = { f | labels = f.labels ++ [ label ] } }


removeLabel : Label -> NodeDoc -> NodeDoc
removeLabel label data =
    let
        f =
            data.form
    in
    { data | form = { f | labels = LE.remove label f.labels } }


resetPost : NodeDoc -> NodeDoc
resetPost data =
    let
        form =
            data.form
    in
    { data | form = { form | post = Dict.empty } }


resetNode : NodeDoc -> NodeDoc
resetNode data =
    let
        form =
            data.form
    in
    { data | form = { form | node = initNodeFragment Nothing } }


reset : NodeDoc -> NodeDoc
reset data =
    data |> initBlob data.form.node



-- Getters


getMandate : NodeDoc -> Mandate
getMandate data =
    data.form.node.mandate |> withDefault initMandate


hasMandate : Maybe Mandate -> Bool
hasMandate mandate_m =
    let
        mandate =
            --data.form.node.mandate
            withDefault initMandate mandate_m
    in
    mandate.purpose
        /= ""
        || withDefault "" mandate.responsabilities
        /= ""
        || withDefault "" mandate.domains
        /= ""
        || withDefault "" mandate.policies
        /= ""



-- Update Form


updatePost : String -> String -> NodeDoc -> NodeDoc
updatePost field value data =
    { data | form = updateNodeForm field value data.form }


updatePost2 : String -> Maybe String -> NodeDoc -> NodeDoc
updatePost2 field value_m data =
    case value_m of
        Just value ->
            { data | form = updateNodeForm field value data.form }

        Nothing ->
            data


updateFromRoleExt : RoleExtFull -> NodeDoc -> NodeDoc
updateFromRoleExt role data =
    data
        |> updatePost "name" role.name
        -- rewite nameid by appending the #number of this role used
        |> (\x -> updatePost2 "nameid" (Maybe.map (\nameid -> nameid ++ "-" ++ String.fromInt (withDefault 0 role.n_roles)) x.form.node.nameid) x)
        |> updatePost "role_type" (RoleType.toString role.role_type)
        |> updatePost "role_ext" role.id
        |> updatePost2 "about" role.about
        |> updatePost2 "color" role.color
        |> (\x ->
                let
                    form =
                        x.form

                    node =
                        form.node
                in
                { x | form = { form | node = { node | mandate = role.mandate } } }
           )



--


type alias OrgaNodeData =
    -- should be merge with Op in the @future Model Components
    { focus : NodeFocus
    , tid_r : GqlData String
    , node : NodeFragment
    , isLazy : Bool
    , source : FractalBaseRoute
    , hasBeenPushed : Bool
    , receiver : String
    , hasInnerToolbar : Bool
    }


type alias Op msg =
    { data : NodeDoc
    , result : GqlData Tension -- result from new tension components
    , now : Time.Posix
    , publish_result : GqlData BlobFlag
    , blob : Blob
    , isAdmin : Bool
    , tension_blobs : GqlData TensionBlobs

    -- Blob control
    , onSubmit : (Time.Posix -> msg) -> msg
    , onSubmitBlob : NodeDoc -> Time.Posix -> msg
    , onCancelBlob : msg
    , onPushBlob : String -> Time.Posix -> msg

    -- Blob change
    , onChangePost : String -> String -> msg
    , onAddResponsabilities : msg
    , onAddDomains : msg
    , onAddPolicies : msg
    }


view : OrgaNodeData -> Maybe (Op msg) -> Html msg
view data op_m =
    Lazy.lazy2 view_ data op_m


view_ : OrgaNodeData -> Maybe (Op msg) -> Html msg
view_ data op_m =
    case data.tid_r of
        Success tid ->
            div []
                [ if not data.hasInnerToolbar then
                    case op_m of
                        Just op ->
                            div [ class "mb-4" ]
                                [ div [ class "level" ]
                                    [ div [ class "level-left" ]
                                        [ viewToolbar op.data.mode data ]
                                    , div [ class "level-right" ]
                                        [ viewNodeStatus op ]
                                    ]
                                , case op.publish_result of
                                    Failure err ->
                                        viewGqlErrors err

                                    _ ->
                                        text ""
                                ]

                        Nothing ->
                            text ""

                  else
                    text ""
                , div [ id "DocContainer", class "box" ]
                    [ viewBlob data op_m ]
                ]

        Failure err ->
            viewGqlErrors err

        LoadingSlowly ->
            div [ class "spinner" ] []

        _ ->
            text ""


viewToolbar : NodeView -> OrgaNodeData -> Html msg
viewToolbar mode data =
    let
        tid =
            withDefaultData "" data.tid_r

        iconOpts =
            ternary data.hasInnerToolbar "icon-xs" ""
    in
    div [ class "field has-addons docToolbar" ]
        [ p
            [ class "control tooltip has-tooltip-arrow"
            , attribute "data-tooltip" (upH T.view)
            ]
            [ a
                [ class "button is-small is-rounded is-discrete"
                , classList [ ( "is-active", mode == NodeView ) ]
                , href
                    (Route.Tension_Dynamic_Dynamic_Action { param1 = data.focus.rootnameid, param2 = tid } |> toHref)
                ]
                [ A.icon ("icon-eye " ++ iconOpts) ]
            ]
        , p
            [ class "control tooltip has-tooltip-arrow"
            , attribute "data-tooltip" (upH T.edit)
            ]
            [ a
                [ class "button is-small is-rounded  is-discrete"
                , classList [ ( "is-active", mode == NodeEdit ) ]
                , href
                    ((Route.Tension_Dynamic_Dynamic_Action { param1 = data.focus.rootnameid, param2 = tid } |> toHref) ++ "?v=edit")
                ]
                [ A.icon ("icon-edit-2 " ++ iconOpts) ]
            ]
        , p
            [ class "control tooltip has-tooltip-arrow"
            , attribute "data-tooltip" (upH T.revisions)
            ]
            [ a
                [ class "button is-small is-rounded  is-discrete"
                , classList [ ( "is-active", mode == NodeVersions ) ]
                , href
                    ((Route.Tension_Dynamic_Dynamic_Action { param1 = data.focus.rootnameid, param2 = tid } |> toHref) ++ "?v=history")
                ]
                [ A.icon ("icon-history " ++ iconOpts) ]
            ]
        ]


viewNodeStatus : Op msg -> Html msg
viewNodeStatus op =
    case op.blob.pushedFlag of
        Just flag ->
            div [ class "has-text-success is-italic" ]
                [ textH (T.published ++ " " ++ formatDate op.now flag) ]

        Nothing ->
            let
                isLoading =
                    op.publish_result == LoadingSlowly
            in
            div [ class "field has-addons" ]
                [ div [ class "has-text-warning is-italic mr-3" ]
                    [ textH T.revisionNotPublished ]
                , if op.isAdmin then
                    div
                        [ class "button is-small is-success has-text-weight-semibold"
                        , onClick (op.onSubmit <| op.onPushBlob op.blob.id)
                        ]
                        [ A.icon1 "icon-share" (upH T.publish)
                        , loadingSpin isLoading
                        ]

                  else
                    text ""
                ]


viewBlob : OrgaNodeData -> Maybe (Op msg) -> Html msg
viewBlob data op_m =
    case op_m of
        Just op ->
            case op.data.mode of
                NodeView ->
                    div [ classList [ ( "is-lazy", data.isLazy ) ] ]
                        [ viewAboutSection data
                        , hr [ class "has-background-border-light" ] []
                        , viewMandateSection data.node.mandate data.node.role_type
                        ]

                NodeEdit ->
                    let
                        txt =
                            getNodeTextFromNodeType (withDefault NodeType.Role data.node.type_)

                        isLoading =
                            op.data.result == LoadingSlowly

                        isSendable1 =
                            data.node.name /= op.data.form.node.name || data.node.about /= op.data.form.node.about

                        isSendable2 =
                            data.node.mandate /= op.data.form.node.mandate
                    in
                    div []
                        [ viewAboutInput data.hasBeenPushed data.source txt op.data.form.node op
                        , viewBlobButtons isSendable1 isLoading op
                        , hr [ class "has-background-border-light" ] []
                        , viewMandateInput txt op.data.form.node.mandate op
                        , viewBlobButtons isSendable2 isLoading op
                        ]

                NodeVersions ->
                    viewVersions op.now op.tension_blobs

                NoView ->
                    text ""

        Nothing ->
            div [ classList [ ( "is-lazy", data.isLazy ) ] ]
                [ viewAboutSection data
                , hr [ class "has-background-border-light" ] []
                , viewMandateSection data.node.mandate data.node.role_type
                ]



--- Template view


viewAboutSection : OrgaNodeData -> Html msg
viewAboutSection data =
    let
        type_ =
            withDefault NodeType.Role data.node.type_

        nameid =
            data.node.nameid
                |> Maybe.map (\nid -> nodeIdCodec data.receiver nid type_)
                |> withDefault ""
    in
    div []
        [ div [ class "level subtitle" ]
            [ div [ class "level-left", attribute "style" "width:85%;" ]
                [ A.icon "icon-info icon-lg mr-2"
                , span [ class "nowrap" ] [ textH T.about ]
                , text T.space_
                , if isTensionBaseUri data.source && data.hasBeenPushed then
                    a
                        [ href <| uriFromNameid OverviewBaseUri nameid [] ]
                        [ text <| withDefault "" data.node.name ]

                  else if data.source == OverviewBaseUri && not (isBaseMember nameid) then
                    a
                        [ href <| toHref <| Route.Tension_Dynamic_Dynamic_Action { param1 = nid2rootid nameid, param2 = withDefaultData "" data.tid_r } ]
                        [ text <| withDefault "" data.node.name ]

                  else
                    span [ class "is-name" ] [ withDefault "" data.node.name |> text ]
                ]
            , if data.hasInnerToolbar && isSuccess data.tid_r then
                div [ class "level-right is-marginless is-small" ] [ viewToolbar NoView data ]

              else
                text ""
            ]
        , case data.node.about of
            Just ab ->
                p [ class "is-human" ] [ text ab ]

            Nothing ->
                text ""
        ]


viewMandateSection : Maybe Mandate -> Maybe RoleType.RoleType -> Html msg
viewMandateSection mandate_m role_type_m =
    div []
        [ div [ class "level subtitle" ]
            [ div [ class "level-left" ]
                [ A.icon "icon-book-open icon-lg mr-2"
                , textH T.mandate
                ]
            ]
        , case mandate_m of
            Just mandate ->
                div []
                    [ viewMandateSubSection (upH T.purpose) (Just mandate.purpose)
                    , viewMandateSubSection (upH T.responsabilities) mandate.responsabilities
                    , viewMandateSubSection (upH T.domains) mandate.domains
                    , viewMandateSubSection (upH T.policies) mandate.policies
                    ]

            Nothing ->
                case role_type_m of
                    Just role_type ->
                        let
                            rt =
                                RoleType.toString role_type |> String.toLower

                            md =
                                "https://doc.fractale.co/circle/#" ++ rt
                        in
                        renderMarkdown "is-human" md

                    Nothing ->
                        div [ class "is-italic" ] [ text "No description for this node." ]

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


viewAboutInput hasBeenPushed source txt node op =
    div [ class "pb-0" ]
        [ div [ class "field" ]
            [ label [ class "label" ] [ textH T.name ]
            , div [ class "control" ]
                [ input
                    [ class "input autofocus followFocus"
                    , attribute "data-nextfocus" "aboutField"
                    , type_ "text"
                    , placeholder (upH T.name)
                    , value (withDefault "" node.name)
                    , onInput <| op.onChangePost "name"
                    , required True
                    ]
                    []
                , if (isTensionBaseUri source && hasBeenPushed == False) || (source == OverviewBaseUri && isFailure op.result) then
                    viewUrlForm node.nameid (op.onChangePost "nameid") False

                  else
                    text ""
                ]
            , p [ class "help-label" ] [ textH txt.name_help ]
            ]
        , div [ class "field" ]
            [ label [ class "label" ] [ textH T.about ]
            , div [ class "control" ]
                [ input
                    [ id "aboutField"
                    , class "input followFocus"
                    , attribute "data-nextfocus" "textAreaModal"
                    , type_ "text"
                    , placeholder (upH T.aboutOpt)
                    , spellcheck True
                    , value (withDefault "" node.about)
                    , onInput <| op.onChangePost "about"
                    ]
                    []
                ]
            , p [ class "help-label" ] [ textH txt.about_help ]
            , br [] []
            ]
        ]


viewMandateInput txt mandate op =
    let
        purpose =
            mandate |> Maybe.map (\m -> m.purpose) |> withDefault ""

        responsabilities =
            mandate |> Maybe.map (\m -> withDefault "" m.responsabilities) |> withDefault ""

        domains =
            mandate |> Maybe.map (\m -> withDefault "" m.domains) |> withDefault ""

        policies =
            mandate |> Maybe.map (\m -> withDefault "" m.policies) |> withDefault ""

        showResponsabilities =
            op.data.doAddResponsabilities || responsabilities /= ""

        showDomains =
            op.data.doAddDomains || domains /= ""

        showPolicies =
            op.data.doAddPolicies || policies /= ""

        purpose_len =
            List.length <| String.lines purpose
    in
    div [ class "pb-0" ]
        [ div [ class "field" ]
            [ div [ class "label" ]
                [ textH T.purpose ]
            , div [ class "control" ]
                [ textarea
                    [ id "textAreaModal"
                    , class "textarea"
                    , rows (min 15 (max purpose_len 5))
                    , placeholder (upH txt.ph_purpose)
                    , value purpose
                    , onInput <| op.onChangePost "purpose"
                    , required True
                    ]
                    []
                ]
            ]
        , if showResponsabilities then
            let
                responsabilities_len =
                    List.length <| String.lines purpose
            in
            div [ class "field" ]
                [ div [ class "label" ] [ textH T.responsabilities ]
                , div [ class "control" ]
                    [ textarea
                        [ class "textarea"
                        , rows (min 15 (max responsabilities_len 5))
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
                [ div [ class "button is-small", onClick op.onAddResponsabilities ]
                    [ A.icon1 "icon-plus" "", textH T.addResponsabilities ]
                ]

          else
            text ""
        , if showDomains == False then
            span [ class "pr-2" ]
                [ div [ class "button is-small", onClick op.onAddDomains ]
                    [ A.icon1 "icon-plus" "", textH T.addDomains ]
                ]

          else
            text ""
        , if showPolicies == False then
            span [ class "pr-2" ]
                [ div [ class "button is-small", onClick op.onAddPolicies ]
                    [ A.icon1 "icon-plus" "", textH T.addPolicies ]
                ]

          else
            text ""
        ]



---- Components view


viewBlobButtons : Bool -> Bool -> Op msg -> Html msg
viewBlobButtons isSendable isLoading op =
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


type alias OpAuthority msg =
    { onSelect : RoleType.RoleType -> msg
    , selection : RoleType.RoleType
    }


viewSelectAuthority : String -> OpAuthority msg -> Html msg
viewSelectAuthority position op =
    let
        checked cls =
            A.icon1 ("icon-check " ++ cls) ""

        unchecked =
            A.icon1 "icon-check is-invisible" ""
    in
    div [ class "field" ]
        [ div [ class ("dropdown " ++ position) ]
            [ div [ class "button dropdown-trigger", attribute "aria-controls" "select-authority" ]
                [ span [ class ("has-text-" ++ roleColor op.selection) ] [ textH (RoleType.toString op.selection) ], i [ class "ml-3 icon-chevron-down1 icon-tiny" ] [] ]
            , div [ id "select-authority", class "dropdown-menu", attribute "role" "menu" ]
                [ div [ class "dropdown-content is-right" ] <|
                    List.map
                        (\role_type ->
                            let
                                clsColor =
                                    "has-text-" ++ roleColor role_type
                            in
                            div
                                [ class ("dropdown-item button-light " ++ clsColor)
                                , onClick <| op.onSelect role_type
                                ]
                                [ ternary (op.selection == role_type) (checked clsColor) unchecked
                                , textH (RoleType.toString role_type)

                                --, span [ class "is-pulled-right mx-2 is-small tooltip" ] [ A.icon "icon-info" ]
                                ]
                        )
                        [ RoleType.Peer, RoleType.Coordinator ]
                ]
            ]
        ]


type alias OpGovernance msg =
    { onSelect : NodeMode.NodeMode -> msg
    , selection : NodeMode.NodeMode
    }


viewSelectGovernance : String -> OpGovernance msg -> Html msg
viewSelectGovernance position op =
    let
        checked cls =
            A.icon1 ("icon-check " ++ cls) ""

        unchecked =
            A.icon1 "icon-check is-invisible" ""
    in
    div [ class "field" ]
        [ div [ class ("dropdown " ++ position) ]
            [ div [ class "button dropdown-trigger", attribute "aria-controls" "select-governance" ]
                [ span [ class "has-text-" ] [ textH (NodeMode.toString op.selection) ], i [ class "ml-3 icon-chevron-down1 icon-tiny" ] [] ]
            , div [ id "select-governance", class "dropdown-menu", attribute "role" "menu" ]
                [ div [ class "dropdown-content is-right" ] <|
                    List.map
                        (\mode ->
                            div [ class "dropdown-item button-light ", onClick <| op.onSelect mode ]
                                [ ternary (op.selection == mode) (checked "") unchecked, textH (NodeMode.toString mode) ]
                        )
                        NodeMode.list
                ]
            ]
        ]



-- Versions view


viewVersions : Time.Posix -> GqlData TensionBlobs -> Html msg
viewVersions now blobsData =
    Lazy.lazy2 viewVersions_ now blobsData


viewVersions_ : Time.Posix -> GqlData TensionBlobs -> Html msg
viewVersions_ now blobsData =
    case blobsData of
        Success tblobs ->
            let
                headers =
                    []
            in
            div [ class "table-containe" ]
                -- @debug table-container with width=100%, do not work!
                [ table [ class "table is-fullwidth table-container" ]
                    [ thead []
                        [ tr [] (headers |> List.map (\x -> th [] [ textH x ]))
                        ]
                    , tblobs.blobs
                        |> withDefault []
                        |> List.indexedMap (\i d -> viewVerRow now i d)
                        |> List.concat
                        |> tbody []
                    ]
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


updateNodeForm : String -> String -> TensionForm -> TensionForm
updateNodeForm field value form =
    let
        node =
            form.node

        mandate =
            withDefault initMandate node.mandate
    in
    case field of
        -- Mandate
        "purpose" ->
            { form | node = { node | mandate = Just { mandate | purpose = value } } }

        "responsabilities" ->
            { form | node = { node | mandate = Just { mandate | responsabilities = ternary (value == "") Nothing (Just value) } } }

        "domains" ->
            { form | node = { node | mandate = Just { mandate | domains = ternary (value == "") Nothing (Just value) } } }

        "policies" ->
            { form | node = { node | mandate = Just { mandate | policies = ternary (value == "") Nothing (Just value) } } }

        -- NodeFragment
        "nameid" ->
            { form | node = { node | nameid = Just (nameidEncoder value) } }

        "about" ->
            { form | node = { node | about = Just value } }

        "role_type" ->
            { form | node = { node | role_type = RoleType.fromString value } }

        "role_ext" ->
            { form | node = { node | role_ext = Just value } }

        "color" ->
            { form | node = { node | color = Just value } }

        "visibility" ->
            { form | node = { node | visibility = NodeVisibility.fromString value } }

        "mode" ->
            { form | node = { node | mode = NodeMode.fromString value } }

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
                                    (Dict.insert "title" ("[" ++ action2str action ++ "] " ++ value) form.post)

                            newData =
                                { node
                                    | name = Just value
                                    , nameid = Just (nameidEncoder value)
                                }
                        in
                        { form | post = newPost, node = newData }

                    else
                        { form | node = { node | name = Just value } }

        _ ->
            -- title, message...
            { form | post = Dict.insert field value form.post }


viewUrlForm nameid_m onChangePost hasBorderDanger =
    div [ class "urlForm" ]
        [ div [ class "field is-horizontal" ]
            [ div [ class "field-body control" ]
                [ div [] [ text "URL" ]
                , input
                    [ class "input px-0"
                    , disabled True
                    , value " https://fractale.co/o/"
                    , attribute "style" "width: 11.1em"
                    ]
                    []
                , input
                    [ class "input pl-1"
                    , classList [ ( "has-border-danger", hasBorderDanger ) ]
                    , type_ "text"
                    , value (withDefault "" nameid_m)
                    , onInput <| onChangePost
                    ]
                    []
                ]
            ]
        ]
