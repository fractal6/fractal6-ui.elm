{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2026 Fractale Co

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


module Components.NodeDoc exposing (..)

import Assets as A
import Components.Comments exposing (viewCommentInputHeader)
import Components.UserSearchPanel exposing (viewUser, viewUsers)
import Dict exposing (Dict)
import Fractale.Codecs exposing (FractalBaseRoute(..), NodeFocus, nameidEncoder)
import Fractale.Error exposing (viewGqlErrors)
import Fractale.Form exposing (Ev, FormText, InputViewMode(..), TensionForm, UserForm, initFormText, initTensionForm)
import Fractale.User exposing (UserState(..))
import Fractale.View exposing (byAt, helperButton, viewNodeDescr, viewUrlForm)
import Generated.Route as Route exposing (toHref)
import Html exposing (Html, a, br, button, div, hr, i, input, label, p, span, strong, table, tbody, td, text, textarea, th, thead, tr)
import Html.Attributes exposing (attribute, class, classList, colspan, disabled, href, id, name, placeholder, required, rows, spellcheck, style, title, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Lazy as Lazy
import List.Extra as LE
import Loading exposing (GqlData, RequestResult(..), isFailure, isSuccess, loadingSpin, withDefaultData)
import Markdown exposing (renderMarkdown)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Schema.Enum.NodeMode as NodeMode
import Schema.Enum.NodeType as NodeType
import Schema.Enum.NodeVisibility as NodeVisibility
import Schema.Enum.RoleType as RoleType
import Schema.Enum.TensionStatus as TensionStatus
import Schema.Enum.TensionType as TensionType
import Session exposing (SessionCommon)
import Set
import String.Format as Format
import Text as T
import Time
import Utils.Bool exposing (ternary)
import Utils.Date exposing (formatDate)
import Utils.Html exposing (showIf, showMaybe)
import Utils.Diff as Diff
import Utils.Maybe exposing (unwrap)
import Utils.String exposing (space_)



{- NodeDoc

   Shared object for view and input around a Node object (and NodeFragment),
   and controlled by a tension source (TensionForm).
   Viewing and editing node name, mandate and changing a role authority or circle
   governance for examples.
-}


type alias NodeDoc =
    { node : NodeFragment --redundant with data.node ? Use to create tension / blob
    , form : TensionForm
    , result : GqlData PatchTensionPayloadID
    , mode : NodeView
    , editMode : Maybe NodeEdit
    , doAddResponsabilities : Bool
    , doAddDomains : Bool
    , doAddPolicies : Bool
    }


type NodeView
    = NodeEdit
    | NodeVersions
    | NoView


type NodeEdit
    = EditAbout
    | EditMandate


init : Dict.Dict String String -> String -> Maybe NodeType.NodeType -> NodeView -> UserState -> NodeDoc
init lexicon tid node_type mode user =
    { node = initNodeFragment Nothing
    , form = initTensionForm lexicon tid node_type user
    , result = NotAsked
    , editMode = Nothing
    , mode = mode
    , doAddResponsabilities = False
    , doAddDomains = False
    , doAddPolicies = False
    }


initBlob : Dict.Dict String String -> NodeFragment -> NodeDoc -> NodeDoc
initBlob lexicon nf data =
    let
        form =
            data.form
    in
    { data
        | node = nf
        , form = { form | node = nf, txt = initFormText lexicon nf.type_ }
        , result = NotAsked
    }


nodeViewEncoder : NodeView -> String
nodeViewEncoder x =
    case x of
        NodeEdit ->
            ""

        NodeVersions ->
            "history"

        NoView ->
            "noview"


nodeViewDecoder : String -> NodeView
nodeViewDecoder x =
    case x of
        "history" ->
            NodeVersions

        "noview" ->
            NoView

        _ ->
            NodeEdit



-- Global method


getNodeView : NodeDoc -> NodeView
getNodeView data =
    data.mode


getMandate : NodeDoc -> Mandate
getMandate data =
    data.form.node.mandate |> withDefault initMandate


getRoleType : NodeDoc -> Maybe RoleType.RoleType
getRoleType data =
    data.form.node.role_type


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



-- State Controls


setNodeEdit : Maybe NodeEdit -> NodeDoc -> NodeDoc
setNodeEdit value data =
    { data | editMode = value }


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
    { data | form = { form | node = data.node } }


reset : NodeDoc -> NodeDoc
reset data =
    data |> resetNode |> resetPost |> setNodeEdit Nothing



-- Update Form


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


setTensionType : TensionType.TensionType -> NodeDoc -> NodeDoc
setTensionType type_ data =
    let
        f =
            data.form
    in
    { data | form = { f | type_ = Just type_ } }


setSource : EmitterOrReceiver -> NodeDoc -> NodeDoc
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
            { f | source = { nameid = nameid, name = "", role_type = Nothing, color = Nothing } }
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


setAssignees : List User -> NodeDoc -> NodeDoc
setAssignees assignees data =
    let
        f =
            data.form
    in
    { data | form = { f | assignees = assignees } }


addAssignee : User -> NodeDoc -> NodeDoc
addAssignee assignee data =
    let
        f =
            data.form
    in
    { data | form = { f | assignees = f.assignees ++ [ assignee ] } }


removeAssignee : User -> NodeDoc -> NodeDoc
removeAssignee assignee data =
    let
        f =
            data.form
    in
    { data | form = { f | assignees = LE.remove assignee f.assignees } }



--


type alias OrgaNodeData =
    -- should be merge with Op in the @future Model Components
    { focus : NodeFocus
    , tid_r : GqlData String

    -- The node is given by another components...
    , node : Maybe Node
    , node_data : NodeData
    , leads : List User

    --
    , session : SessionCommon
    , isLazy : Bool
    , source : FractalBaseRoute
    , hasBeenPushed : Bool
    , hasInnerToolbar : Bool
    , isAdmin : Bool
    }


type alias Op msg =
    { session : SessionCommon
    , data : NodeDoc
    , result : GqlData Tension -- result from new tension components
    , publish_result : GqlData TensionBlobFlag
    , blob : Blob
    , tension_blobs : GqlData TensionBlobs

    -- Blob control
    , expandedDiff : String -- blob id whose diff is expanded in the revisions view ("" = none)
    , onToggleDiff : String -> msg
    , onSubmit : Bool -> (Time.Posix -> msg) -> msg
    , onSubmitBlob : NodeDoc -> Time.Posix -> msg
    , onCancelBlob : msg
    , onPushBlob : String -> Time.Posix -> msg

    -- Blob change
    , onChangeEdit : NodeEdit -> msg
    , onChangePost : String -> String -> msg
    , onAddResponsabilities : msg
    , onAddDomains : msg
    , onAddPolicies : msg
    , mdOps :
        Maybe
            { onChangeViewMode : String -> InputViewMode -> msg
            , onRichText : String -> String -> msg
            , onToggleMdHelp : String -> msg
            , post : Dict String String
            }
    }


view : OrgaNodeData -> Maybe (Op msg) -> Html msg
view data op_m =
    Lazy.lazy2 view_ data op_m


view_ : OrgaNodeData -> Maybe (Op msg) -> Html msg
view_ data op_m =
    case data.tid_r of
        Success _ ->
            div [ id "blobDocument" ]
                [ if not data.hasInnerToolbar then
                    case op_m of
                        Just op ->
                            div [ class "mb-4" ]
                                [ div [ class "level" ]
                                    [ div [ class "level-right" ]
                                        [ viewNodeStatus data.isAdmin op ]
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
                , viewBlob data op_m
                ]

        Failure err ->
            viewGqlErrors err

        LoadingSlowly ->
            div [ class "spinner" ] []

        _ ->
            text ""


viewToolbarDropdown : NodeView -> OrgaNodeData -> Html msg
viewToolbarDropdown mode data =
    let
        tid =
            withDefaultData "" data.tid_r

        iconOpts =
            ternary data.hasInnerToolbar "icon-xs" ""
    in
    div [ class "dropdown is-right has-text-weight-normal" ]
        [ div [ class "dropdown-trigger" ]
            [ div
                [ class "ellipsis button-light"
                , attribute "aria-controls" "edit-ellipsis-card"
                , attribute "aria-haspopup" "true"
                ]
                [ A.icon "icon-more-vertical icon-lg" ]
            ]
        , div [ id "edit-ellipsis-card", class "dropdown-menu", attribute "role" "menu" ]
            [ div [ class "dropdown-content p-0" ] <|
                [ a
                    [ class "dropdown-item stealth-link"

                    --, classList [ ( "is-active", mode == NodeEdit ) ]
                    , href
                        (Route.Tension_Dynamic_Dynamic_Action { param1 = data.focus.rootnameid, param2 = tid } |> toHref)
                    ]
                    [ A.icon1 ("icon-edit-2 " ++ iconOpts) T.edit ]
                , hr [ class "dropdown-divider" ] []
                , a
                    [ class "dropdown-item stealth-link"

                    --, classList [ ( "is-active", mode == NodeVersions ) ]
                    , href
                        ((Route.Tension_Dynamic_Dynamic_Action { param1 = data.focus.rootnameid, param2 = tid } |> toHref) ++ "?v=history")
                    ]
                    [ A.icon1 ("icon-history " ++ iconOpts) T.revisions ]
                ]
            ]
        ]


viewNodeStatus : Bool -> Op msg -> Html msg
viewNodeStatus isAdmin op =
    case op.blob.pushedFlag of
        Just flag ->
            div [ class "has-text-success is-italic" ]
                [ text (T.published ++ " " ++ formatDate op.session.lang op.session.now flag) ]

        Nothing ->
            div [ class "field has-addons" ]
                [ div [ class "has-text-warning is-italic mr-3" ]
                    [ text T.revisionNotPublished ]
                , if isAdmin then
                    let
                        isLoading =
                            op.publish_result == LoadingSlowly
                    in
                    div
                        [ class "button is-small is-success has-text-weight-semibold"
                        , onClick (op.onSubmit (not isLoading) <| op.onPushBlob op.blob.id)
                        , title (T.publishTitle op.session.lexicon)
                        ]
                        [ A.icon1 "icon-share" T.publish
                        , loadingSpin isLoading
                        ]

                  else
                    text ""
                ]


viewBlob : OrgaNodeData -> Maybe (Op msg) -> Html msg
viewBlob data op_m =
    case op_m of
        Just op ->
            -- Tension view
            case op.data.mode of
                NodeEdit ->
                    div [ class "box doc-container", classList [ ( "is-lazy", data.isLazy ) ] ] <|
                        (if op.data.editMode == Just EditAbout then
                            let
                                nameid_ =
                                    Maybe.map (.nameid >> String.split "#" >> LE.last) data.node
                                        |> withDefault Nothing

                                isSendable =
                                    (data.node_data.about /= op.data.form.node.about)
                                        || (Maybe.map .name data.node /= op.data.form.node.name)
                                        -- root nameid is ""
                                        || (nameid_ /= op.data.form.node.nameid && op.data.form.node.nameid /= Just "")

                                isLoading =
                                    op.data.result == LoadingSlowly
                            in
                            [ viewAboutInput data.hasBeenPushed op.data.form.txt op.data.form.node op
                            , viewBlobButtons isSendable isLoading op
                            ]

                         else
                            [ showMaybe data.node (\node -> viewAboutSection node data (Just op.onChangeEdit)) ]
                        )
                            ++ [ hr [] [] ]
                            ++ (if op.data.editMode == Just EditMandate then
                                    let
                                        isSendable =
                                            data.node_data.mandate /= op.data.form.node.mandate

                                        isLoading =
                                            op.data.result == LoadingSlowly
                                    in
                                    [ viewMandateInput op.data.form.txt op.data.form.node.mandate op
                                    , viewBlobButtons isSendable isLoading op
                                    ]

                                else
                                    [ viewMandateSection op.session (unwrap Nothing .role_type data.node) data.node_data.mandate (Just op.onChangeEdit) ]
                               )

                NodeVersions ->
                    viewVersions op.session op.expandedDiff op.onToggleDiff op.tension_blobs

                NoView ->
                    text ""

        Nothing ->
            -- Overview view
            div [ class "box doc-container", classList [ ( "is-lazy", data.isLazy ) ] ]
                [ showMaybe data.node
                    (\node ->
                        div []
                            [ -- About
                              viewAboutSection node data Nothing
                            , -- Circle lead
                              if List.length data.leads > 0 then
                                let
                                    i =
                                        List.length data.leads

                                    txt =
                                        if i == 1 then
                                            String.toLower T.firstLink

                                        else
                                            String.toLower T.firstLinks
                                in
                                div [ class " mt-3" ]
                                    [ A.icon1 "icon-users" ""
                                    , span [ class "has-text-evidence" ] [ text (String.fromInt i) ]
                                    , span [ class "is-discrete" ] [ text (" " ++ txt ++ "  " ++ space_) ]
                                    , viewUsers True data.leads
                                    ]

                              else
                                -- Role Lead link Maybe.map
                                showMaybe node.first_link
                                    (\fs ->
                                        div [ class "is-inline-flex mt-3" ]
                                            [ A.icon1 "icon-user" (String.toLower T.firstLink ++ "  " ++ space_)
                                            , viewUser True fs.username
                                            ]
                                    )

                            -- Open Contracts
                            , if data.isAdmin then
                                case unwrap 0 .n_open_contracts data.node of
                                    0 ->
                                        text ""

                                    i ->
                                        let
                                            tid =
                                                withDefaultData "" data.tid_r
                                        in
                                        div [ class "is-flex mt-3" ]
                                            [ a [ class "has-text-warning is-size-7", href (Route.Tension_Dynamic_Dynamic_Contract { param1 = data.focus.rootnameid, param2 = tid } |> toHref) ]
                                                [ strong [] [ text (String.fromInt i) ], text " open contracts" ]
                                            ]

                              else
                                text ""
                            ]
                    )
                , hr [] []
                , viewMandateSection data.session (unwrap Nothing .role_type data.node) data.node_data.mandate Nothing
                ]



--- Template view


viewAboutSection : Node -> OrgaNodeData -> Maybe (NodeEdit -> msg) -> Html msg
viewAboutSection node data op_m =
    div []
        [ -- Node title
          div [ class "level subtitle" ]
            [ div [ class "level-left", style "max-width" "90%" ]
                [ A.icon "icon-info icon-lg mr-2"
                , span [ class "nowrap" ] [ text T.about ]
                , text space_
                , span [ class "has-text-strong" ] [ text node.name ]
                ]
            , if
                data.hasInnerToolbar
                    && isSuccess data.tid_r
                    && not (List.member node.role_type (List.map Just [ RoleType.Guest, RoleType.Owner, RoleType.Pending, RoleType.Retired ]))
              then
                div [ class "level-right m-0 is-small is-hidden-mobile" ]
                    [ viewToolbarDropdown NoView data ]

              else
                showMaybe op_m
                    (\onChangeEdit ->
                        div
                            [ class "button has-text-weight-normal is-pulled-right is-small"
                            , onClick (onChangeEdit EditAbout)
                            ]
                            [ A.icon "icon-edit-2" ]
                    )
            ]
        , -- Node Hints
          showIf (op_m == Nothing) <|
            div [ class "columns mt-1 mb-3" ]
                [ div [ class "column is-6 py-0" ]
                    [ viewNodeDescr False node ]
                ]
        , -- Node About
          showMaybe data.node_data.about
            (\about -> renderMarkdown "" "is-human has-text-strong" about)
        ]


viewMandateSection : SessionCommon -> Maybe RoleType.RoleType -> Maybe Mandate -> Maybe (NodeEdit -> msg) -> Html msg
viewMandateSection session role_type_m mandate_m op_m =
    div []
        [ div [ class "level subtitle" ]
            [ div [ class "level-left" ]
                [ A.icon "icon-book-open icon-lg mr-2"
                , text (T.mandate session.lexicon)
                ]
            , Maybe.map
                (\onChangeEdit ->
                    div
                        [ class "button has-text-weight-normal is-pulled-right is-small"
                        , onClick (onChangeEdit EditMandate)
                        ]
                        [ A.icon "icon-edit-2" ]
                )
                op_m
                |> withDefault (text "")
            ]
        , case mandate_m of
            Just mandate ->
                div []
                    [ viewMandateSubSection session T.purpose (Just mandate.purpose)
                    , viewMandateSubSection session T.responsabilities mandate.responsabilities
                    , viewMandateSubSection session T.domains mandate.domains
                    , viewMandateSubSection session T.policies mandate.policies
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
                        renderMarkdown session.file_server_url "is-human" md

                    Nothing ->
                        div [ class "is-italic" ] [ text "No description for this node." ]

        --, p [ class "column is-fullwidth" ] []
        ]


viewMandateSubSection : SessionCommon -> String -> Maybe String -> Html msg
viewMandateSubSection session name maybePara =
    case maybePara of
        Just para ->
            div [ class "subSection" ]
                [ div [ class "label" ] [ text name ]
                , p [ class "mt-3" ] [ renderMarkdown session.file_server_url "is-human" para ]
                ]

        Nothing ->
            text ""



--- Input view


{-| About View for the Tension Blob input
-}
viewAboutInput hasBeenPushed txt node op =
    div []
        [ div [ class "field" ]
            [ label [ class "label" ] [ text T.name ]
            , div [ class "control" ]
                [ input
                    [ class "input autofocus followFocus"
                    , attribute "data-nextfocus" "aboutField"
                    , type_ "text"
                    , placeholder T.name
                    , value (withDefault "" node.name)
                    , onInput <| op.onChangePost "name"
                    , required True
                    ]
                    []
                ]
            , p [ class "help-label" ] [ text txt.name_help ]
            , if not hasBeenPushed || isFailure op.result then
                div [ class "mt-3" ]
                    [ viewUrlForm node.nameid (op.onChangePost "nameid") False ]

              else
                text ""
            ]
        , div [ class "field" ]
            [ label [ class "label" ] [ text T.about ]
            , div [ class "control" ]
                [ input
                    [ id "aboutField"
                    , class "input followFocus"
                    , attribute "data-nextfocus" "textAreaModal"
                    , type_ "text"
                    , placeholder T.aboutOpt
                    , spellcheck True
                    , value (withDefault "" node.about)
                    , onInput <| op.onChangePost "about"
                    ]
                    []
                ]
            , p [ class "help-label" ] [ text txt.about_help ]
            , br [] []
            ]
        ]


{-| About View for the NewTension input
-}
viewAboutInput2 txt node op =
    div []
        [ div [ class "field is-grouped" ]
            [ div [ class "control is-expanded" ]
                [ label [ class "label" ] [ text T.name ]
                , input
                    [ class "input autofocus followFocus"
                    , attribute "data-nextfocus" "aboutField"
                    , type_ "text"
                    , placeholder T.name
                    , value (withDefault "" node.name)
                    , onInput <| op.onChangePost "name"
                    , required True
                    ]
                    []
                , p [ class "help-label" ] [ text txt.name_help ]
                , if isFailure op.result then
                    div [ class "mt-3" ]
                        [ viewUrlForm node.nameid (op.onChangePost "nameid") True ]

                  else
                    text ""
                ]
            , case node.type_ of
                Just NodeType.Role ->
                    div [ class "control" ]
                        [ div [ class "field mb-5" ]
                            [ label [ class "label is-pulled-left" ] [ text T.authority, helperButton "ml-2 is-right" (T.authorityHelper op.session.lexicon) ]
                            , viewSelectAuthority op
                            ]
                        ]

                Just NodeType.Circle ->
                    div [ class "control" ]
                        [ div [ class "field mb-5" ]
                            [ label [ class "label is-pulled-left" ] [ text T.governance, helperButton "ml-2 is-right" T.governanceHelper ]
                            , viewSelectGovernance op
                            ]
                        ]

                Nothing ->
                    text ""
            ]
        , div [ class "field" ]
            [ label [ class "label" ] [ text T.about ]
            , div [ class "control" ]
                [ input
                    [ id "aboutField"
                    , class "input followFocus"
                    , attribute "data-nextfocus" "textAreaModal"
                    , type_ "text"
                    , placeholder T.aboutOpt
                    , spellcheck True
                    , value (withDefault "" node.about)
                    , onInput <| op.onChangePost "about"
                    ]
                    []
                ]
            , p [ class "help-label" ] [ text txt.about_help ]
            , br [] []
            ]
        ]



-- @TODO
-- viewAboutInput3 (the view use in Org.Settings)


viewMandateInput :
    FormText
    -> Maybe Mandate
    ->
        { a
            | session : SessionCommon
            , data : NodeDoc
            , mdOps :
                Maybe
                    { onChangeViewMode : String -> InputViewMode -> msg
                    , onRichText : String -> String -> msg
                    , onToggleMdHelp : String -> msg
                    , post : Dict String String
                    }
            , onChangePost : String -> String -> msg
            , onAddResponsabilities : msg
            , onAddDomains : msg
            , onAddPolicies : msg
        }
    -> Html msg
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

        mdField targetid =
            case op.mdOps of
                Just mdOps ->
                    let
                        isP =
                            Dict.get ("viewMode:" ++ targetid) mdOps.post == Just "Preview"
                    in
                    { fieldClass = "field md-editor"
                    , header =
                        viewCommentInputHeader
                            { onChangeViewMode = mdOps.onChangeViewMode targetid
                            , onRichText = mdOps.onRichText
                            , onToggleMdHelp = mdOps.onToggleMdHelp
                            }
                            targetid
                            { viewMode = ternary isP Preview Write, post = mdOps.post }
                    , isPreview = isP
                    , preview = \val -> ternary isP (div [] [ hr [] [], div [ class "mt-2 mx-3" ] [ renderMarkdown op.session.file_server_url "is-human hidden-textarea" val ] ]) (text "")
                    }

                Nothing ->
                    { fieldClass = "field"
                    , header = text ""
                    , isPreview = False
                    , preview = \_ -> text ""
                    }

        md_purpose =
            mdField "mandatePurpose"

        md_responsabilities =
            mdField "mandateResponsabilities"

        md_domains =
            mdField "mandateDomains"

        md_policies =
            mdField "mandatePolicies"
    in
    div []
        [ div [ class md_purpose.fieldClass ]
            [ div [ class "label" ]
                [ text T.purpose, helperButton "ml-2" (T.purposeHelper |> Format.value txt.purposeSubject) ]
            , md_purpose.header
            , div [ class "control" ]
                [ textarea
                    [ id "mandatePurpose"
                    , class "textarea"
                    , classList [ ( "is-invisible-force", md_purpose.isPreview ) ]
                    , rows (min 15 (max purpose_len 2))
                    , placeholder txt.ph_purpose
                    , value purpose
                    , onInput <| op.onChangePost "purpose"
                    , required True
                    ]
                    []
                , md_purpose.preview purpose
                ]
            ]
        , if showResponsabilities then
            let
                input_len =
                    List.length <| String.lines purpose
            in
            div [ class md_responsabilities.fieldClass ]
                [ div [ class "label" ] [ text T.responsabilities, helperButton "ml-2" T.responsabilitiesHelper ]
                , md_responsabilities.header
                , div [ class "control" ]
                    [ textarea
                        [ id "mandateResponsabilities"
                        , class "textarea autofocus"
                        , classList [ ( "is-invisible-force", md_responsabilities.isPreview ) ]
                        , rows (min 15 (max input_len 2))
                        , placeholder txt.ph_responsabilities
                        , value responsabilities
                        , onInput <| op.onChangePost "responsabilities"
                        ]
                        []
                    , md_responsabilities.preview responsabilities
                    ]
                ]

          else
            text ""
        , if showDomains then
            let
                input_len =
                    List.length <| String.lines purpose
            in
            div [ class md_domains.fieldClass ]
                [ div [ class "label" ] [ text T.domains, helperButton "ml-2" T.domainsHelper ]
                , md_domains.header
                , div [ class "control" ]
                    [ textarea
                        [ id "mandateDomains"
                        , class "textarea autofocus"
                        , classList [ ( "is-invisible-force", md_domains.isPreview ) ]
                        , rows (min 15 (max input_len 2))
                        , placeholder txt.ph_domains
                        , value domains
                        , onInput <| op.onChangePost "domains"
                        ]
                        []
                    , md_domains.preview domains
                    ]
                ]

          else
            text ""
        , if showPolicies then
            let
                input_len =
                    List.length <| String.lines purpose
            in
            div [ class md_policies.fieldClass ]
                [ div [ class "label" ] [ text T.policies, helperButton "ml-2" T.policiesHelper ]
                , md_policies.header
                , div [ class "control" ]
                    [ textarea
                        [ id "mandatePolicies"
                        , class "textarea autofocus"
                        , classList [ ( "is-invisible-force", md_policies.isPreview ) ]
                        , rows (min 15 (max input_len 2))
                        , placeholder txt.ph_policies
                        , value policies
                        , onInput <| op.onChangePost "policies"
                        ]
                        []
                    , md_policies.preview policies
                    ]
                ]

          else
            text ""
        , if not showResponsabilities then
            span [ class "pr-2" ]
                [ div [ class "button is-small", onClick op.onAddResponsabilities ]
                    [ A.icon1 "icon-plus" "", text T.addResponsabilities ]
                ]

          else
            text ""
        , if not showDomains then
            span [ class "pr-2" ]
                [ div [ class "button is-small", onClick op.onAddDomains ]
                    [ A.icon1 "icon-plus" "", text T.addDomains ]
                ]

          else
            text ""
        , if not showPolicies then
            span [ class "pr-2" ]
                [ div [ class "button is-small", onClick op.onAddPolicies ]
                    [ A.icon1 "icon-plus" "", text T.addPolicies ]
                ]

          else
            text ""
        ]



---- Components view


{-| Integrate in view\*Input when ths will be a state-full component
-}
viewBlobButtons : Bool -> Bool -> Op msg -> Html msg
viewBlobButtons isSendable isLoading op =
    let
        d =
            op.data

        f =
            op.data.form

        data =
            { d | form = { f | withBlob = True } }
    in
    div []
        [ case op.data.result of
            Failure err ->
                viewGqlErrors err

            _ ->
                text ""
        , div [ class "field is-grouped is-grouped-right mt-1" ]
            [ div [ class "control" ]
                [ div [ class "buttons" ]
                    [ button [ class "button", onClick op.onCancelBlob ]
                        [ text T.cancel ]
                    , button
                        [ class "button is-success"
                        , classList [ ( "is-loading", isLoading ) ]
                        , disabled (not isSendable)
                        , onClick (op.onSubmit (not isLoading) <| op.onSubmitBlob data)
                        ]
                        [ text T.saveChanges ]
                    ]
                ]
            ]
        ]


viewSelectAuthority op =
    let
        role_type_selected =
            withDefault RoleType.Peer op.data.form.node.role_type

        checked cls =
            A.icon1 ("icon-check " ++ cls) ""

        unchecked =
            A.icon1 "icon-check is-invisible" ""
    in
    div [ class "field" ]
        [ div [ class "dropdown is-right" ]
            [ div [ class "button dropdown-trigger", attribute "aria-controls" "select-authority" ]
                [ span [ class ("has-text-" ++ (RoleType.toString role_type_selected |> String.toLower)) ] [ text (RoleType.toString role_type_selected) ], i [ class "ml-3 icon-chevron-down1 icon-tiny" ] [] ]
            , div [ id "select-authority", class "dropdown-menu", attribute "role" "menu" ]
                [ div [ class "dropdown-content is-right" ] <|
                    List.map
                        (\role_type ->
                            let
                                clsColor =
                                    "has-text-" ++ (RoleType.toString role_type |> String.toLower)
                            in
                            div
                                [ class ("dropdown-item button-light " ++ clsColor)
                                , onClick <| op.onChangePost "role_type" (RoleType.toString role_type)
                                ]
                                [ ternary (role_type_selected == role_type) (checked clsColor) unchecked
                                , text (RoleType.toString role_type)

                                --, span [ class "is-pulled-right mx-2 is-small tooltip" ] [ A.icon "icon-info" ]
                                ]
                        )
                        [ RoleType.Peer, RoleType.Coordinator ]
                ]
            ]
        ]


viewSelectGovernance op =
    let
        mode_selected =
            withDefault NodeMode.Coordinated op.data.form.node.mode

        checked cls =
            A.icon1 ("icon-check " ++ cls) ""

        unchecked =
            A.icon1 "icon-check is-invisible" ""
    in
    div [ class "field" ]
        [ div [ class "dropdown is-right" ]
            [ div [ class "button dropdown-trigger", attribute "aria-controls" "select-governance" ]
                [ span [ class "has-text-" ] [ text (NodeMode.toString mode_selected) ], i [ class "ml-3 icon-chevron-down1 icon-tiny" ] [] ]
            , div [ id "select-governance", class "dropdown-menu", attribute "role" "menu" ]
                [ div [ class "dropdown-content is-right" ] <|
                    List.map
                        (\mode ->
                            div [ class "dropdown-item button-light", onClick <| op.onChangePost "mode" (NodeMode.toString mode) ]
                                [ ternary (mode_selected == mode) (checked "") unchecked, text (NodeMode.toString mode) ]
                        )
                        NodeMode.list
                ]
            ]
        ]



-- Versions view


viewVersions : SessionCommon -> String -> (String -> msg) -> GqlData TensionBlobs -> Html msg
viewVersions session expandedDiff onToggleDiff blobsData =
    Lazy.lazy4 viewVersions_ session expandedDiff onToggleDiff blobsData


viewVersions_ : SessionCommon -> String -> (String -> msg) -> GqlData TensionBlobs -> Html msg
viewVersions_ session expandedDiff onToggleDiff blobsData =
    case blobsData of
        Success tblobs ->
            let
                blobs =
                    withDefault [] tblobs.blobs
            in
            div [ class "table-containe" ]
                -- @debug table-container with width=100%, do not work!
                [ table [ class "table is-fullwidth table-container" ]
                    [ blobs
                        |> List.indexedMap (\i d -> viewVerRow session expandedDiff onToggleDiff i d (LE.getAt (i + 1) blobs))
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


viewVerRow : SessionCommon -> String -> (String -> msg) -> Int -> Blob -> Maybe Blob -> List (Html msg)
viewVerRow session expandedDiff onToggleDiff i blob prevBlob =
    let
        isExpanded =
            expandedDiff == blob.id
    in
    [ tr [ class "mediaBox is-hoverable", classList [ ( "is-active", i == 0 ) ] ]
        [ td []
            [ span []
                [ text <|
                    if prevBlob == Nothing then
                        T.onNode_blob

                    else
                        T.document ++ space_ ++ T.edited
                ]
            , text space_
            , byAt session blob.createdBy blob.createdAt
            ]
        , td [ class "has-text-right" ]
            [ case blob.pushedFlag of
                Just flag ->
                    span
                        [ class "mr-3"
                        , attribute "style" "cursor: inherit;"
                        , title (T.published ++ " " ++ formatDate session.lang session.now flag)
                        ]
                        [ A.icon "icon-flag" ]

                Nothing ->
                    text ""
            , button
                [ class "button is-small is-rounded is-discrete"
                , classList [ ( "is-active", isExpanded ) ]
                , title T.showDiff
                , onClick (onToggleDiff blob.id)
                ]
                [ A.icon "icon-eye" ]
            ]
        ]
    ]
        ++ (if isExpanded then
                [ tr [] [ td [ colspan 2 ] [ viewBlobDiff blob prevBlob ] ] ]

            else
                []
           )



--- Blob diff (split view)


{-| The diffable text fields of a blob, labels aligned across revisions. -}
blobFields : Blob -> List ( String, String )
blobFields blob =
    let
        n =
            withDefault (initNodeFragment Nothing) blob.node

        m =
            withDefault initMandate n.mandate
    in
    [ ( T.name, withDefault "" n.name )
    , ( T.about, withDefault "" n.about )
    , ( T.purpose, m.purpose )
    , ( T.responsabilities, withDefault "" m.responsabilities )
    , ( T.domains, withDefault "" m.domains )
    , ( T.policies, withDefault "" m.policies )
    ]


viewBlobDiff : Blob -> Maybe Blob -> Html msg
viewBlobDiff blob prevBlob =
    let
        old_fields =
            prevBlob
                |> Maybe.map blobFields
                |> withDefault (List.map (\( k, _ ) -> ( k, "" )) (blobFields blob))

        changed =
            List.map2 (\( k, new ) ( _, old ) -> ( k, old, new )) (blobFields blob) old_fields
                |> List.filter (\( _, old, new ) -> old /= new)
    in
    if changed == [] then
        div [ class "is-discrete is-italic pb-2" ] [ text T.noChanges ]

    else
        div [ class "pb-2" ] <|
            List.map
                (\( k, old, new ) ->
                    div [ class "mb-3" ]
                        [ div [ class "is-size-7 has-text-weight-semibold mb-1" ] [ text k ]
                        , table [ class "diff-split" ]
                            [ tbody [] (Diff.diffLines old new |> toSplitRows |> withContext diffContext |> List.map viewHunk) ]
                        ]
                )
                changed


type alias DiffRow =
    { left : Maybe String, right : Maybe String, changed : Bool }


{-| Unchanged lines kept around each change. -}
diffContext : Int
diffContext =
    4


type Hunk
    = Line DiffRow
    | Skipped Int


{-| Pair removed/added hunks side by side, padding the shorter side. -}
toSplitRows : List (Diff.Change () String) -> List DiffRow
toSplitRows changes =
    let
        flush ( rem, add ) rows =
            rows ++ zipPad (List.reverse rem) (List.reverse add)

        step change ( pending, rows ) =
            case change of
                Diff.Removed l ->
                    ( Tuple.mapFirst ((::) l) pending, rows )

                Diff.Added l ->
                    ( Tuple.mapSecond ((::) l) pending, rows )

                Diff.Similar l r _ ->
                    ( ( [], [] ), flush pending rows ++ [ DiffRow (Just l) (Just r) True ] )

                Diff.NoChange l ->
                    ( ( [], [] ), flush pending rows ++ [ DiffRow (Just l) (Just l) False ] )

        ( leftover, rows_ ) =
            List.foldl step ( ( [], [] ), [] ) changes
    in
    flush leftover rows_


zipPad : List String -> List String -> List DiffRow
zipPad rem add =
    let
        n =
            max (List.length rem) (List.length add)

        pad xs =
            List.map Just xs ++ List.repeat (n - List.length xs) Nothing
    in
    List.map2 (\l r -> DiffRow l r True) (pad rem) (pad add)


{-| Keep changed lines plus `n` lines of context, collapsing the gaps. -}
withContext : Int -> List DiffRow -> List Hunk
withContext n rows =
    let
        kept =
            rows
                |> List.indexedMap Tuple.pair
                |> List.filter (Tuple.second >> .changed)
                |> List.concatMap (\( i, _ ) -> List.range (i - n) (i + n))
                |> Set.fromList

        step ( i, row ) acc =
            if Set.member i kept then
                Line row :: acc

            else
                case acc of
                    (Skipped k) :: rest ->
                        Skipped (k + 1) :: rest

                    _ ->
                        Skipped 1 :: acc
    in
    rows
        |> List.indexedMap Tuple.pair
        |> List.foldl step []
        |> List.reverse


viewHunk : Hunk -> Html msg
viewHunk hunk =
    case hunk of
        Line row ->
            viewDiffRow row

        Skipped k ->
            tr [ class "diff-skip" ]
                [ td [ colspan 2 ] [ text (T.unchangedLines |> Format.value (String.fromInt k)) ] ]


viewDiffRow : DiffRow -> Html msg
viewDiffRow row =
    let
        ( left, right ) =
            case ( row.changed, row.left, row.right ) of
                ( True, Just l, Just r ) ->
                    inlineDiff l r

                _ ->
                    ( [ text (withDefault "" row.left) ], [ text (withDefault "" row.right) ] )
    in
    tr []
        [ td [ classList [ ( "diff-del", row.changed && row.left /= Nothing ) ] ] left
        , td [ classList [ ( "diff-add", row.changed && row.right /= Nothing ) ] ] right
        ]


{-| Char-level highlight within a changed line pair. Skipped when the lines are
too long or too dissimilar, where a char diff is noise rather than signal.
-}
inlineDiff : String -> String -> ( List (Html msg), List (Html msg) )
inlineDiff l r =
    let
        size =
            String.length l + String.length r
    in
    -- ponytail: O(NP) over chars, guarded by length; switch to word-level if prose diffs read noisy
    if size > 4000 then
        ( [ text l ], [ text r ] )

    else
        let
            changes =
                Diff.diff (String.toList l) (String.toList r)

            common =
                LE.count isNoChange changes
        in
        if toFloat (2 * common) / toFloat size < 0.3 then
            -- too dissimilar: highlighting every char adds nothing
            ( [ text l ], [ text r ] )

        else
            ( viewRuns (sideRuns True changes), viewRuns (sideRuns False changes) )


isNoChange : Diff.Change Never Char -> Bool
isNoChange c =
    case c of
        Diff.NoChange _ ->
            True

        _ ->
            False


{-| Keep the chars visible on one side, flagging those that changed. -}
sideChar : Bool -> Diff.Change Never Char -> Maybe ( Bool, Char )
sideChar isLeft c =
    case c of
        Diff.NoChange ch ->
            Just ( False, ch )

        Diff.Removed ch ->
            ternary isLeft (Just ( True, ch )) Nothing

        Diff.Added ch ->
            ternary isLeft Nothing (Just ( True, ch ))

        Diff.Similar _ _ ever ->
            never ever


sideRuns : Bool -> List (Diff.Change Never Char) -> List ( Bool, String )
sideRuns isLeft changes =
    changes
        |> List.filterMap (sideChar isLeft)
        |> LE.groupWhile (\a b -> Tuple.first a == Tuple.first b)
        |> List.map (\( x, xs ) -> ( Tuple.first x, String.fromList (List.map Tuple.second (x :: xs)) ))


viewRuns : List ( Bool, String ) -> List (Html msg)
viewRuns =
    List.map (\( changed, s ) -> ternary changed (span [ class "diff-chg" ] [ text s ]) (text s))



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
            if form.isNewNode then
                { form
                    | node = { node | name = Just value, nameid = Just (nameidEncoder value) }
                    , post = Dict.insert "title" value form.post
                }

            else
                { form | node = { node | name = Just value } }

        _ ->
            -- title, message...
            { form | post = Dict.insert field value form.post }
