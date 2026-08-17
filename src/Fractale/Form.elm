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


module Fractale.Form exposing
    ( ActionForm
    , ArtefactNodeForm
    , AssigneeForm
    , CommentPatchForm
    , ContractForm
    , Ev
    , FormText
    , InputViewMode(..)
    , LabelForm
    , OrgaForm
    , ProjectForm
    , ProjectPanelForm
    , ProjectTemplateForm
    , TensionForm
    , TensionTemplateForm
    , UserForm
    , UserProfileForm
    , buildVote
    , decodeColumnRef
    , decodeLabel
    , decodeProjectRef
    , encodeLabel
    , ev2eventFragment
    , eventFromForm
    , form2cid
    , initActionForm
    , initArtefactNodeForm
    , initAssigneeForm
    , initCommentPatchForm
    , initContractForm
    , initFormText
    , initLabelForm
    , initProjectForm
    , initProjectPanelForm
    , initProjectTemplateForm
    , initTensionForm
    , initTensionTemplateForm
    , initUserForm
    , initUserProfileForm
    , isSelfContract
    , makeCandidateContractForm
    , tensionToActionForm
    )

import Dict
import Fractale.Codecs exposing (contractIdCodec, memberIdCodec, nid2rootid, nodeFromFragment, voteIdCodec)
import Fractale.User exposing (UserState(..), uctxFromUser)
import Maybe exposing (withDefault)
import ModelSchema exposing (..)
import Schema.Enum.BlobType as BlobType
import Schema.Enum.ContractStatus as ContractStatus
import Schema.Enum.ContractType as ContractType
import Schema.Enum.Lang as Lang
import Schema.Enum.NodeType as NodeType
import Schema.Enum.ProjectStatus as ProjectStatus
import Schema.Enum.RoleType as RoleType
import Schema.Enum.TensionEvent as TensionEvent
import Schema.Enum.TensionStatus as TensionStatus
import Schema.Enum.TensionType as TensionType
import String.Extra as SE
import Text as T



--
-- Tension Form
--


type alias Ev =
    { event_type : TensionEvent.TensionEvent
    , old : String
    , new : String
    }


ev2eventFragment : Ev -> EventFragment
ev2eventFragment ev =
    { event_type = ev.event_type, old = Just ev.old, new = Just ev.new }


type alias TensionForm =
    { id : String
    , uctx : UserCtx
    , source : EmitterOrReceiver
    , target : PNode
    , status : Maybe TensionStatus.TensionStatus
    , type_ : Maybe TensionType.TensionType
    , isNewNode : Bool
    , emitter : Maybe EmitterOrReceiver
    , receiver : Maybe EmitterOrReceiver
    , post : Post -- createdBy, createdAt, title, message and Node attr...
    , viewMode : InputViewMode
    , txt : FormText

    -- data
    , events : List Ev
    , blob_type : Maybe BlobType.BlobType
    , node : NodeFragment
    , md : Maybe String
    , users : List UserForm
    , labels : List Label
    , assignees : List User
    }


initTensionForm : Dict.Dict String String -> String -> Maybe NodeType.NodeType -> UserState -> TensionForm
initTensionForm lexicon tid node_type user =
    { uctx = uctxFromUser user
    , id = tid
    , source = EmitterOrReceiver "" "" Nothing Nothing
    , target = initPNode
    , status = Nothing
    , type_ = Nothing
    , isNewNode = False
    , emitter = Nothing
    , receiver = Nothing
    , post = Dict.empty
    , users = []
    , events = []
    , labels = []
    , assignees = []
    , blob_type = Nothing
    , node = initNodeFragment node_type
    , md = Nothing
    , viewMode = Write
    , txt = initFormText lexicon node_type
    }


eventFromForm : Ev -> TensionForm -> Event
eventFromForm event form =
    { id = ""
    , createdAt = Dict.get "createdAt" form.post |> withDefault ""
    , createdBy = Username form.uctx.username
    , event_type = event.event_type
    , old = Just event.old
    , new = Just event.new
    , mentioned = Nothing
    }


type alias UserForm =
    -- Name is optional but when get user from lookup it
    -- allow to manage two records, a User and UserForm but only one.
    { username : String, name : Maybe String, email : String, pattern : String }


type alias CommentPatchForm =
    { id : String -- comment id (for edit/patch)
    , uctx : UserCtx
    , post : Post
    , viewMode : InputViewMode
    , linkCopied : String
    }


initCommentPatchForm : UserState -> List ( String, String ) -> CommentPatchForm
initCommentPatchForm user data =
    { uctx = uctxFromUser user
    , id = ""
    , post = Dict.fromList data
    , viewMode = Write
    , linkCopied = ""
    }


type alias AssigneeForm =
    { uctx : UserCtx
    , tid : String
    , targets : List String -- Where the labels come from
    , assignee : User -- selected/unselected item
    , isNew : Bool -- to add or remove item
    , events : List Ev
    , post : Post
    }


encodeLabel : Label -> String
encodeLabel label =
    label.name ++ "§" ++ withDefault "" label.color


decodeLabel : String -> Label
decodeLabel label_raw =
    Label "" (SE.leftOfBack "§" label_raw) (SE.rightOfBack "§" label_raw |> Just) []


{-| Decodes "id§name§" descriptors emitted by graph/card\_resolver.go.
-}
decodeProjectRef : String -> { id : String, name : String }
decodeProjectRef raw =
    case String.split "§" raw of
        id :: name :: _ ->
            { id = id, name = name }

        _ ->
            { id = "", name = raw }


{-| Decodes "id§name§color§projectId" descriptors emitted by graph/card\_resolver.go.
The projectId tail is optional for backward compatibility with older events.
-}
decodeColumnRef : String -> { id : String, name : String, color : String, projectId : String }
decodeColumnRef raw =
    case String.split "§" raw of
        id :: name :: color :: projectId :: _ ->
            { id = id, name = name, color = color, projectId = projectId }

        id :: name :: color :: _ ->
            { id = id, name = name, color = color, projectId = "" }

        id :: name :: _ ->
            { id = id, name = name, color = "", projectId = "" }

        _ ->
            { id = "", name = raw, color = "", projectId = "" }


initAssigneeForm : String -> UserState -> AssigneeForm
initAssigneeForm tid user =
    { uctx = uctxFromUser user
    , tid = tid
    , targets = []
    , assignee = User "" Nothing
    , isNew = False
    , events = []
    , post = Dict.empty
    }


type alias LabelForm =
    { uctx : UserCtx
    , tid : String
    , targets : List String -- Where the items come from
    , label : Label -- selected/unselected item
    , isNew : Bool -- to add or remove item
    , events : List Ev
    , post : Post
    }


initLabelForm : String -> UserState -> LabelForm
initLabelForm tid user =
    { uctx = uctxFromUser user
    , tid = tid
    , targets = []
    , label = Label "" "" Nothing []
    , isNew = False
    , events = []
    , post = Dict.empty
    }


type alias ProjectForm =
    { uctx : UserCtx
    , id : String
    , nameid : String -- use for roonameid identification
    , status : Maybe ProjectStatus.ProjectStatus
    , post : Post
    , columns : Maybe (List ColumnDraft)
    , collaborators_add : List String
    , collaborators_remove : List String
    , peerCanEditProject : Maybe Bool
    , guestCanEditProject : Maybe Bool
    }


type alias ProjectPanelForm =
    { uctx : UserCtx
    , tid : String
    , targets : List String -- circles to fetch projects from
    , project : ProjectWithColumns -- selected/unselected item
    , isNew : Bool
    , post : Post
    }


initProjectPanelForm : String -> UserState -> ProjectPanelForm
initProjectPanelForm tid user =
    { uctx = uctxFromUser user
    , tid = tid
    , targets = []
    , project = { id = "", name = "", columns = [], nodes = [] }
    , isNew = False
    , post = Dict.empty
    }


initProjectForm : UserState -> String -> ProjectForm
initProjectForm user nameid =
    { uctx = uctxFromUser user
    , id = ""
    , nameid = nameid
    , status = Nothing
    , columns = Nothing
    , post = Dict.fromList []
    , collaborators_add = []
    , collaborators_remove = []
    , peerCanEditProject = Nothing
    , guestCanEditProject = Nothing
    }



--
-- Settings Form
--


type alias ArtefactNodeForm =
    { uctx : UserCtx
    , id : String
    , nameid : String -- use for roonameid identification
    , post : Post
    , mandate : Mandate
    , role_type : RoleType.RoleType
    }


initArtefactNodeForm : UserState -> String -> String -> ArtefactNodeForm
initArtefactNodeForm user nameid initColor =
    { uctx = uctxFromUser user
    , id = ""
    , nameid = nameid
    , post = Dict.fromList [ ( "color", initColor ) ]
    , mandate = initMandate
    , role_type = RoleType.Peer
    }



--
-- Tension Template Form
--


type alias TensionTemplateForm =
    { uctx : UserCtx
    , id : String
    , nameid : String -- circle nameid for rootnameid derivation
    , post : Post -- "name", "title", "comment"
    , description : Maybe String
    , viewMode : InputViewMode
    , type_ : TensionType.TensionType
    , is_recursive : Bool
    , labels : List Label
    , assignees : List User
    , orig_labels : List Label -- Dgraph `set` appends to lists; we diff against originals to build `remove` mutations
    , orig_assignees : List User
    }


initTensionTemplateForm : UserState -> String -> TensionTemplateForm
initTensionTemplateForm user nameid =
    { uctx = uctxFromUser user
    , id = ""
    , nameid = nameid
    , post = Dict.empty
    , description = Nothing
    , viewMode = Write
    , type_ = TensionType.Operational
    , is_recursive = False
    , labels = []
    , assignees = []
    , orig_labels = []
    , orig_assignees = []
    }



--
-- Project Template Form
--


type alias ProjectTemplateForm =
    { uctx : UserCtx
    , id : String
    , nameid : String -- circle nameid for rootnameid derivation
    , post : Post -- "name", "old_name"
    , description : Maybe String
    , is_recursive : Bool
    , columns : List ColumnDraft
    }


initProjectTemplateForm : UserState -> String -> ProjectTemplateForm
initProjectTemplateForm user nameid =
    { uctx = uctxFromUser user
    , id = ""
    , nameid = nameid
    , post = Dict.empty
    , description = Nothing
    , is_recursive = False
    , columns = []
    }



--
-- Join Form
--


type alias ActionForm =
    { uctx : UserCtx
    , tid : String -- Tension ID
    , bid : String -- Blob ID
    , node : Node -- Original Node
    , fragment : NodeFragment -- Node changes
    , events : List Ev -- Changes events
    , users : List UserForm -- Note: For contract, One event <-> One user (candidate)
    , post : Post
    }


initActionForm : String -> UserState -> ActionForm
initActionForm tid user =
    { uctx = uctxFromUser user
    , tid = tid
    , bid = ""
    , node = initNode
    , fragment = initNodeFragment Nothing
    , users = []
    , events = []
    , post = Dict.empty
    }


initUserForm : UserForm
initUserForm =
    { username = "", name = Nothing, email = "", pattern = "" }


tensionToActionForm : TensionForm -> ActionForm
tensionToActionForm form =
    initActionForm form.id (LoggedIn form.uctx)
        |> (\f ->
                { f
                    | node = nodeFromFragment form.target.nameid form.node
                    , users = form.users
                    , events = form.events
                    , post = form.post
                }
           )



--
-- User Profile Form
--


type alias UserProfileForm =
    { username : String
    , notifyByEmail : Maybe Bool
    , lang : Maybe Lang.Lang
    , post : Post
    }


initUserProfileForm : String -> UserProfileForm
initUserProfileForm username =
    UserProfileForm username Nothing Nothing Dict.empty


type alias OrgaForm =
    { uctx : UserCtx
    , post : Post
    }



--
-- Contract Form
--


type alias ContractForm =
    { uctx : UserCtx
    , tid : String
    , status : ContractStatus.ContractStatus
    , contract_type : ContractType.ContractType
    , event : EventFragment
    , contractid : String
    , participants : List Vote
    , candidates : List Username
    , pending_candidates : List Email
    , post : Post
    , node_type : Maybe NodeType.NodeType
    }


initContractForm : UserState -> ContractForm
initContractForm user =
    { uctx = uctxFromUser user
    , tid = "" -- example
    , status = ContractStatus.Open
    , contract_type = ContractType.AnyCoordoDual
    , event = initEventFragment
    , contractid = ""
    , participants = []
    , candidates = []
    , pending_candidates = []
    , post = Dict.empty
    , node_type = Nothing -- help identify the type of tension for "Move" events.
    }


buildVote : String -> String -> String -> Int -> Vote
buildVote contractid rootnameid username value =
    { voteid = voteIdCodec contractid rootnameid username
    , node = { nameid = memberIdCodec rootnameid username }
    , data = [ value ]
    }


makeCandidateContractForm : ActionForm -> List ContractForm
makeCandidateContractForm form =
    List.map2 Tuple.pair form.users form.events
        |> List.map
            (\( u, e ) ->
                let
                    ( et, old, new ) =
                        ( TensionEvent.toString e.event_type, e.old, e.new )

                    contractid =
                        contractIdCodec form.tid et old new

                    rootnameid =
                        nid2rootid form.node.nameid

                    -- Feed candidate and pendingcandidate (only one candidate/pendingCandidate supported)
                    ( candidates, pending_candidates ) =
                        List.foldl
                            (\uf ( cand, pend ) ->
                                if uf.email == "" then
                                    ( [ { username = uf.username } ], [] )

                                else
                                    ( [], [ { email = uf.email } ] )
                            )
                            ( [], [] )
                            [ u ]
                in
                { uctx = form.uctx
                , tid = form.tid
                , event = ev2eventFragment e
                , post = form.post
                , status = ContractStatus.Open
                , contract_type = ContractType.AnyCandidates
                , contractid = contractid
                , participants = [ buildVote contractid rootnameid form.uctx.username 1 ]
                , candidates = candidates
                , pending_candidates = pending_candidates
                , node_type = Nothing
                }
            )


form2cid : ActionForm -> String
form2cid form =
    let
        ( et, old, new ) =
            List.head form.events
                |> Maybe.map (\x -> ( TensionEvent.toString x.event_type, x.old, x.new ))
                |> withDefault ( "", "", "" )
    in
    contractIdCodec form.tid et old new


isSelfContract : UserCtx -> List UserForm -> Bool
isSelfContract uctx users =
    List.length users
        == 1
        && List.member uctx.username (List.map (\x -> x.username) users)



--
-- View
--


type InputViewMode
    = Write
    | Preview


type alias FormText =
    { added : String
    , name_help : String
    , about_help : String
    , message_help : String
    , ph_purpose : String
    , ph_responsabilities : String
    , ph_domains : String
    , ph_policies : String
    , submit : String
    , close_submit : String
    , purposeSubject : String
    }


initFormText : Dict.Dict String String -> Maybe NodeType.NodeType -> FormText
initFormText lexicon node_type =
    case node_type of
        Nothing ->
            FormText
                (T.tensionAdded lexicon)
                (T.tensionTitleHelp lexicon)
                ""
                T.tensionMessageHelp
                ""
                ""
                ""
                ""
                (T.tensionSubmit lexicon)
                ""
                T.orgaSubject

        Just NodeType.Role ->
            FormText
                T.roleAdded
                T.roleNameHelp
                T.roleAboutHelp
                T.roleMessageHelp
                T.phRolePurpose
                T.phRoleResponsabilities
                T.phRoleDomains
                T.phRolePolicies
                (T.tensionSubmit lexicon)
                T.tensionRoleCloseSubmit
                T.roleSubject

        Just NodeType.Circle ->
            FormText
                T.circleAdded
                T.circleNameHelp
                T.circleAboutHelp
                T.circleMessageHelp
                T.phCirclePurpose
                T.phCircleResponsabilities
                T.phCircleDomains
                T.phCirclePolicies
                (T.tensionSubmit lexicon)
                T.tensionCircleCloseSubmit
                T.circleSubject
