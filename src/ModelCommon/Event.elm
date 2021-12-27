module ModelCommon.Event exposing (..)

import Dict exposing (Dict)
import Extra exposing (colorToTextColor, ternary)
import Extra.Date exposing (formatDate)
import Fractal.Enum.BlobType as BlobType
import Fractal.Enum.ContractType as ContractType
import Fractal.Enum.NodeType as NodeType
import Fractal.Enum.NodeVisibility as NodeVisibility
import Fractal.Enum.RoleType as RoleType
import Fractal.Enum.TensionAction as TensionAction
import Fractal.Enum.TensionEvent as TensionEvent
import Fractal.Enum.TensionStatus as TensionStatus
import Fractal.Enum.TensionType as TensionType
import Generated.Route as Route exposing (toHref)
import Global
import Html exposing (Html, a, br, button, div, hr, i, p, span, sub, text)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id)
import Html.Events exposing (onClick)
import Assets as A
import Identicon
import Maybe exposing (withDefault)
import ModelCommon exposing (UserState(..), getParentFragmentFromRole)
import ModelCommon.Codecs
    exposing
        ( ActionType(..)
        , FractalBaseRoute(..)
        , NodeFocus
        , getTensionCharac
        , nid2rootid
        , uriFromNameid
        , uriFromUsername
        )
import ModelCommon.View exposing (statusColor)
import ModelSchema exposing (EmitterOrReceiver, Label, NodeExt, Post, Tension, User, UserCtx, Username)
import Text as T exposing (textH, textT, upH)
import Time


contractTypeToText : ContractType.ContractType -> String
contractTypeToText c =
    case c of
        ContractType.AnyCoordoDual ->
            "dual coordinators"

        ContractType.AnyCandidates ->
            "poll"

        ContractType.AnyCoordoTarget ->
            "receiver coordinator"

        ContractType.AnyCoordoSource ->
            "emitter coordinator"


contractEventToText : TensionEvent.TensionEvent -> String
contractEventToText c =
    case c of
        TensionEvent.Moved ->
            "move tension"

        _ ->
            "@TODO text"


eventToIcon : TensionEvent.TensionEvent -> String
eventToIcon ev =
    case ev of
        TensionEvent.Created ->
            "icon-alert-circle has-text-" ++ statusColor TensionStatus.Open

        TensionEvent.Reopened ->
            "icon-alert-circle has-text-" ++ statusColor TensionStatus.Open

        TensionEvent.Closed ->
            "icon-alert-circle has-text-" ++ statusColor TensionStatus.Closed

        TensionEvent.TitleUpdated ->
            "icon-edit-2"

        TensionEvent.TypeUpdated ->
            "icon-edit-2"

        TensionEvent.Visibility ->
            "icon-lock"

        TensionEvent.Authority ->
            "icon-key"

        TensionEvent.AssigneeAdded ->
            "icon-user"

        TensionEvent.AssigneeRemoved ->
            "icon-user"

        TensionEvent.LabelAdded ->
            "icon-tag"

        TensionEvent.LabelRemoved ->
            "icon-tag"

        TensionEvent.CommentPushed ->
            "icon-message-square"

        TensionEvent.BlobCommitted ->
            "icon-edit-2"

        TensionEvent.BlobPushed ->
            "icon-share"

        TensionEvent.BlobArchived ->
            "icon-archive"

        TensionEvent.BlobUnarchived ->
            "icon-archive"

        TensionEvent.UserJoined ->
            "icon-login-in"

        TensionEvent.UserLeft ->
            "icon-login-out"

        TensionEvent.MemberLinked ->
            "icon-login-in"

        TensionEvent.MemberUnlinked ->
            "icon-login-out"

        TensionEvent.Moved ->
            "right-arrow2 pl-0 pr-0 mr-0"

        _ ->
            ""
