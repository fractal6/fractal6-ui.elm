# Project Peer/Guest Edit Permissions

## Overview

Projects now support two boolean flags that extend edit access beyond coordinators and explicit collaborators:

- **peerCanEditProject**: When `True`, any organization member with a Peer (or higher) role can edit the project.
- **guestCanEditProject**: When `True`, any organization member including Guests can edit the project. Enabling this implicitly enables peer access as well.

Both default to `False`, preserving the original behavior where only coordinators and explicit collaborators have edit rights.

## Authorization Logic (`Auth.getProjectRights`)

The permission check evaluates in this order:
1. Is the user an explicit collaborator? -> allow
2. Is the user the org owner? -> allow
3. Is `guestCanEditProject` true and user has any org role? -> allow
4. Is `peerCanEditProject` true and user has a non-Guest org role? -> allow
5. Fall through to existing circle-mode-based checks (Agile/Coordinated)

## Data Model

### `ProjectData` and `ProjectFull` (ModelSchema.elm)
Both types include:
```elm
, peerCanEditProject : Bool
, guestCanEditProject : Bool
```

### `ProjectForm` (Fractale/Form.elm)
Uses `Maybe Bool` to distinguish between "not set" and explicit values:
```elm
, peerCanEditProject : Maybe Bool
, guestCanEditProject : Maybe Bool
```

## GraphQL

- **Query**: Both fields are fetched in `projectDataPayload` (QueryProject.elm) and `projectFullPayload` (QueryNode.elm).
- **Create**: Both are required fields in `AddProjectInput`, defaulting to `False` via `withDefault`.
- **Update**: Both are optional in `ProjectPatch`, sent only when changed.
- **JSON Decoder**: `Codecs.projectDecoder` decodes both fields with `withDefault False`.

## UI

### Project Creation/Edit Form (Org/Projects.elm)
Two checkbox fields appear after the collaborators section:
- "Allow peers to edit" (`TogglePeerCanEdit`)
- "Allow guests to edit" (`ToggleGuestCanEdit`)
Toggling guest ON automatically enables peer. The form tracks changes for the edit `isSendable` check.

### Project Settings Panel (Components/ProjectSettingsPanel.elm)
A "Permissions" section appears below collaborators with the same two checkboxes. Changes are submitted immediately via `DoSubmitPermissions`.

## i18n Keys
- `permissions`, `peerCanEditProject`, `peerCanEditProjectHelp`, `guestCanEditProject`, `guestCanEditProjectHelp`
