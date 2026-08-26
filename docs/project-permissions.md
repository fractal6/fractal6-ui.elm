# Project Peer/Guest Edit Permissions

Two boolean flags on a project extend edit access beyond coordinators and explicit
collaborators, both `False` by default:

- `peerCanEditProject` — any org member with a Peer or higher role can edit.
- `guestCanEditProject` — any org member, Guests included. Enabling it implies peer access.

## Authorization

`Auth.getProjectRights` allows on the first match:

1. explicit collaborator
2. org owner
3. `guestCanEditProject` and the user has any org role
4. `peerCanEditProject` and the user has a non-Guest org role
5. otherwise, fall through to the circle-mode checks (Agile / Coordinated)

## Data model

`ProjectData` and `ProjectFull` (`src/ModelSchema.elm`) carry both flags as `Bool`;
`ProjectForm` (`src/Fractale/Form.elm`) uses `Maybe Bool` to tell "unset" from an explicit
value. `Codecs.projectDecoder` defaults them to `False`.

GraphQL: fetched in `projectDataPayload` (`QueryProject.elm`) and `projectFullPayload`
(`QueryNode.elm`), required on create (`AddProjectInput`), optional on update
(`ProjectPatch`, sent only when changed).

## UI

Checkboxes in the project create/edit form (`src/Org/Projects.elm`, after the collaborators
section, `TogglePeerCanEdit` / `ToggleGuestCanEdit` — guest ON turns peer ON) and in the
"Permissions" section of `src/Components/ProjectSettingsPanel.elm`, which submits immediately
through `DoSubmitPermissions`.

i18n keys: `permissions`, `peerCanEditProject(Help)`, `guestCanEditProject(Help)`.
