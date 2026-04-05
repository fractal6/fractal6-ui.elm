# Project Collaborators Management

## Overview

Project collaborators are users who have write access to a project without needing to be coordinators of the linked circles. This feature exposes the existing backend `Project.collaborators` field in the UI.

## Architecture

### Data Model

- `ProjectData` (ModelSchema.elm) includes `description : Maybe String` and `collaborators : List Username`
- `ProjectForm` (Bulk.elm) includes `collaborators_add` and `collaborators_remove` lists, matching Dgraph's `set`/`remove` semantics
- `projectDataPayload` (QueryProject.elm) fetches both fields from the API

### Mutation Encoders (PatchNode.elm)

- `addProjectInputEncoder`: sends `collaborators` via `set` when `collaborators_add` is non-empty
- `updateProjectInputEncoder`: sends `collaborators` via `set` for additions and `remove` for removals, independently from the existing node move logic

### UI Entry Points

1. **Project creation/edit form** (`Org/Projects.elm`): A `UserInput` component allows selecting collaborators when creating or editing a project. On edit, the diff between original and current collaborators is computed to generate `collaborators_add`/`collaborators_remove`.

2. **Project settings panel** (`Components/ProjectSettingsPanel.elm`): A side-menu panel accessible from the project board view (settings button in toolbar). Allows:
   - Editing project title and description inline
   - Viewing current collaborators as tags with delete buttons
   - Adding new collaborators via `UserInput` search
   - Each add/remove is an immediate mutation (no batch)

### Authorization

`Auth.getProjectRights` checks if the user is either a project collaborator or a coordinator of any linked node. This is now used in `Org/Project.elm` instead of `hasLazyAdminRole` for the admin check.

### Port

`closeProjectSettingsPanelFromJs` — registered as an Elm port in the component, handled by the generic `OUTSIDE_CLICK_CLOSE` JS handler.

## i18n Keys

- `collaborators`, `collaboratorsHelp`, `projectSettings` (in `i18n/i18n.toml`)
