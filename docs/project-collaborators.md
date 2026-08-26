# Project Collaborators

Users with write access to a project without being coordinators of the linked circles. Exposes
the backend `Project.collaborators` field.

## Data model

`ProjectData` (`src/ModelSchema.elm`) carries `description` and `collaborators : List Username`,
both fetched by `projectDataPayload` (`QueryProject.elm`). `ProjectForm`
(`src/Fractale/Form.elm`) splits edits into `collaborators_add` / `collaborators_remove`, which
map onto Dgraph's `set` / `remove` in `addProjectInputEncoder` and `updateProjectInputEncoder`
(`PatchNode.elm`) — `set` alone appends, so removals need `remove`.

## UI

- Project create/edit form (`src/Org/Projects.elm`): a `UserInput` picker; on edit the
  add/remove lists come from diffing against the original collaborators.
- Project settings panel (`src/Components/ProjectSettingsPanel.elm`): side menu from the board
  toolbar, editing title/description and collaborators. Each add/remove mutates immediately,
  no batch. Closing is handled by the generic `OUTSIDE_CLICK_CLOSE` JS handler through the
  `closeProjectSettingsPanelFromJs` port.

Access is checked by `Auth.getProjectRights` (collaborator or coordinator of a linked node),
used for the admin check in `src/Org/Project.elm`. See `docs/project-permissions.md` for the
peer/guest flags.

i18n keys: `collaborators`, `collaboratorsHelp`, `projectSettings`.
