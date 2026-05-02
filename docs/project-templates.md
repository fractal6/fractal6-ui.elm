# Project Templates

Pre-defined column layouts (kanban schemas) for projects, scoped to a circle. Schema: `ProjectTemplate` in `fractal6.graphql` (stores columns as `columns_json`).

## Where it lives

- **Settings CRUD**: `src/Org/Settings.elm` (`ProjectTemplatesMenu`) — create/edit/delete templates per circle, with inherited (top) and sub-circle sections. Uses an inline column editor (name/description/color, reorder, add/remove).
- **Template picker**: `src/Org/Projects.elm` — the "Simple Kanban" tag in the new-project columns editor is a dropdown listing inherited templates. Two-step fetch: list via REST (`fetchProjectTemplatesTop` → `ProjectTemplateLite`), then full content via GQL (`getProjectTemplateById` → `ProjectTemplateFull`) on selection. Selected template's `columns_json` is decoded and applied to the project form.
- **Shared column row**: `Components.ColorPicker.viewColumnRow` (and `ColumnRowOp`) renders the swatch + name/description inputs + move/remove buttons. Used by both the Settings editor and the new-project columns editor.

## Key queries/mutations

- `getProjectTemplates`, `getProjectTemplateById` — `src/Query/QueryNode.elm`
- `addOneProjectTemplate`, `updateOneProjectTemplate`, `removeOneProjectTemplate` — `src/Query/PatchNode.elm`
- `fetchProjectTemplatesTop`, `fetchProjectTemplatesSub` — `src/Requests.elm`

## Key types

- `ProjectTemplateLite` / `ProjectTemplateFull` — `src/ModelSchema.elm`
- `ProjectTemplateForm` — `src/Fractale/Form.elm`
- `ColumnDraft`, `encodeColumnsJson`, `decodeColumnsJson` — `src/Codecs.elm` (shared between picker and Settings)
