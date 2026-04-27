# Tension Templates

Pre-filled tension forms defined at the circle level. When templates exist, users see a template picker before creating a tension. Schema: `TensionTemplate` in `fractal6.graphql`.

## Where it lives

- **Settings CRUD**: `src/Org/Settings.elm` (`TemplatesMenu`) — create/edit/delete templates per circle, with inherited (ancestor/children) sections.
- **Template picker**: `src/Form/NewTension.elm` — two-step fetch: list via REST (`fetchTensionTemplatesTop` → `TensionTemplateLite`), then full content via GQL (`getTensionTemplateById` → `TensionTemplateFull`) on selection.
- **isTemplateTensionOnly**: `src/Org/Settings.elm` (Organisation tab) — hides "Blank tension" option when enabled.

## Key queries/mutations

- `getTensionTemplates` — `src/Query/QueryNode.elm`
- `addOneTensionTemplate`, `updateOneTensionTemplate`, `removeOneTensionTemplate` — `src/Query/PatchNode.elm`
- `fetchTensionTemplatesTop`, `fetchTensionTemplatesSub`, `setIsTemplateTensionOnly` — `src/Requests.elm`

## Key types

- `TensionTemplateLite` / `TensionTemplateFull` / `NodeRights.isTemplateTensionOnly` — `src/ModelSchema.elm`
- `TensionTemplateForm` — `src/Bulk.elm`
