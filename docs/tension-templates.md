# Tension Templates

## Overview

Tension templates allow organizations to define pre-filled tension forms at the circle level. When templates exist, users are presented with a template picker before creating a new tension.

## Schema

The `TensionTemplate` GraphQL type has:
- `name` — internal identifier for the template
- `description` — optional short description to help identify the template
- `title` — pre-filled tension title
- `comment` — pre-filled tension description/body
- `type_` — tension type (Operational, Governance, Help, Alert, Announcement)
- `is_recursive` — when true, the template is also available in sub-circles (default: true)
- `labels` — optional pre-filled labels
- `assignees` — optional pre-filled assignees
- `nodes` — circles where the template is attached

## Features

### Settings CRUD (Org Settings > "Tension templates")
- **Location**: `src/Org/Settings.elm`, menu entry `TemplatesMenu`
- Create, edit, and delete tension templates per circle
- Form fields: name, description (optional), title, comment (textarea), type selector, is_recursive checkbox
- Table view with name, description, title, type, recursive flag, and edit/delete actions
- Inherited templates: "Top" (ancestor) and "Sub" (children) sections shown via REST endpoints
- Query: `getTensionTemplates` in `src/Query/QueryNode.elm`
- REST: `fetchTensionTemplatesTop`, `fetchTensionTemplatesSub` in `src/Requests.elm`
- Mutations: `addOneTensionTemplate`, `updateOneTensionTemplate`, `removeOneTensionTemplate` in `src/Query/PatchNode.elm`

### Template Picker (Tension Creation Form)
- **Location**: `src/Form/NewTension.elm`
- **Two-step fetch**: Templates are browsed via REST (`fetchTensionTemplatesTop`), which returns `TensionTemplateLite` (lightweight: id, name, description, is_recursive, nodes). On selection, full content is fetched via GQL (`getTensionTemplateById`) returning `TensionTemplateFull`
- **Recursive inheritance**: Ancestor templates with `is_recursive=true` appear in the picker for all sub-circles
- **Caching**: Templates are only fetched when `model.templates == RemoteData.NotAsked` (reset on form close)
- Picker uses stacked list layout with `modal-card-foot` footer (matching the form's modal pattern)
- Each template shows name (bold) and description as subtitle
- Selecting a template shows a loading state, then pre-fills: title, comment, type, labels, assignees
- A "Blank tension" option allows creating from scratch (unless restricted)
- Keyboard accessible: template cards have `tabindex 0` and `onEnter` handlers
- A template indicator tag is shown in the form header with an option to clear/change template

### isTemplateTensionOnly (Global Settings)
- **Location**: `src/Org/Settings.elm`, in the "Organisation" settings tab
- Toggle switch to restrict tension creation to template-based only
- When enabled and templates exist, the "Blank tension" option is hidden from the picker
- REST endpoint: `setIsTemplateTensionOnly` in `src/Requests.elm`
- Stored on the root `Node.isTemplateTensionOnly` field

## Key Types

- `TensionTemplateLite` / `TensionTemplateFull` — in `src/ModelSchema.elm`
- `TensionTemplateForm` — in `src/Bulk.elm`
- `NodeRights.isTemplateTensionOnly` — extended in `src/ModelSchema.elm`
- `RNode.isTemplateTensionOnly` — for local graph / tension form access

## i18n Keys

Added in `i18n/i18n.toml`: `tensionTemplate`, `tensionTemplates`, `newTensionTemplate`, `noTensionTemplates`, `tensionTemplatesInfoHeader`, `tensionTemplatesInfoDoc`, `templateDeleteInfoHeader`, `confirmDeleteTemplate`, `isRecursive`, `isRecursiveHelp`, `selectTemplate`, `blankTension`, `blankTensionHelp`, `isTemplateTensionOnly`, `isTemplateTensionOnlyHelp`, `templateName`, `templateTitle`, `templateComment`, `templateDescription`, `templateDescriptionHelp`, `inheritedTemplates`, `subTemplates`.
