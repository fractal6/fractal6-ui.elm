# Tension Templates

## Overview

Tension templates allow organizations to define pre-filled tension forms at the circle level. When templates exist, users are presented with a template picker before creating a new tension.

## Schema

The `TensionTemplate` GraphQL type has:
- `name` — internal identifier for the template
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
- Form fields: name, title, comment (textarea), type selector, is_recursive checkbox
- Table view with name, title, type, recursive flag, and edit/delete actions
- Query: `getTensionTemplates` in `src/Query/QueryNode.elm`
- Mutations: `addOneTensionTemplate`, `updateOneTensionTemplate`, `removeOneTensionTemplate` in `src/Query/PatchNode.elm`

### Template Picker (Tension Creation Form)
- **Location**: `src/Form/NewTension.elm`
- When templates exist for the target circle, a picker is shown instead of the blank form
- Each template is displayed as a clickable card with name and title preview
- Selecting a template pre-fills: title, comment, type, labels, assignees
- A "Blank tension" option allows creating from scratch (unless restricted)
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

Added in `i18n/i18n.toml`: `tensionTemplate`, `tensionTemplates`, `newTensionTemplate`, `noTensionTemplates`, `tensionTemplatesInfoHeader`, `tensionTemplatesInfoDoc`, `templateDeleteInfoHeader`, `confirmDeleteTemplate`, `isRecursive`, `isRecursiveHelp`, `selectTemplate`, `blankTension`, `blankTensionHelp`, `isTemplateTensionOnly`, `isTemplateTensionOnlyHelp`, `templateName`, `templateTitle`, `templateComment`.
