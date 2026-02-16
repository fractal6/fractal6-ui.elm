# Lexicon Feature — User-Defined Terminology

**Status**: Active
**Introduced**: v0.8.5 (2025-10-12)

## Overview

The lexicon feature allows organisations to customise the vocabulary used throughout the Fractale UI. For example, an organisation could rename "Tension" to "Ticket", "Issue", or "Mandate" to "Charter", "Readme", etc. Substitutions are applied dynamically at render time via the i18n system.

## How It Works

### Data flow

```
localStorage("lexicon")  →  Elm flags  →  SessionCommon.lexicon (Dict String String)  →  Text.* functions
```

1. **Startup**: `public/index.js` reads `localStorage.getItem("lexicon")` and passes it to Elm as a flag.
2. **Decoding**: `Session.elm` decodes the JSON into `Dict String String` and stores it in `SessionCommon.lexicon`.
3. **Rendering**: Translation functions in `Text.elm` that contain lexicon placeholders accept the dict and perform substitution at call-site.

### i18n placeholder syntax

In `i18n/i18n.toml`, lexicon-aware entries use the pattern `{{_TERM_}}`:

```toml
[tensions]
  en = "{{_Tension_}}s"
  fr = "{{_Tension_}}s"

[moveTension]
  en = "Move {{_tension_}}"
  fr = "Deplacer la {{_tension_}}"

[searchTensions]
  en = "Search {{_tension_}}s"
  fr = "Rechercher des {{_tension_}}s"
```

### Generated Elm code

`i18n.py` detects `{{_TERM_}}` patterns and generates functions with a `Dict String String -> String` signature:

```elm
tensions : Dict String String -> String
tensions lexicon =
    "{{_Tension_}}s"
        |> String.Format.namedValue "_Tension_" (withDefault "Tension" (Dict.get "Tension" lexicon))
```

The substitution uses `jorgengranseth/elm-string-format` (v1.0.1). Each term falls back to a hardcoded default defined in `i18n.py`:

```python
default_lexicon = {
    "tension": "tension",
    "Tension": "Tension",
    "mandate": "mandate",
    "Mandate": "Mandate",
}
```

Case variants are separate keys so that capitalisation is preserved contextually.

### Regenerating Text.elm

```bash
python i18n.py gen -w -l en
```

### Backend

- `Node.lexicon` is a JSON string stored on the root node (e.g., `{"Tension":"Ticket","tension":"ticket","Mandate":"Charter","mandate":"charter"}`)
- `POST /auth/setlexicon` endpoint accepts `{ "nameid": rootid, "val": lexiconJsonString }`


## Key implementation details

### Accessing lexicon in different contexts

- **Pages** (Org/Tension.elm, etc.): `session.common.lexicon` or `global.session.common.lexicon`
- **Components** (with `SessionCommon`): `model.session.lexicon` or `session.lexicon`
- **Org/Settings.elm**: `model.lexicon` (stored in Model at init time from `session.common.lexicon`)
