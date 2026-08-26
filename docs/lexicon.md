# Lexicon — User-Defined Terminology

Organisations can rename the vocabulary the UI uses: "Tension" → "Ticket", "Mandate" →
"Charter", etc. Substitution happens at render time, through the i18n system.

```
localStorage("lexicon") → Elm flag → SessionCommon.lexicon (Dict String String) → Text.* functions
```

`public/index.js` reads the flag, `Session.elm` decodes it into `SessionCommon.lexicon`, and
the generated `Text.elm` functions substitute at the call site.

## Authoring

Lexicon-aware entries in `i18n/i18n.toml` use the `{{_Term_}}` placeholder, e.g.
`en = "Search {{_tension_}}s"`. `i18n.py` detects them and generates a function taking
`Dict String String` that substitutes with `jorgengranseth/elm-string-format`, falling back to
`default_lexicon` in `i18n.py`. Case variants (`tension` / `Tension`) are separate keys so
capitalisation stays contextual.

Regenerate with `python i18n.py gen -w -l en`.

## Backend

`Node.lexicon` is a JSON string on the root node
(`{"Tension":"Ticket","tension":"ticket",…}`), written through
`POST /auth/setlexicon` with `{ nameid, val }`.

## Reading it

- Pages: `global.session.common.lexicon`
- Components holding a `SessionCommon`: `session.lexicon`
- `Org/Settings.elm`: `model.lexicon`, copied at init
