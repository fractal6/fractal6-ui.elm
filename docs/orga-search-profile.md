# Org Search Bar on User Profile

## Overview
A search-as-you-type filter bar on the User Profile page (`/u/{username}`) that lets users quickly find organisations when they belong to 5 or more.

## Behavior
- The search bar only appears when the user has **5 or more** organisations.
- Typing filters the displayed org list in real-time using fuzzy matching (MiniSearch).
- Clearing the input restores the full list.
- Sorting (activity/newest) still works; changing the sort re-initializes the search index.

## Implementation

### JS (ports.js)
- New MiniSearch instance `session.qso` indexed on `nameid` with fields `name` and `nameid`.
- `INIT_ORGASEARCH` action: initializes the index with org data.
- `SEARCH_ORGAS` action: searches the index and sends results via `lookupOrgaFromJs` port.

### Elm Ports (Ports.elm)
- `initOrgaSearch : List NodeExt -> Cmd msg` — encodes orgs as `[{name, nameid}]` and sends to JS.
- `searchOrga : String -> Cmd msg` — sends search pattern to JS.
- `lookupOrgaFromJs` — incoming port receiving search results from JS.

### Profile.elm
- Model fields: `orgaFilter` (current input text), `orgaLookup` (Maybe list of matching nameids).
- On `GotNodes` success with 5+ orgs: initializes the MiniSearch index.
- `OnOrgaFilterInput ""`: resets filter, shows all orgs.
- `OnOrgaFilterInput pattern`: triggers JS search.
- `ChangeOrgaLookup nameids`: stores results; view filters orgs by matching nameids.

### i18n
- Key: `searchOrganisations` — "Search organisations" (EN) / "Rechercher des organisations" (FR)
