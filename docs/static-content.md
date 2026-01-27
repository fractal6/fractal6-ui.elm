
# Static HTML Content

Static HTML content (e.g., welcome page descriptions, blog posts) can be fetched from the assets server and injected into Elm views.

## How it works

1. **Fetching**: Use `Requests.fetchStaticPage` to fetch HTML content from `api.assets`:
   ```elm
   fetchStaticPage apis "welcome" (GotStaticContent "static-welcome")
   -- Fetches: {api.assets}/welcome.html
   ```

2. **Rendering**: Since Elm 0.19 removed direct `innerHTML` support (see https://github.com/elm/html/issues/172), we use a port-based approach:
   - Create an empty div with a specific ID in the view:
     ```elm
     div [ id "static-welcome" ] []
     ```
   - When content is received, use the `Ports.setInnerHtml` port to inject HTML:
     ```elm
     Ports.setInnerHtml { id = "static-welcome", html = content }
     ```

3. **Assets Server**: Static HTML files are stored on the assets server (configured via `api.assets` in webpack). The backend serves these files as-is.

## Message pattern with partial application

Use partial application to pass the element ID without adding model state:

```elm
-- Msg type includes element ID as first parameter
| GotStaticContent String (Result Http.Error String)

-- Fetch with element ID via partial application
fetchStaticPage apis "welcome" (GotStaticContent "static-welcome")

-- Handler extracts the ID from the message
GotStaticContent elementId result ->
    case result of
        Ok content ->
            ( model, Ports.setInnerHtml { id = elementId, html = content }, Cmd.none )
        Err _ ->
            ( { model | staticContent = StaticFailure }, Cmd.none, Cmd.none )
```

## Example implementation

See `src/Pages/About.elm` for a complete example of:
- Fetching static content on page init
- Storing content state in the model (`StaticContentState` type)
- Rendering via the `setInnerHtml` port

## Guidelines

- Static HTML should contain only body content (no `<html>`, `<head>`, `<body>` tags)
- Use Bulma CSS classes for styling consistency
- Keep interactive elements (forms, buttons with actions) in Elm - use static content only for display purposes
- The port-based innerHTML approach is safe for server-controlled content from trusted sources
