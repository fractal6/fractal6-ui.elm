# Component Communication Architecture

This document describes the patterns used for component communication in the Fractale Elm codebase.

## Overview

The application follows The Elm Architecture (TEA) with elm-spa v4 for routing. The message flow hierarchy is:

```
Main.elm
  → Global.elm (global state, navbar, footbar)
    → Pages/*.elm (route-specific pages)
      → Components/*.elm (reusable components)
        → Form/*.elm (form components)
```

## Message Flow

### Top-Level Structure

- **Main.elm**: Entry point, handles URL changes, wraps Global and Page messages
- **Global.elm**: Manages session state, renders layout (navbar, body, footbar)
- **Pages/**: Route handlers, each with their own Model/Msg/update/view
- **Components/**: Reusable UI components with encapsulated state

### Message Wrapping

Parent modules wrap child messages using constructors:

```elm
-- In Main.elm
type Msg
    = Global Global.Msg
    | Page Pages.Msg

-- In update
Global globalMsg ->
    let
        ( global, globalCmd ) = Global.update globalMsg model.global
    in
    ( { model | global = global }, Cmd.map Global globalCmd )
```

## Component Patterns

### 1. Out Record Pattern

Most components in `src/Components/` return an `Out` record instead of just `Cmd Msg`:

```elm
type alias Out =
    { cmds : List (Cmd Msg)      -- Local commands
    , gcmds : List GlobalCmd     -- Global commands (affect app state)
    , result : Maybe ResultType  -- Data to return to parent
    }

-- Helper constructors
noOut : Out
noOut = Out [] [] Nothing

out0 : List (Cmd Msg) -> Out
out0 cmds = Out cmds [] Nothing

out1 : List GlobalCmd -> Out
out1 gcmds = Out [] gcmds Nothing

out2 : List (Cmd Msg) -> List GlobalCmd -> Out
out2 cmds gcmds = Out cmds gcmds Nothing
```

**Usage in update:**
```elm
update : Apis -> Msg -> State -> ( State, Out )
update apis msg (State model) =
    case msg of
        OnSubmit ->
            ( State model
            , out2 [ sendRequest ] [ DoNavigate "/success" ]
            )
```

### 2. Html.map Pattern

Parent components use `Html.map` to transform child messages:

```elm
-- In parent view
UserInput.view config model.userInput
    |> Html.map UserInputMsg

-- In parent Msg type
type Msg
    = UserInputMsg UserInput.Msg
    | ...

-- In parent update
UserInputMsg msg ->
    let
        ( data, out ) = UserInput.update apis msg model.userInput
    in
    ( { model | userInput = data }
    , out.cmds |> List.map (Cmd.map UserInputMsg) |> Cmd.batch
    , ...
    )
```

### 3. State Wrapper Pattern

Components use an opaque `State` type to encapsulate internal model:

```elm
-- Exposed
type State
    = State Model

-- Internal (not exposed)
type alias Model =
    { field1 : String
    , field2 : Int
    , ...
    }

-- Initialization
init : Config -> State
init config =
    initModel config |> State
```

This prevents parent components from directly accessing internal state.

### 4. GlobalCmd Pattern

Components can trigger application-wide actions through `GlobalCmd`:

```elm
type GlobalCmd
    = DoFocus String           -- Navigate to a node
    | DoNavigate String        -- Navigate to a URL
    | DoUpdatePath Path        -- Update session path
    | DoUpdateTree Tree        -- Update session tree
    | DoPushSystemNotif Notif  -- Show system notification
    | ...
```

### 5. Subscription Mapping

Subscriptions from child components are mapped in the parent:

```elm
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ ...
        , ActionPanel.subscriptions model.actionPanel
            |> List.map (Sub.map ActionPanelMsg)
            |> Sub.batch
        , SelectType.subscriptions
            |> List.map (Sub.map SelectTypeMsg)
            |> Sub.batch
        ]
```

## Configuration Patterns

### Record-Based Handlers

For components that need callbacks without internal state (like Navbar), use a handlers record:

```elm
type alias NavbarHandlers msg =
    { onReplaceUrl : String -> msg
    , onCloseOutdated : msg
    , onScrollToTop : msg
    , onScrollToBottom : msg
    }

view : SessionData -> NavbarHandlers msg -> Html msg
view session handlers =
    div []
        [ button [ onClick handlers.onScrollToTop ] [ text "Top" ]
        , ...
        ]
```

**Benefits:**
- Semantic naming (self-documenting)
- Easy to extend (add fields to record)
- Type-safe (compiler checks field names)
- Order-independent (unlike positional parameters)

### Config Records for Options

Components accept configuration through record parameters:

```elm
type alias Config =
    { labelText : Html Msg
    , placeholder : String
    , isRequired : Bool
    }

view : Config -> State -> Html Msg
view config (State model) =
    ...
```

## Naming Conventions

### Message Prefixes

| Prefix | Usage | Example |
|--------|-------|---------|
| `On` | Event handlers (user actions) | `OnClick`, `OnSubmit`, `OnOpen` |
| `Do` | Actions to perform | `DoFocus`, `DoNavigate`, `DoPatch` |
| `Got` / `Ack` | Response handlers | `GotData`, `AckSubmit` |
| `Set` | State setters | `SetField`, `SetValue` |

### Type Naming

- `State` - Opaque component state wrapper
- `Model` - Internal component model (not exposed)
- `Msg` - Component message type
- `Out` - Component output record
- `Config` / `Op` - Configuration options

## Pure View Components

Some components (like Navbar) are pure view functions without internal state:

```elm
-- No State type, no Msg type, no update function
view : Data -> Handlers msg -> Html msg
```

Use this pattern when:
- The component only renders data
- All interactions are handled by the parent
- No internal state management needed

## Files Reference

| Directory | Purpose |
|-----------|---------|
| `src/Components/` | Reusable stateful components |
| `src/Form/` | Form-specific components |
| `src/Org/` | Organization page sub-views |
| `src/User/` | User page sub-views |
| `src/Pages/` | Route entry points |
