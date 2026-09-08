# Component Communication Architecture

How messages and state travel in the Fractale Elm codebase: The Elm Architecture with
elm-spa v4 for routing.

```
Main.elm                     router + bootstrap (owns Key + Url)
  → Global.elm               shared session state, layout (navbar, body, footbar)
    → Pages/*.elm            route logic, reads Global.Model through Page.component
      → Components/*.elm     reusable stateful components
      → Form/*.elm           reusable form components
```

Two other ways to look at the same thing:

- Composition: `Main.view = Global.view (navbar + Pages.view page global + footbar)`, and
  pages emit `Cmd Global.Msg` upward.
- Wrapping: each parent owns a constructor per child (`Global Global.Msg`, `Page Pages.Msg`,
  `UserInputMsg UserInput.Msg`) and re-maps the child through that constructor everywhere it
  surfaces: `Html.map` in the view, `Cmd.map` on the returned commands, `Sub.map` on the
  subscriptions.

## Component patterns

**State wrapper.** Components expose an opaque `type State = State Model` and keep `Model`
private, so parents cannot poke at internals. `init` builds it, setter functions
(`State -> State`) are the only way in.

**Out record.** `update : Apis -> Msg -> State -> ( State, Out )` where `Out` carries
`cmds` (local), `gcmds` (application-wide `GlobalCmd`s) and an optional `result` for the
parent. The `noOut` / `out0` / `out1` / `out2` helpers cover the common shapes.

**Submit guard.** `OnSubmit isSendable next` is only a routing gate: the `isSendable` flag is
baked into the rendered handler and is stale on a double click/tap (the second click lands
before the rAF re-render disables the button). The operation branch (`SubmitX time`) is the
real guard: it re-checks `Loading.isLoading` on the result it owns and returns `noOut`.
Retries (`PushTension`, token refresh) either bypass the gate or reset that result first.
When the view needs the same predicate (`canSubmitTension`, `canSubmitTensionComment`, …),
share it instead of duplicating.

**GlobalCmd.** The bridge from a component to `Global.elm` (`DoFocus`, `DoNavigate`,
`DoUpdateTree`, `DoPushSystemNotif`, …). Pages translate them in `mapGlobalOutcmds`.

**Interceptor.** Components that outlive the page hold a stale `SessionCommon`. Fix it at the
Org page level: match the specific message (typically `OnOpen`) and inject fresh session data
with a setter before calling the component's `update`. See `docs/draft-persistence.md`.

**Handler / config records.** Stateless components take their callbacks and options as
records (`{ onReplaceUrl, onScrollToTop, … }`) rather than positional arguments — named,
extensible, order-independent. A component that only renders (Navbar) has no `State`, `Msg`
or `update` at all: `view : Data -> Handlers msg -> Html msg`.

## Naming conventions

| Prefix | Usage | Example |
|--------|-------|---------|
| `On` | Page event handlers (user actions) | `OnClick`, `OnSubmit`, `OnOpen` |
| `Do` | Global action to perform | `DoFocus`, `DoNavigate`, `DoPatch` |
| `Got` / `Ack` | Response handlers | `GotData`, `AckSubmit` |
| `Set` | State setters | `SetField`, `SetValue` |

Types: `State` (opaque wrapper), `Model` (private), `Msg`, `Out`, `Config` / `Op` (options).

## Files reference

| Directory | Purpose |
|-----------|---------|
| `src/Components/` | Reusable stateful components |
| `src/Form/` | Form-specific components |
| `src/Org/` | Organization page sub-views |
| `src/User/` | User page sub-views |
| `src/Pages/` | Route entry points |
