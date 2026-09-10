# Action cards

`Components.ActionCard` is a stateless, domain-independent button/link component.
`view attributes config` takes icon, title, description, featured emphasis, and a typed
action: `Link url` renders a native anchor; `Click msg` renders `button type="button"`.
Extra attributes support IDs, disabled buttons and link interception. Layout belongs to
the caller; use Bulma columns for responsive grids.

```elm
ActionCard.view []
    { icon = "icon-layout"
    , title = T.createNewProject
    , description = T.welcomeProjectHint
    , featured = True
    , action = ActionCard.Link projectUrl
    }
```

`assets/sass/components/_action-card.scss` owns the shared appearance. It neutralizes
Bulma's link-only box shadows, uses theme tokens, and keeps an explicit keyboard focus
outline for both links and buttons.

## Signup welcome

`Fractale.Welcome.view` renders the success heading and two action cards for both
`Pages.Verification` and `Components.AuthModal`. It only needs a username and a
`linkAttributes` callback. Verification leaves links native (`always []`); the modal
intercepts them to close before navigating. An explicit destination suppresses the
usual current-page refresh. The modal's “Got it” button still closes and refreshes the
invitation page.

`Org.Overview.viewWelcomeCards` uses the same component without changing its permissions,
visibility rules, or creation handlers.
