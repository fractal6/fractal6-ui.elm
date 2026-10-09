# User avatars

Users upload an avatar in their settings (Profile tab); it replaces the generated identicon
everywhere, including the graphpack. Hovering any avatar shows a user hover card.

## Data

- The file id comes with the user data: `avatar : Maybe String` on `User`, `Member`, `UserCtx`,
  `UserProfile`, `UserFull`, `UserCard`. No avatar = no field, no request.
- GraphQL: `avatarPayload` (`Query/QueryNode.elm`) selects `User.avatar { id }`, used by every user
  selection (`userPayload`, profile/full/roles payloads, comment authors, project collaborators).
- JSON (REST users, uctx, localStorage, graphpack): nested `avatar: {id}`
  (`Codecs.avatarDecoder`/`avatarEncoder`).
- Own avatar = `uctx.avatar`, refreshed by tokenack on every load. After an upload/remove, the
  settings page sends `UpdateUserSession { uctx | avatar = … }`.

## Rendering

`Fractale.View.getAvatar0..3 session user` (and `viewUser*`, `viewUserFull`, `viewProfileC`,
`UserSearchPanel.viewUsers`, all taking `SessionCommon`) render `<img loading="lazy">` at
`{file_server_url}/file/<id>` (immutable, browser-cached), or the identicon. Both carry
`data-user-card="<username>"`. The image fills the circle (`_shapes.scss`) over an empty box of
the identicon's size, so both sit on the same baseline.

## Upload — `src/User/Settings.elm`

- Avatar row at the top of the profile form: current avatar (`getAvatar2`), hidden
  `<input type=file data-avatar-input>` behind a "Change the avatar" button, plus Remove.
- `assets/js/avatars.js` `initAvatarCapture` re-encodes the picked file (`reencodeAvatar`):
  center-crop 256x256, WebP 0.85, JPEG fallback on a filled canvas when WebP is ignored (Safari),
  EXIF orientation applied and stripped. Undecodable input (e.g. HEIC) never gets uploaded.
- Result through `Ports.avatarFileFromJs` (`{file}` or `{error}`), then `Api.File.upload
  (UserAvatar username)`. Replace = upload only (the backend drops the previous one);
  Remove = `Api.File.delete`.

## Graphpack

`drawRoleAvatar` (`assets/js/graphpack_d3.js`) draws the first link's avatar in place of the
`@username` line on the roles whose names are drawn, and at the same place on the unnamed roles
one level below (when the zoomed node is focused). Users without an avatar (or while it loads)
get the identicon, ported to JS in `avatars.js` (`identicon`, same hash as `dividat/elm-identicon`,
LRU-cached for the last 50 users).
The `@username` text only stays when the circle is too small. Sizes: `avatarMinRayon` / `avatarMaxRayon`. One `Image` per id (`avatarImages`), one coalesced
redraw after decodes (`scheduleAvatarRedraw`, skipped during motion). The disc is kept on
`node.ctx.avatar` for the hover hit test (`getAvatarUnderPointer`).

## Hover card

- One card, `Components/UserCard.elm` (shown username + cache: one `queryUserCard` per user per
  session), held by `Global.elm` as `userCard` and rendered at the end of the layout as `#userCard`.
  No events, links only.
- `assets/js/avatars.js` `initUserCard`: delegated `pointerover`/`pointerout` on `[data-user-card]`,
  0.5s show delay, stays open while the pointer is on the card or a press started on it (text
  selection), hidden on scroll and on a press elsewhere. The large profile picture (`getAvatar3`)
  has no card.
  Sends the username (or null) through `Ports.userCardFromJs`, then places the card once Elm
  rendered it (`cardPosition`: below the anchor, flipped above, clamped), re-placed on resize.
- Graphpack: `hoverAvatar` calls the same `showUserCard`/`hideUserCard`; on the disc the card
  wins over the node tooltip.
- Touch: no hover, avatars keep linking to the profile.

## Known gap

Contract pages only have a username (`event.new`) and keep the identicon; the card still works.

## Tests

- `tests/Elm/AvatarCodecTest.elm` — codecs with/without `avatar`, round trips.
- `tests/Js/avatars.test.js` — identicon hash + LRU, re-encode + JPEG fallback, card placement, hover listener.
- `tests/Js/graphpackHitTest.test.js` — avatar disc hit test, one redraw for N loads.
