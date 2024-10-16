# Changelog

All notable changes to this project will be documented in this file.
The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).


## [0.8.4] - 2024-10-10

### 🚀 Features

- Add new view mode with url query `?view=embed` (iframe compatible)
- Open link in newtab in embed view mode

### 🐛 Bug Fixes

- *(Project/draft)* Show edit button for admin only


### ◀️ Revert

- Elm-graphql update as alias is not working well.

### 🚜 Refactor

- *(chore)* Elm-spa dependency + import Spa code
- *(chore)* Use session instead of conf.

### Hotfix

- Fix actionPanel to remove user from the members page
- Reorder users like labels in filter box
- Fix graphpack right click (context menu)
- *(ui)* Improve mobile fit
- *(ux)* Fix losing scrollbar after clicking link in auth modal
- *(ui/markdown)* Make checkbox correctly aligned.
- *(ui/markdown)* Better checkbox margins and flex container.


## [0.8.3] - 2024-06-18

### ⛰️  Features

- [ui] make roles accessible with dropdown menu in the tree left menu
- [ux] add a notification box if client version is outdated.
- [ux/edition] allow to check/uncheck chekboxes in messages (Markdown) by clicking.
- [ux/edition] use arrows to navigate in the user mention selection (@...).
- [members view] new option for Owner to grant user Owner.
- [members view] Show pending roles in user row if any.
- [members view] Use a modal confirmation before adding new owner

### 🐛 Bug Fixes

- [ui] Improves alignements and size and colors of various items
- [ui] fix order of selected labels in the filter box.
- [ui] make "open contracts hints" in Overview page only visible by circle coordos.
- [ui] Improve node tag descriptors view + improve tag alignement.
- [ux] Redirect to valid link when user link himself to a role from the members page.
- [ux] Add "Remove guest" entry in the ellipsis menu.
- [ux] fix duplicate tensions if tree menu open.
- [ux] Disable clickability for email-invited users without accounts in contract view
- [ux] Improve contract duplicate error messages and redirection + better user context reload on page refresh.
- [ux] Open link in a new tab in the contract confirm modal.
- [ux/orga map] Fix Graphpack navigation, and right-click menu.
- [tree view] Show open contracts when roles are collapsed.
- [ux/edition] when entering <enter> after en empty list, exit the list (markdown).
- [ui/edition] Parse and decode url automatically in markdown text.
- [project view] open tension in new tab link + i18n.
- [project view] Fix moving column in project board.
- [project view] Keep adding draft after typing enter.
- [members view] Fix adding role to users from members page.

### 🚜 Refactor

- [codefactor] simplify the modular port decoder
- Action panel + no new tension action for membersipNode.

### 🎨 Styling

- [tree view] Increase the menu width and item spacing.
- Choose organisation, with a 's'


## [0.8.2]

### New
- ux: add 'convert to tension' and 'remove draft' in the draft panel.

### Fixed
- tensions: Show again the number of closed tensions in the tensions view.
- tensions: Show again the tensions in the "By assignees" tab.
- board: fix the scroll position when clicking on a card option in long column.
- board: draft comment was not transfered to new tension form when converting.
- ui: usee 3-dot dropdown button in the circle card.
- ui: highlight the answer/comment in tension page when redirected from notification page.
- ui: stay on the selection view for tensions (Tensions|Jounal) in the overview page when navigating the organisation.

### Changed
- ux: Improve the graphpack UX by zooming one circle at a time.
- ui: better rendering optimization with lazy module.
- upgrade dependencies.


## [0.8.1]

### New
- ux: Use a  default Kanban template when creating new projects.
- ui: add an image screenschot on new project page.
- build: Add default theme in webpack config.

### Fixed 
- URI to fetch images in the About page


## [0.8]

### New
- [MAJOR]: Projects management feature: https://fractale.co/tension//0x33ea36
- Add user password reset, in user settings page.
- Show open contracts in circle about section, in overview page.
- New role for users can directly added in the members tab.

### Changed
- Better title for Document and Contracts tab, in tension page .
- data URI change from /data to /assets.
- show #members and #watcher in the organisation explore page.

### Fixed 
- Fix navigation context when navigating from Document or Contracts, in tension page.
- multiple code refactor: Comments is now a components, rename WebData -> RestData, and ++.


## [0.7.9]

### Changed
- Update package.json 

### Fixed
- make schema compatible with dgraph v22.
- contractPage loading race condition.
- add nameTooLong error + hint.
- show sub/top section of labels/roles in settings only if present. 

## [0.7.8]

### New
- Tree selector for targets/recipients for moveTension and newTension input
- Add hint helper beside Authority/Governance input for new Role/Circle and for visibility choice at org creation.

### Changed
- increase the max number of users showed in user mention search box.

### Fixed
- fix label context: https://fractale.co/tension/f6/0x349bda
- markdown unbreakable space ignore when AltGr is pressed.
- loading state not updated sometimes on tensions page loaded


## [0.7.7]

### New
- Search panel helper when mentionning user with "@" character. Available for
    - tension comment input.
    - new tension form.
    - contrant comment input.

### Fixed
- Accept "%" character in markdown autolink parsing.
- List autocompletion for large input value. 


## [0.7.6]

### Fixed
- fix markdown link parsing for french character (accents)
- open new tab by default in foorbar links
- light color themes fixes.


## [0.7.5]

### New
- Tension pin support.
- make tension category explicit in tension view.
- new https://fractale.co/help route to give feedback.

### Changed
- ui/refactor: the help button triggers now a dropdown before opening the modal.
- ui: slight header highlight for authored comments.

### Fixed
- tension can now be moved to the root circle.


## [0.7.4]

### Fixed
- ui: make labels clickable in tension view.
- ui: fix theme/color background color for tension type tag.
- ui: fix theme/color for Announcement.
- ui: fix break-word overflow in message box
- ui: fix left menu padding when navigating out and in organisation page + footer margin.
- ui: do not parse error mardown message with the f6 md parser.
- schema: remove obsolete Node.mandate and NodeFragment.children fields


## [0.7.3]

### New
- Tension editor (new tension and comment) are resizable, horizontally and vertically.
- User reactions (emojis) in tension/comments with a vital set of emojis proposed.
- Tension board view by assigned user.

### Changed
- When filtering tension to see either Announce, Alert or Governance types, use all status (open and close) by default instead of just open tensions as the the open/close status make less significance for those types.

### Fixed
- resize footer page when the left menu is open.


## [0.7.2]

### New
- ux: column headers in circle view (Tensions page) clickable to filter for only those.
- ux: label are clickable to see a filtered list of tension with this label.
- notification: add count hint for new tension, contract and assigned tensions.
- markdown: add new formating entry for lists.
- markdown: a help button that displays an overview of the markdown syntax.

### Changed
- improve error message for non authorized tensions creation.
- improve error message for duplicate name for new organisation.
- improve dark theme for emoticon and graph packing (organisation circles colors).
- improve message when leaving an organisation.
- inverse close and comment button position.

### Fixed
- About page layout overflow.
- notification back button.
- markdown "rich syntax" to better handler selection.


## [0.7]

### New
- schema: new Announcement tension type.
- Overview: add circle/role hints information in the description page.
- Helperbar: organisation watch/unwatch button.
- HelperBar: organisation count hints (total number of tensons, members and watching users)
- subscription: handle organisation unsubscribe (unwatch) from email link.
- TreeMenu: circle tension count hints.
- NewTension: improved tension type dropdown menu with hints.
- AuthModal: integrate the viewAuthNeeded modal in the this component. 

### Changed
- ui: icon for secret visibility.
- ui: color code for collector/bot role type.
- ui: tooltip and graphpack visual improvmen.
- ui: fix table border.

### Fixed
- authentification modal behaviour when unsubscribing while logged out.
- overview window position for mobile.


## [0.6.9]

### New
- Add a AGPLv3 License
- Tensions: Ability to move tension across circle in the "board view" with drag&drop.
- Tension: mentionable tension with new event
- Markdown: rich text edition (toolbar and markdown hints)
- TreeMenu: auto scroll to the focus circle

### Changed
- TreeMenu: Open menu when hovering the left border screen (like in notion.so)
- package.json: elm hot reload package that cause npm to need a legacy option to work

### Fixed
- table (+markdown) style
- Tensions: minimum height for the board view
- Markdown: remove list-style for checkbox + better left margin.
- Notifications: tooltip inversed in "mark as read" tag.


## [0.6.6]

### New
- New Welcome Page describing Fractale.
- Unread notification hints (unread events + peding contracts) in navbar, computed at page initialization, token refresh, an notifification page na vigation.

### Changed
- [ui] switched header in new tension form to be more consistent.

### Fixed
- improved translations
- improved mobile style and navigation


## [0.6.5]

### New
- [ux] add the orga map legend
- [ux] ActionPanel now works to manages roles in Members pages and HelperBar user's roles.
- [ux] add i18n translation dropdown when unlogged
- [ux] add signin button to the navbartop.

### Changed
- [ui] icon for owner and coordinator roles
- [ui/ux] better action panel icon, order and consistency
- [ux] helperBar user's role do not link the overview page anymore but open the action panel instead.
- [ux] A propos node title do not link to overview/tension page to avoid confusing user more than necessary.
- [ux] do not always reset the page scroll on navigation.
- [ux/tension] tension node hav no more a "edit title" button. Title edit automatically when blob is pushed.

### Fixed
- [ui/ux] tension receiver handle circle and roles (with color!)
- [ux] TreeMenu and OrgaMenu (side menu) focus updates.
- [ui] Improved traduction and notification messages.


## [0.6.3]

### New
- 18n translations, language settings and i18n build rules.
- Tensions/By Circles: circle view as "new tension" button shortcut in each columnn
- Tensions/By Circles: fix column order 
- TreeMenu: new method to get the list of nodes ordered as the tree menu.
- Tensions: search  button on search icon at right in the search bar.

### Changed
- User Signin prompt if landing on a contract page unlloged.
- Reinitialized scroll position when navigating

### Fixed
- Join organisation button initialization
- Loose scrool bar when browser navigation with an active modal 
- UI: wrapping long node names


## [0.6.2]

### New
- tension C-enter hint to submit
- unsubscribe from a tension from URL.

### Changed 
- [AuthModal] Do not close the modal on background click.
- [Helperbar] User played roles are now hidden in mobile view.
- [UserInput] Add a better hint message when no user found in the invitation dialogue.
- [Org/Members] merge the sub-circles roles into a solely member tables with a new column for sub-circles roles.
- [ui] remove modal-close button redundant with the cancel one, and add a little "delete" button for success box (now colored).
- [schema] Fetch the role color when member role element/button view.

### Fixed
- [css] left menu auto margin and border.
- [codefactor] Consolidate the NodeDoc pseudo component. Simplify th doctoolbar keeoping only edit and revision tabs . Inline Authority and governance dropdown
- [Comments] publish tension/comment wiht Ctrl+enter.
- [UpdateTension] add automatically the CommentPushed event to history if a comment is present from the form.
- prevent empty message or title to be post (except for updating comment which the current way to delete the post content...)
- prevent multiple submit of flooding the server on click of C-enter repeat


## v0.6

### New
- Orga quick switcher in a left menu.
- Orga tree menu in a left menu.
- Landing of the User settings page !
- Support of multiple invitation on the invivation form.
- Improve creatin of ne organiation. (Visibility choice)

### Changed 
- User url is now root uri ie fractale.co/dtrckd
- Tension modal UI improved
- Signup has email input in first choice, before username...

### Fixed
- Token update in JoinOrga and  component using port subscription
- many optimization and code factorization.


## v0.5

### New
- This changelog file
- Notfications email + in-app. 
- Show visibility in circle according the visibility of parents
- Organisation settings
- Templates roles are available in organisation settings (similar interface than labels management)
- Add a new tension sub-tab in the tensions mains tab: The circle view, which allows the tensions to be separated in a kanban like view.
- A info message can now be passed to the ConfirmModal component.
- New "edit node" entry in settings menu that redirect to the node's [Governance] tension.
- Icon marker on tension's filter buttons if they are different then default settings.
- Add the reset password page.

### Changed
- Improve the tension search bar filter
    - filter on open/closed tensions
    - filter on tension type
    - filter on tension labels (multiple selections)
    - filter on tension assigness (multiple selections)
    - search in all sub-circle/focused circle or just the focused one
- upgrade Bulma extension to @creativebulma repos.

### Fixed
- Authorisation to edit tensions are refined according who you are (see api)
    * creator of that tension
    * coordinator of the source or target circle
    * assigness
- Fix how duplicate label are managed in the label settings page.
- Tensions added when in the tensions tab view are now rendered in the list (circles view only).
- Change the url according the chosen view in the tensions page.
- Markdown support autolinks (external and internal), user tag (@someone)


## v0.4

### New
- Tension Labels management:
    - add labels (or tags) to assist in categorization/triage/search of tensions
    - tensions may have one or more labels
    - visual/memory color coding
    - description (optional) of the labels
    - labels are linked to circles and offer the set of labels accessible for tensions inside a circle.
    - The labels are inherited in the child circles
- Small navigation facilitation added in the header where the current path in the organisation is displayed.

### Changed
- Only the roles of coordinators can edit tensions.

### Fixed
- Small bug fix that made us have to update our mdp every day! (updating the token after 30 days today)


## v0.3

### New
- Quick Help in top menu
- Collect user feedback directly in the platform (top menu)
- Users can create new (personal) organisation
- Users can leave their roles
- Circle/role can be archived by user with correct rights.
- Many bug fixes

