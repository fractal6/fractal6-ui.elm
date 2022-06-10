# Changelog

All notable changes to this project will be documented in this file.
The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).


## [Unrealeased]

### New
- Orga quick switcher in a left menu.
- Landing of the User settings page !
- Support of multiple invitation on the invivation form.

### Changed 
- User url is now root uri ie fractale.co/dtrckd
- Tension modal UI improved
- Signup has email input in first choice, before username...

### Fixed
- Token update in JoinOrga and  component using port subscription


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
- Small navigation facilitation added in the header where the current path in the organization is displayed.

### Changed
- Only the roles of coordinators can edit tensions.

### Fixed
- Small bug fix that made us have to update our mdp every day! (updating the token after 30 days today)


## v0.3

### New
- Quick Help in top menu
- Collect user feedback directly in the platform (top menu)
- Users can create new (personal) organization
- Users can leave their roles
- Circle/role can be archived by user with correct rights.
- Many bug fixes

