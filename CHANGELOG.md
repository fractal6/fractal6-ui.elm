# Changelog                                                                                                                                                                                                        

All notable changes to this project will be documented in this file.
The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).


## [Unrealeased]                                                                                                                                                                                                   

### New
- This changelog file
- add a new tension sub-tab in the tensions mains tab: The circle view, which allows the tensions to be separated in a kanban like view.
- A info message can now be passed to the ConfirmModal component.
- New "edit node" entry in settings menu that redirect to the node's [Governance] tension.
- icon marker on tension's filter buttons if they are different then default settings.
- add the reset password page.

### Changed
- Improve the tension search bar filter
	- filter on open/closed tensions
    - filter on tension type
    - filter on tension labels (multiple selections)
    - filter on tension assigness (multiple selections)
    - search in all sub-circle/focused circle or just the focused one
- upgrade Bulma extension to @creativebulma repos.

### Fixed
- authorisation to edit tensions are refined according who you are (see api)
    * creator of that tension
    * coordinator of the source or target circle
    * assigness
- fix how duplicate label are managed in the label settings page.
- Tensions added when in the tensions tab view are now rendered in the list (circles view only).
- change the url according the chosen view in the tensions page.

## [0.4] - ...

### New
- Tension Labels management:
    - add labels (or tags) to assist in categorization/triage/search of tensions
    - tensions may have one or more labels
    - visual/memory color coding
    - description (optional) of the labels
    - labels are linked to circles and offer the set of labels accessible for tensions inside a circle.
    - The labels are inherited in the child circles
- small navigation facilitation added in the header where the current path in the organization is displayed.

### Changed
- only the roles of coordinators can edit tensions.

### Fixed
- small bug fix that made us have to update our mdp every day! (updating the token after 30 days today)


## [0.3] - ...

### New
- Quick Help in top menu
- collect user feedback directly in the platform (top menu)
- users can create new (personal) organization
- users can leave their roles
- circle/role can be archived by user with correct rights.
- many bug fixes

