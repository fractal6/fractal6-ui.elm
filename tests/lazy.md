### Tests

- test if lazy viewNodeLine works with function/msg !? yes it is (see https://discourse.elm-lang.org/t/understanding-how-the-lazy-function-works/9620)
- test if lazy viewCommentOrEvent works with dynamical list !? indeed no !
- test if lazy Board.viewMediaTension (and draft) works !? (that's ok)
- test if viewGuestRow works !? (no for admin, hover change all the time !!)


# Problematic lazy call

- mediaTension | {noMsg = NoMsg}
- mediaOrga | {noMsg = NoMsg}

