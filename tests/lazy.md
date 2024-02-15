### Tests

- test if lazy viewNodeLine works with function/msg !? yes it is (see https://discourse.elm-lang.org/t/understanding-how-the-lazy-function-works/9620)
- test if lazy viewCommentOrEvent works with dynamical list !? indeed no !
- test if lazy Board.viewMediaTension (and draft) works !? (that's ok)
- test if viewGuestRow works !? (no for admin, hover change all the time !!)


# Improving lazy

All components that have an Op record in input parameters will usually fit bad with lazy call...
-> either use a `SupModel a` to use common model attribute, or pass directly the arguments.


