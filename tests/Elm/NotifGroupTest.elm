module Elm.NotifGroupTest exposing (..)

import Expect
import ModelSchema exposing (EventKind(..), UserEvent)
import Schema.Enum.TensionEvent as TensionEvent
import Test exposing (Test, describe, test)
import User.Notifications exposing (groupNotifs)


ev : String -> TensionEvent.TensionEvent -> String -> UserEvent
ev id type_ tid =
    { id = id
    , isRead = False
    , event =
        [ TensionEvent
            { createdAt = ""
            , createdBy = { username = "bob" }
            , event_type = type_
            , tension = { id = tid, emitterid = "", receiver = { name = "", nameid = "", source = Nothing }, title = "" }
            , new = Nothing
            }
        ]
    }


comment : String -> String -> UserEvent
comment id tid =
    ev id TensionEvent.CommentPushed tid


notifGroupTests : Test
notifGroupTests =
    describe "groupNotifs"
        [ test "merges adjacent comments of a same tension only" <|
            \_ ->
                [ comment "1" "a", comment "2" "a", comment "3" "b", comment "4" "a", ev "5" TensionEvent.Closed "a", comment "6" "a" ]
                    |> groupNotifs
                    |> List.map (List.map .id)
                    |> Expect.equal [ [ "1", "2" ], [ "3" ], [ "4" ], [ "5" ], [ "6" ] ]
        , test "merges the creation with the comments, even when not the head event" <|
            \_ ->
                let
                    created =
                        ev "3" TensionEvent.Created "a"

                    labelFirst =
                        { created | event = (ev "3" TensionEvent.LabelAdded "a").event ++ created.event }
                in
                [ comment "1" "a", comment "2" "a", labelFirst, ev "4" TensionEvent.Created "b" ]
                    |> groupNotifs
                    |> List.map (List.map .id)
                    |> Expect.equal [ [ "1", "2", "3" ], [ "4" ] ]
        , test "other events are never merged" <|
            \_ ->
                [ ev "1" TensionEvent.Closed "a", ev "2" TensionEvent.Closed "a" ]
                    |> groupNotifs
                    |> List.map (List.map .id)
                    |> Expect.equal [ [ "1" ], [ "2" ] ]
        ]
