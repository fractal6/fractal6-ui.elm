module Elm.TensionPickerTest exposing (..)

import Components.TensionPicker exposing (completeWords, filterTensions)
import Expect
import ModelSchema exposing (PinTension)
import Schema.Enum.TensionStatus as TensionStatus
import Schema.Enum.TensionType as TensionType
import Test exposing (..)


tension : String -> String -> PinTension
tension id title =
    { id = id, title = title, createdAt = "", createdBy = { username = "u" }, type_ = TensionType.Operational, status = TensionStatus.Open, labels = Nothing }


tensions : List PinTension
tensions =
    [ tension "0x1a" "Deploy the API", tension "0x2b" "Fix login", tension "0xde1" "Other" ]


suite : Test
suite =
    describe "TensionPicker.filterTensions"
        [ test "empty pattern keeps everything" <|
            \_ -> filterTensions "" tensions |> List.map .id |> Expect.equal [ "0x1a", "0x2b", "0xde1" ]
        , test "matches the title (case-insensitive) or the id prefix" <|
            \_ -> filterTensions "DE" tensions |> List.map .id |> Expect.equal [ "0x1a", "0xde1" ]
        , test "the id matches by prefix only" <|
            \_ -> filterTensions "b" tensions |> List.map .id |> Expect.equal []
        , test "words are ORed, best matches first" <|
            \_ -> filterTensions "deploy fix login" tensions |> List.map .id |> Expect.equal [ "0x2b", "0x1a" ]
        , test "completeWords keeps the words followed by a space" <|
            \_ -> List.map completeWords [ "", "dep", "Deploy ", "deploy  ap" ] |> Expect.equal [ [], [], [ "deploy" ], [ "deploy" ] ]
        , test "caps the list at 15" <|
            \_ -> filterTensions "" (List.repeat 30 (tension "0x1" "t")) |> List.length |> Expect.equal 15
        ]
