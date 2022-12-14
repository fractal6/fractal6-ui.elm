-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Fractal.Enum.ErrorBla exposing (..)

import Json.Decode as Decode exposing (Decoder)


type ErrorBla
    = ContactCoordo
    | OrgaLimitReached
    | MemberLimitReached
    | EmailLimitReached
    | StorageLimitReached


list : List ErrorBla
list =
    [ ContactCoordo, OrgaLimitReached, MemberLimitReached, EmailLimitReached, StorageLimitReached ]


decoder : Decoder ErrorBla
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "ContactCoordo" ->
                        Decode.succeed ContactCoordo

                    "OrgaLimitReached" ->
                        Decode.succeed OrgaLimitReached

                    "MemberLimitReached" ->
                        Decode.succeed MemberLimitReached

                    "EmailLimitReached" ->
                        Decode.succeed EmailLimitReached

                    "StorageLimitReached" ->
                        Decode.succeed StorageLimitReached

                    _ ->
                        Decode.fail ("Invalid ErrorBla type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : ErrorBla -> String
toString enum____ =
    case enum____ of
        ContactCoordo ->
            "ContactCoordo"

        OrgaLimitReached ->
            "OrgaLimitReached"

        MemberLimitReached ->
            "MemberLimitReached"

        EmailLimitReached ->
            "EmailLimitReached"

        StorageLimitReached ->
            "StorageLimitReached"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe ErrorBla
fromString enumString____ =
    case enumString____ of
        "ContactCoordo" ->
            Just ContactCoordo

        "OrgaLimitReached" ->
            Just OrgaLimitReached

        "MemberLimitReached" ->
            Just MemberLimitReached

        "EmailLimitReached" ->
            Just EmailLimitReached

        "StorageLimitReached" ->
            Just StorageLimitReached

        _ ->
            Nothing
