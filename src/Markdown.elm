{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2024 Fractale Co

   This file is part of Fractale.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU Affero General Public License as
   published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Affero General Public License for more details.

   You should have received a copy of the GNU Affero General Public License
   along with Fractale.  If not, see <http://www.gnu.org/licenses/>.
-}


module Markdown exposing (renderMarkdown, setMdCheckbox)

import Bulk.Codecs exposing (FractalBaseRoute(..), toLink)
import Extra exposing (regexContains, regexFromString, regexfirstMatchLength)
import Generated.Route as Route exposing (toHref)
import Html exposing (Html, a, div, i, input, label, li, span, table, text, u, ul)
import Html.Attributes exposing (attribute, checked, class, disabled, href, rel, target, title, type_)
import Html.Lazy as Lazy
import List.Extra as LE
import Markdown.Block as Block
import Markdown.Html
import Markdown.Parser as Markdown
import Markdown.Renderer exposing (defaultHtmlRenderer)
import Maybe exposing (withDefault)
import Regex
import String exposing (startsWith, toLower)
import Url exposing (percentDecode)


urlRegex : Regex.Regex
urlRegex =
    regexFromString "(^|[^\\w\\[\\`])https?://[À-ÿ\\w\\-\\+\\.\\?\\#/@~&=:%_]+"


userRegex : Regex.Regex
userRegex =
    regexFromString "(^|[^\\w\\[\\`])@[\\w\\-\\.]+\\b"


tensionRegex : Regex.Regex
tensionRegex =
    regexFromString "(^|[^\\w\\[\\`])0x[0-9a-f]+"


renderMarkdown : String -> String -> Html msg
renderMarkdown style content =
    Lazy.lazy2 renderMarkdown_ style content


renderMarkdown_ : String -> String -> Html msg
renderMarkdown_ style content =
    case
        content
            |> frac6Parser
            |> Markdown.parse
            |> Result.mapError deadEndsToString
            |> Result.andThen
                (\ast ->
                    if style |> String.split " " |> List.member "f6-error" then
                        Markdown.Renderer.render (frac6Renderer style False) ast

                    else
                        Markdown.Renderer.render (frac6Renderer style True) ast
                )
    of
        Ok rendered ->
            div [ class ("content markdown-body " ++ style) ] rendered

        Err errors ->
            text errors


renderMdDefault : String -> String -> Html msg
renderMdDefault style content =
    case
        content
            |> Markdown.parse
            |> Result.mapError deadEndsToString
            |> Result.andThen
                (\ast ->
                    Markdown.Renderer.render (frac6Renderer style False) ast
                )
    of
        Ok rendered ->
            span [ class ("content markdown-body fix-inline " ++ style) ] rendered

        Err errors ->
            text errors


deadEndsToString deadEnds =
    deadEnds
        |> List.map Markdown.deadEndToString
        |> String.join "\n"


frac6Renderer : String -> Bool -> Markdown.Renderer.Renderer (Html msg)
frac6Renderer style recursive =
    { defaultHtmlRenderer
        | link =
            -- Differential external and internal link
            \link content ->
                let
                    lk =
                        toLower link.destination

                    attrs =
                        if
                            not (startsWith "https://fractale.co" lk || startsWith "http://fractale.co" lk)
                                && (startsWith "https://" lk || startsWith "http://" lk)
                        then
                            [ href link.destination, target "_blank", rel "noopener" ]

                        else
                            [ href link.destination, class "is-link" ]
                in
                case link.title of
                    Just t ->
                        a (title t :: attrs) content

                    Nothing ->
                        a attrs content
        , unorderedList =
            \items ->
                ul []
                    (items
                        |> List.map
                            (\item ->
                                case item of
                                    Block.ListItem task children ->
                                        let
                                            checkbox =
                                                case task of
                                                    Block.NoTask ->
                                                        Nothing

                                                    Block.IncompleteTask ->
                                                        Just <|
                                                            label []
                                                                [ input
                                                                    [ type_ "checkbox"
                                                                    , checked False
                                                                    , class "checkbox_readonly"

                                                                    --, disabled True
                                                                    ]
                                                                    []
                                                                ]

                                                    Block.CompletedTask ->
                                                        Just <|
                                                            label []
                                                                [ input
                                                                    [ type_ "checkbox"
                                                                    , checked True
                                                                    , class "checkbox_readonly"
                                                                    ]
                                                                    []
                                                                ]
                                        in
                                        case checkbox of
                                            Just cb ->
                                                li [ class "is-checkbox" ] [ cb, div [] children ]

                                            Nothing ->
                                                li [] children
                            )
                    )
        , table = \x -> div [ class "table-container" ] [ table [] x ]
        , text =
            \t ->
                if recursive then
                    mardownRoutine
                        style
                        ( urlRegex, autoLink )
                        [ ( userRegex, userLink )
                        , ( tensionRegex, tensionLink )

                        --, ( "\\bo/[0-9a-zA-Z\\-_\]+", circleLink )
                        ]
                        t

                else
                    text t
        , html =
            -- Html tag supported in the text
            Markdown.Html.oneOf
                [ Markdown.Html.tag "i"
                    (\cls content ->
                        span [] ([ i [ class cls ] [] ] ++ content)
                    )
                    |> Markdown.Html.withAttribute "class"

                -- @DEBUG p/inline issue, see https://github.com/dillonkearns/elm-markdown/issues/50
                -- Add Tag: primary, success, info, link, warning, danger.
                -- Add tag: sub, sup, tiny, small, medium, large, huge
                --
                , Markdown.Html.tag "u"
                    (\cls content ->
                        u [ class (withDefault "" cls) ] content
                    )
                    |> Markdown.Html.withOptionalAttribute "class"
                , Markdown.Html.tag "span"
                    (\cls content ->
                        span [ class (withDefault "" cls) ] content
                    )
                    |> Markdown.Html.withOptionalAttribute "class"
                , Markdown.Html.tag "div"
                    (\cls content ->
                        div [ class (withDefault "" cls) ] content
                    )
                    |> Markdown.Html.withOptionalAttribute "class"
                ]
    }


mardownRoutine : String -> ( Regex.Regex, Regex.Match -> String -> String ) -> List ( Regex.Regex, Regex.Match -> String -> String ) -> String -> Html msg
mardownRoutine style rep next_replacers content =
    let
        reg =
            Tuple.first rep

        replacer =
            Tuple.second rep

        matches =
            Regex.find reg content
    in
    -- Split on the regex (and append the regex replacer)
    Regex.split reg content
        |> List.indexedMap
            (\i next_content ->
                [ case LE.uncons next_replacers of
                    Just ( next_replacer, rest_replacers ) ->
                        -- Keep going the regex matching on that part
                        mardownRoutine style next_replacer rest_replacers next_content

                    Nothing ->
                        -- No more regex replacer
                        text next_content
                ]
                    ++ (case LE.getAt i matches of
                            Just match ->
                                -- regex replacement
                                let
                                    reg_replacement =
                                        replacer match content
                                in
                                -- Fix because left space are ignored  in renderMdDefault...
                                -- Needed for regex that do not support word boundary (\b).
                                -- Word boundary based regex at the start of string was removed in favor of a sub-reg match
                                -- bacause of space inconsistence... (splited but present on match.)
                                if String.left 1 reg_replacement == " " then
                                    [ text " ", renderMdDefault style (String.dropLeft 1 reg_replacement) ]

                                else
                                    [ renderMdDefault style reg_replacement ]

                            Nothing ->
                                []
                       )
            )
        |> List.concat
        |> span []



--
-- Replacer routine
--


frac6Parser : String -> String
frac6Parser content =
    content
        -- Username format
        --|> Regex.replace (regexFromString "(^|\\s|[^\\w\\[\\`])@([\\w\\-\\.]+)\\b") userLink
        -- Tension format
        --|> Regex.replace (regexFromString "\\b0x[0-9a-f]+") tensionLink
        -- Autolink
        --|> Regex.replace urlRegex autoLink
        -- Escape "_" in link to give the priority to autolink
        |> escapeLinks
        -- Force line break (except for Table)
        |> Regex.replace (regexFromString "\n[^\n|]") (\m -> "  " ++ m.match)


autoLink : Regex.Match -> String -> String
autoLink m full =
    let
        match =
            m.match

        ( parts, right ) =
            if List.member (String.right 1 match) [ ".", "," ] then
                ( String.dropRight 1 match, String.right 1 match )

            else
                ( match, "" )

        ( left, link ) =
            if String.left 1 parts /= "h" then
                ( String.left 1 parts, String.dropLeft 1 parts )

            else
                ( " ", parts )

        decodedLink =
            percentDecode link |> withDefault link
    in
    if String.slice (m.index - 2) m.index full == "](" then
        match

    else
        left
            ++ "["
            ++ decodedLink
            ++ "]"
            ++ "("
            ++ decodedLink
            ++ ")"
            ++ right


userLink : Regex.Match -> String -> String
userLink m full =
    let
        match =
            m.match

        ( parts, right ) =
            if List.member (String.right 1 match) [ ".", "-" ] then
                ( String.dropRight 1 match, String.right 1 match )

            else
                ( match, "" )

        ( left, username ) =
            if String.left 1 parts /= "@" then
                ( String.left 1 parts, String.dropLeft 1 parts )

            else
                ( " ", parts )
    in
    left
        ++ "["
        ++ username
        ++ "]"
        ++ "("
        ++ toLink UsersBaseUri (String.dropLeft 1 username) []
        ++ ")"
        ++ right


tensionLink : Regex.Match -> String -> String
tensionLink m full =
    let
        match =
            m.match

        ( left, tid ) =
            if String.left 1 match /= "0" then
                ( String.left 1 match, String.dropLeft 1 match )

            else
                ( " ", match )
    in
    left
        ++ "["
        ++ tid
        ++ "]"
        ++ "("
        ++ (Route.Tension_Dynamic_Dynamic { param1 = "", param2 = tid } |> toHref)
        ++ ")"


circleLink : Regex.Match -> String -> String
circleLink m full =
    let
        match =
            m.match

        ( left, tid ) =
            if String.left 1 match /= "o" then
                ( String.left 1 match, String.dropLeft 1 match )

            else
                ( " ", match )
    in
    left
        ++ "["
        ++ tid
        ++ "]"
        ++ "("
        -- TODO: split on / to know which route to use
        --++ (Route.Org { param1 = "", param2 = tid } |> toHref)
        ++ ")"



--
-- Parsing
--


{-| Escape \_ in link !
-}
escapeLinks : String -> String
escapeLinks input =
    let
        escapeUnderscores url =
            String.replace "_" "\\_" url

        replaceUnderscores url text =
            String.replace url (escapeUnderscores url) text
    in
    input
        |> Regex.find urlRegex
        |> List.foldl (\match acc -> replaceUnderscores match.match acc) input


{-| Function to set checkbox at the checkbox posisiont (checkbox count)
-}
setMdCheckbox : { position : Int, isChecked : Bool, cid : String } -> String -> String
setMdCheckbox cb markdown =
    let
        -- Split the markdown into lines
        markdownLines =
            String.lines markdown

        -- Checkbox value based on shouldCheck
        checkboxValue c =
            if cb.isChecked then
                c ++ " [x]"

            else
                c ++ " [ ]"

        -- Function to update the line containing the nth checkbox
        updateLines : List String -> Int -> Int -> List String -> List String
        updateLines remainingLines currentIndex targetIndex updatedLines =
            case remainingLines of
                [] ->
                    updatedLines

                lineContent :: rest ->
                    let
                        cbPattern =
                            "^(\\-|\\*|\\+)\\s+\\[[ x]\\]"

                        -- Trim leading spaces from the line
                        trimmedLine =
                            String.trimLeft lineContent

                        -- Check if this line contains a checkbox
                        updatedLine =
                            if regexContains cbPattern trimmedLine && currentIndex == targetIndex then
                                let
                                    -- Calculate the leading spaces from the line
                                    leadingSpacesCount =
                                        String.length lineContent - String.length trimmedLine

                                    leadingSpaces =
                                        String.left leadingSpacesCount lineContent

                                    match_len =
                                        regexfirstMatchLength cbPattern trimmedLine |> withDefault 0
                                in
                                String.append
                                    leadingSpaces
                                    (String.replace (String.left match_len trimmedLine) (checkboxValue (String.left 1 trimmedLine)) trimmedLine)

                            else
                                lineContent

                        nextUpdatedLines =
                            updatedLines ++ [ updatedLine ]

                        nextIndex =
                            if regexContains cbPattern trimmedLine then
                                currentIndex + 1

                            else
                                currentIndex
                    in
                    updateLines rest nextIndex targetIndex nextUpdatedLines

        -- Update the markdown lines
        updatedMarkdownLines =
            updateLines markdownLines 0 cb.position []
    in
    String.join "\n" updatedMarkdownLines
