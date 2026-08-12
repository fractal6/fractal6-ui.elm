{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2026 Fractale Co

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


module Markdown exposing (escapeAmpersandsInHtmlBlocks, frac6Parser, parseMarkdown, processOutsideCodeBlocks, renderMarkdown, setMdCheckbox)

import Fractale.Codecs exposing (FractalBaseRoute(..), toLink)
import Utils.String exposing (regexContains, regexFromString, regexfirstMatchLength)
import Generated.Route as Route exposing (toHref)
import Html exposing (Html, a, details, div, i, img, input, label, li, ol, span, summary, table, text, u, ul)
import Html.Attributes as Attr exposing (alt, attribute, checked, class, disabled, href, rel, src, target, title, type_)
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


{-| Match bare '&' that are NOT already part of an HTML entity.
Negative lookahead skips named (&amp;), decimal (&#123;) and lowercase hex (&#x1f;) entities.
Uppercase hex (&#x1F;) is intentionally NOT protected because elm-markdown
rejects it — escaping it to &amp; is safer than letting the parser crash.
-}
ampersandRegex : Regex.Regex
ampersandRegex =
    regexFromString "&(?![a-zA-Z]+;|#[0-9]+;|#x[0-9a-f]+;)"


{-| Match ALL HTML tags registered as custom handlers in frac6Renderer.
Used to escape them inside code fences within HTML blocks, because
elm-markdown's HTML scanner does not respect code fences — it sees tags
in a code fence and tries to parse them as real HTML, breaking the code block.
The list must stay in sync with the Markdown.Html.oneOf handlers in frac6Renderer.
-}
htmlBlockTagRegex : Regex.Regex
htmlBlockTagRegex =
    regexFromString "<(/?(?:details|summary|div|span|u|i))\\b"


{-| Renderer configuration. `fileServerUrl` is prepended to relative
`/file/<id>` image targets; empty string means no rewrite.
-}
type alias RendererConfig =
    { style : String
    , recursive : Bool
    , fileServerUrl : String
    }


{-| Testable version of the markdown pipeline: preprocess + parse + render.
Returns Ok on success, Err with the error string on failure.
-}
parseMarkdown : String -> Result String (List (Html msg))
parseMarkdown content =
    content
        |> frac6Parser
        |> Markdown.parse
        |> Result.mapError deadEndsToString
        |> Result.andThen
            (\ast -> Markdown.Renderer.render (frac6Renderer { style = "", recursive = True, fileServerUrl = "" }) ast)


renderMarkdown : String -> String -> String -> Html msg
renderMarkdown fileServerUrl style content =
    Lazy.lazy3 renderMarkdown_ fileServerUrl style content


renderMarkdown_ : String -> String -> String -> Html msg
renderMarkdown_ fileServerUrl style content =
    case
        content
            |> frac6Parser
            |> Markdown.parse
            |> Result.mapError deadEndsToString
            |> Result.andThen
                (\ast ->
                    let
                        recursive =
                            not (style |> String.split " " |> List.member "f6-error")
                    in
                    Markdown.Renderer.render (frac6Renderer { style = style, recursive = recursive, fileServerUrl = fileServerUrl }) ast
                )
    of
        Ok rendered ->
            div [ class ("content markdown-body " ++ style) ] rendered

        Err errors ->
            text errors


renderMdDefault : String -> String -> String -> Html msg
renderMdDefault fileServerUrl style content =
    case
        content
            |> Markdown.parse
            |> Result.mapError deadEndsToString
            |> Result.andThen
                (\ast ->
                    Markdown.Renderer.render (frac6Renderer { style = style, recursive = False, fileServerUrl = fileServerUrl }) ast
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


frac6Renderer : RendererConfig -> Markdown.Renderer.Renderer (Html msg)
frac6Renderer config =
    -- see https://github.com/dillonkearns/elm-markdown/blob/master/README.md
    -- for default markdown renderer details
    { defaultHtmlRenderer
        | image =
            -- Prefix relative `/file/<id>` paths with the file server URL
            -- so attachments render against the file server, not the app domain.
            \imageInfo ->
                let
                    resolvedSrc =
                        if config.fileServerUrl /= "" && startsWith "/file/" imageInfo.src then
                            config.fileServerUrl ++ imageInfo.src

                        else
                            imageInfo.src

                    baseAttrs =
                        [ src resolvedSrc, alt imageInfo.alt ]
                in
                case imageInfo.title of
                    Just t ->
                        img (title t :: baseAttrs) []

                    Nothing ->
                        img baseAttrs []
        , link =
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
        , orderedList =
            \startingIndex items ->
                ol
                    [ Attr.start startingIndex ]
                    (items
                        |> List.map
                            (\itemBlocks ->
                                li [] itemBlocks
                            )
                    )
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
                if config.recursive then
                    mardownRoutine
                        config
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
                , Markdown.Html.tag "details"
                    (\content ->
                        details [ class "md-details" ] content
                    )
                , Markdown.Html.tag "summary"
                    (\content ->
                        summary [] content
                    )
                ]
    }


mardownRoutine : RendererConfig -> ( Regex.Regex, Regex.Match -> String -> String ) -> List ( Regex.Regex, Regex.Match -> String -> String ) -> String -> Html msg
mardownRoutine config rep next_replacers content =
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
                        mardownRoutine config next_replacer rest_replacers next_content

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
                                    [ text " ", renderMdDefault config.fileServerUrl config.style (String.dropLeft 1 reg_replacement) ]

                                else
                                    [ renderMdDefault config.fileServerUrl config.style reg_replacement ]

                            Nothing ->
                                []
                       )
            )
        |> List.concat
        |> span []



--
-- Replacer routine
--


{-| Preprocess raw markdown before passing it to the elm-markdown parser.

escapeAmpersandsInHtmlBlocks runs first on the FULL content (it has its own
code-fence tracking) because elm-markdown's HTML block parser scans content
inside <details>/<div> — including code fences — before identifying markdown
structures. A bare '&' anywhere inside an HTML block will crash the parser.

The remaining transformations (escapeLinks, forced line breaks) are wrapped
in processOutsideCodeBlocks so they don't modify fenced code block content.

-}
frac6Parser : String -> String
frac6Parser content =
    content
        -- Escape bare '&' inside HTML block tags (<details>, <div>)
        -- so the elm-markdown HTML parser doesn't choke on them
        -- (e.g. URLs with &param=value would produce "No entity named …" errors).
        -- Must run on the full content including code fences (see docstring).
        |> escapeAmpersandsInHtmlBlocks
        -- Apply the remaining transformations only outside code fences
        |> processOutsideCodeBlocks
            (\segment ->
                segment
                    -- Escape "_" in link to give the priority to autolink
                    |> escapeLinks
                    -- Force line break (except for Table rows starting with |)
                    |> Regex.replace (regexFromString "\n[^\n|]") (\m -> "  " ++ m.match)
            )


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


{-| Apply a transformation only to content outside fenced code blocks (\`\`\` or ~~~).
Lines inside code fences are passed through unchanged. This prevents the
preprocessor from mangling code examples that contain HTML tags, URLs, etc.
-}
processOutsideCodeBlocks : (String -> String) -> String -> String
processOutsideCodeBlocks transform content =
    let
        fencePattern =
            "^\\s*(```|~~~)"

        -- Walk lines, toggling inFence on each fence delimiter.
        -- Accumulate (isCode, lines) segments that are later joined back.
        folder line ( inFence, currentLines, acc ) =
            if regexContains fencePattern line then
                if inFence then
                    -- Closing fence: finish the code segment (include this fence line)
                    ( False, [], acc ++ [ ( True, List.reverse (line :: currentLines) ) ] )

                else
                    -- Opening fence: flush the preceding non-code segment, start code
                    ( True, [ line ], acc ++ [ ( False, List.reverse currentLines ) ] )

            else
                ( inFence, line :: currentLines, acc )

        ( finalInFence, finalLines, segments ) =
            List.foldl folder ( False, [], [] ) (String.lines content)

        allSegments =
            segments ++ [ ( finalInFence, List.reverse finalLines ) ]
    in
    allSegments
        |> List.filterMap
            (\( isCode, segLines ) ->
                if List.isEmpty segLines then
                    -- Drop empty segments to avoid spurious newlines at boundaries
                    Nothing

                else if isCode then
                    Just (String.join "\n" segLines)

                else
                    Just (transform (String.join "\n" segLines))
            )
        |> String.join "\n"


{-| Escape bare '&' to '&amp;' but only on lines that sit inside an HTML
block element (<details> or <div>). This is needed because elm-markdown's
HTML block parser requires proper entity encoding — a bare '&' followed by
letters (like in ?a=1&bar=2) is rejected as an invalid entity reference.

Lines outside HTML blocks are left untouched since the regular markdown
parser accepts bare '&' without issue.

This function tracks fenced code blocks (\`\`\` / ~~~) so that:

  - <details> tags inside a code fence do NOT change the HTML depth
  - '&' inside a code fence that is itself inside an HTML block IS escaped,
    because elm-markdown's HTML scanner sees it before identifying the fence

-}
escapeAmpersandsInHtmlBlocks : String -> String
escapeAmpersandsInHtmlBlocks content =
    let
        -- Only target block-level elements that elm-markdown parses as HTML blocks
        openPattern =
            "^\\s*<(details|div)\\b"

        closePattern =
            "</(details|div)>"

        fencePattern =
            "^\\s*(```|~~~)"

        folder line ( depth, inFence, result ) =
            let
                -- Toggle fence state on fence delimiters
                isFence =
                    regexContains fencePattern line

                newInFence =
                    if isFence then
                        not inFence

                    else
                        inFence

                -- Only count HTML open/close tags when outside code fences,
                -- so that <details> appearing in code examples doesn't alter depth
                opens =
                    if not inFence && not newInFence && regexContains openPattern line then
                        1

                    else
                        0

                closes =
                    if not inFence && not newInFence && regexContains closePattern line then
                        1

                    else
                        0

                -- Escape '&' when we are currently inside (or entering) an HTML block,
                -- regardless of whether we are also inside a code fence.
                -- Additionally, when inside a code fence within an HTML block,
                -- escape block-level opening/closing tags (<details>, <div>) to &lt;
                -- because elm-markdown's HTML scanner doesn't respect code fences —
                -- it would see <details> in the code and try to parse a nested HTML block.
                -- The visual trade-off: <details> in code renders as &lt;details>
                -- but that's better than a parse crash for this rare edge case.
                processedLine =
                    if depth + opens > 0 then
                        let
                            ampEscaped =
                                Regex.replace ampersandRegex (\_ -> "&amp;") line
                        in
                        if inFence || newInFence then
                            Regex.replace htmlBlockTagRegex (\m -> "&lt;" ++ String.dropLeft 1 m.match) ampEscaped

                        else
                            ampEscaped

                    else
                        line

                newDepth =
                    max 0 (depth + opens - closes)
            in
            ( newDepth, newInFence, result ++ [ processedLine ] )

        ( _, _, processedLines ) =
            List.foldl folder ( 0, False, [] ) (String.lines content)
    in
    String.join "\n" processedLines


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
