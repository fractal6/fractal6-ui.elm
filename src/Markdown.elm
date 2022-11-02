module Markdown exposing (renderMarkdown)

import Assets as A
import Extra exposing (regexFromString, space_, ternary)
import Generated.Route as Route exposing (Route, toHref)
import Html exposing (Html, a, br, div, i, span, text)
import Html.Attributes exposing (class, href, rel, style, target, title)
import Html.Lazy as Lazy
import List.Extra as LE
import Markdown.Html
import Markdown.Parser as Markdown
import Markdown.Renderer exposing (defaultHtmlRenderer)
import Maybe exposing (withDefault)
import ModelCommon.Codecs exposing (FractalBaseRoute(..), uriFromUsername)
import Regex
import String exposing (startsWith, toLower)


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
        , text =
            \t ->
                if recursive then
                    mardownRoutine
                        style
                        ( "(^|[^\\w\\[\\`])https?://[\\w\\-\\+\\.\\?\\#/@~&=]+", autoLink )
                        [ ( "(^|[^\\w\\[\\`])@[\\w\\-\\.]+\\b", userLink )
                        , ( "(^|[^\\w\\[\\`])0x[0-9a-f]+", tensionLink )

                        --, ( "\\bo/[0-9a-zA-Z\\-_\]+", circleLink )
                        ]
                        t

                else
                    text t
        , table = \x -> div [ class "table-container" ] [ Html.table [] x ]
        , html =
            -- Html tag supported in the text
            Markdown.Html.oneOf
                [ Markdown.Html.tag "i"
                    (\cls content ->
                        span [] ([ i [ class cls ] [] ] ++ content)
                    )
                    |> Markdown.Html.withAttribute "class"
                , Markdown.Html.tag "span"
                    -- @DEBUG: span does not work because p tag are inserted and break lines
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


mardownRoutine : String -> ( String, Regex.Match -> String -> String ) -> List ( String, Regex.Match -> String -> String ) -> String -> Html msg
mardownRoutine style rep next_replacers content =
    let
        reg =
            Tuple.first rep

        replacer =
            Tuple.second rep

        matches =
            Regex.find (regexFromString reg) content
    in
    -- Split on the regex (and append the regex replacer)
    Regex.split (regexFromString reg) content
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
        --|> Regex.replace (regexFromString "\\bhttps?://[\\w\\-\\+\\.\\?\\#/@~:=]+") autoLink
        --
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
    in
    if String.slice (m.index - 2) m.index full == "](" then
        match

    else
        left
            ++ "["
            ++ link
            ++ "]"
            ++ "("
            ++ link
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
        ++ uriFromUsername UsersBaseUri (String.dropLeft 1 username)
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
