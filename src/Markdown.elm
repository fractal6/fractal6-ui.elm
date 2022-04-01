module Markdown exposing (renderMarkdown)

import Assets as A
import Extra exposing (regexFromString)
import Generated.Route as Route exposing (Route, toHref)
import Html exposing (Html, a, br, div, i, span, text)
import Html.Attributes exposing (class, href, style, target, title)
import Html.Lazy as Lazy
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
                    Markdown.Renderer.render frac6Renderer ast
                )
    of
        Ok rendered ->
            div [ class ("content markdown-body " ++ style) ] rendered

        Err errors ->
            text errors


deadEndsToString deadEnds =
    deadEnds
        |> List.map Markdown.deadEndToString
        |> String.join "\n"


frac6Renderer : Markdown.Renderer.Renderer (Html msg)
frac6Renderer =
    { defaultHtmlRenderer
        | link =
            \link content ->
                let
                    lk =
                        toLower link.destination

                    attrs =
                        if
                            not (startsWith "https://fractale.co" lk || startsWith "http://fractale.co" lk)
                                && (startsWith "https://" lk || startsWith "http://" lk)
                        then
                            [ href link.destination, target "_blank" ]

                        else
                            [ href link.destination, class "is-link" ]
                in
                case link.title of
                    Just t ->
                        a (title t :: attrs) content

                    Nothing ->
                        a attrs content
        , html =
            Markdown.Html.oneOf
                [ Markdown.Html.tag "i"
                    (\cls content ->
                        span [] [ i [ class cls ] [], span [] content ]
                    )
                    |> Markdown.Html.withAttribute "class"
                , Markdown.Html.tag "div"
                    (\cls content ->
                        div [ class (withDefault "" cls) ] content
                    )
                    |> Markdown.Html.withOptionalAttribute "class"
                ]
    }


frac6Parser : String -> String
frac6Parser content =
    content
        -- Username format
        |> Regex.replace (regexFromString "(^|\\s)@[\\w\\-\\.]+") userLink
        -- Tension format
        |> Regex.replace (regexFromString "(^|\\s)\\b0x[0-9a-f]+") tensionLink
        -- Autolink
        |> Regex.replace (regexFromString "(^|\\s)https?://[\\w\\-\\.\\?\\#/,]+") autoLink
        -- JumpLine
        |> Regex.replace (regexFromString "\n[^\n]") (\m -> "  " ++ m.match)


userLink : Regex.Match -> String
userLink match =
    let
        m =
            String.trimLeft match.match

        ( username, right_fragment ) =
            if List.member (String.right 1 m) [ ".", "-" ] then
                ( String.dropRight 1 m, String.right 1 m )

            else
                ( String.dropLeft 1 m, "" )
    in
    " ["
        ++ "@"
        ++ username
        ++ "]"
        ++ "("
        ++ uriFromUsername UsersBaseUri username
        ++ ")"
        ++ right_fragment


autoLink : Regex.Match -> String
autoLink match =
    let
        m =
            String.trimLeft match.match

        ( link, right_fragment ) =
            if List.member (String.right 1 m) [ ".", "?", "," ] then
                ( String.dropRight 1 m, String.right 1 m )

            else
                ( m, "" )
    in
    " ["
        ++ link
        ++ "]"
        ++ "("
        ++ link
        ++ ")"
        ++ right_fragment


tensionLink : Regex.Match -> String
tensionLink match =
    let
        m =
            String.trimLeft match.match

        tid =
            m
    in
    " ["
        ++ tid
        ++ "]"
        ++ "("
        ++ (Route.Tension_Dynamic_Dynamic { param1 = "", param2 = tid } |> toHref)
        ++ ")"
