module Markdown exposing (renderMarkdown)

import Assets as A
import Extra exposing (regexFromString)
import Generated.Route as Route exposing (Route, toHref)
import Html exposing (Html, a, br, div, i, span, text)
import Html.Attributes exposing (class, href, style, target, title)
import Markdown.Html
import Markdown.Parser as Markdown
import Markdown.Renderer exposing (defaultHtmlRenderer)
import Maybe exposing (withDefault)
import ModelCommon.Codecs exposing (FractalBaseRoute(..), uriFromUsername)
import Regex
import String exposing (startsWith, toLower)


renderMarkdown : String -> String -> Html msg
renderMarkdown style content =
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
        |> Regex.replace (regexFromString "(^|\\s)@[\\w\\-\\.]+") userLink
        |> Regex.replace (regexFromString "(^|\\s)\\b0x[0-9a-f]+") tensionLink


userLink : Regex.Match -> String
userLink match =
    let
        m =
            String.trimLeft match.match

        ( username, right_fragment ) =
            if String.right 1 m == "." then
                ( String.dropLeft 1 m |> String.dropRight 1, "." )

            else if String.right 1 m == "-" then
                ( String.dropLeft 1 m |> String.dropRight 1, "-" )

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
