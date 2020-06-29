module Components.Markdown exposing (renderMarkdown)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Markdown.Parser as Markdown
import Markdown.Renderer


renderMarkdown : String -> Html msg
renderMarkdown message =
    case
        message
            |> Markdown.parse
            |> Result.mapError deadEndsToString
            |> Result.andThen (\ast -> Markdown.Renderer.render Markdown.Renderer.defaultHtmlRenderer ast)
    of
        Ok rendered ->
            div [ class "content" ] rendered

        Err errors ->
            text errors


deadEndsToString deadEnds =
    deadEnds
        |> List.map Markdown.deadEndToString
        |> String.join "\n"
