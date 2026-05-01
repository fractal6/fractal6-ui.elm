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


module Utils.Html exposing
    ( showIf
    , showMaybe
    , showMsg
    , textA
    , textD
    , textH
    , textT
    , toText
    )

import Assets as A
import Html exposing (Html, div, input, label, section, span, text)
import Html.Attributes exposing (attribute, class, for, id, name, style, type_)
import Markdown exposing (renderMarkdown)
import Text as T
import Utils.String exposing (decap, upA, upH, upT)


showIf : Bool -> Html msg -> Html msg
showIf a v =
    if a then
        v

    else
        text ""


showMaybe : Maybe a -> (a -> Html msg) -> Html msg
showMaybe a f =
    case a of
        Just x ->
            f x

        Nothing ->
            text ""


toText : List String -> Html msg
toText l =
    l
        |> List.intersperse " "
        |> List.map (\x -> text x)
        |> span []


textH : String -> Html msg
textH s =
    s |> upH |> text


textT : String -> Html msg
textT s =
    s |> upT |> text


textA : String -> Html msg
textA s =
    s |> upA |> text


textD : String -> Html msg
textD s =
    s |> decap |> text


showMsg : String -> String -> String -> String -> String -> Html msg
showMsg id_ cls icon header message =
    if message == "" then
        div [ class ("f6-help-message notification p-4 m-0 mb-4 is-flex is-align-items-start " ++ cls) ]
            [ span [ class "mr-3 has-text-strong", style "margin-top" "0.15em" ] [ A.icon icon ]
            , renderMarkdown "" header
            ]

    else
        let
            did =
                "acc" ++ id_

            mkdCls =
                if String.contains "is-light" cls then
                    "is-light"

                else if String.contains "is-dark" cls then
                    "is-dark"

                else
                    ""
        in
        div [ class "f6-help-message accordion arrows-right" ]
            [ input [ id did, name "accordion", type_ "radio" ] []
            , section [ class ("acc message " ++ cls) ]
                [ label
                    [ class "acc-title message-header"
                    , attribute "title" T.clickMe
                    , for did
                    ]
                    [ span [ class "is-flex is-align-items-flex-start" ]
                        [ span [ class "mr-3 has-text-strong" ] [ A.icon icon ]
                        , span [] [ text (upH header) ]
                        ]
                    ]
                , label [ class "acc-close", for "acc-close" ] []
                , div [ class "acc-content " ]
                    [ renderMarkdown ("message-body " ++ mkdCls) message ]
                ]
            , input [ id "acc-close", name "accordion", type_ "radio" ] []
            ]
