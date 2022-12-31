{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2022 Fractale Co

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


module ModelCommon.Error exposing (..)

import Assets as A
import Extra exposing (ternary, textH, upH)
import Extra.Events exposing (onClickPD)
import Generated.Route as Route exposing (Route, toHref)
import Html exposing (Html, a, button, div, img, p, span, text)
import Html.Attributes exposing (alt, attribute, class, height, href, src, target, width)
import Json.Decode as JD
import Loading
    exposing
        ( ErrorData
        , GqlData
        , HttpError
        , ModalData
        , RequestResult(..)
        , WebData
        , errorDecoder
        , errorsDecoder
        , toErrorData
        )
import Markdown exposing (renderMarkdown)
import Maybe exposing (withDefault)
import ModelCommon.Codecs exposing (NodeFocus)
import RemoteData exposing (RemoteData)
import Text as T


viewHttpErrors : HttpError String -> Html msg
viewHttpErrors httpErr =
    httpErr
        |> toErrorData
        |> viewGqlErrors


viewMaybeErrors : GqlData a -> Html msg
viewMaybeErrors data =
    case data of
        Failure err ->
            viewGqlErrors err

        _ ->
            text ""


viewMaybeWebErrors : WebData a -> Html msg
viewMaybeWebErrors data =
    case data of
        RemoteData.Failure err ->
            viewHttpErrors err

        _ ->
            text ""


viewGqlErrors : ErrorData -> Html msg
viewGqlErrors errMsg =
    errMsg
        |> List.map
            (\e ->
                let
                    err =
                        case JD.decodeString errorsDecoder e of
                            Ok err_ ->
                                err_.errors
                                    |> List.head
                                    |> Maybe.map (\x -> upH x.message)
                                    |> withDefault e

                            Err err_ ->
                                case JD.decodeString (JD.list errorDecoder) e of
                                    Ok err2_ ->
                                        err2_
                                            |> List.head
                                            |> Maybe.map (\x -> upH x.message)
                                            |> withDefault e

                                    Err t ->
                                        e
                in
                p [ class "message-body" ] [ renderMarkdown "is-light" err ]
            )
        |> div [ class "f6-error message is-danger is-light mt-2" ]


viewAuthNeeded : (ModalData -> msg) -> Html msg
viewAuthNeeded onClose =
    div [ class "modal-card" ]
        [ div [ class "modal-card-head" ]
            [ div [ class "modal-card-title is-size-6 has-text-weight-semibold" ]
                [ text T.authenticationNeeded
                , button [ class "delete is-pulled-right", onClickPD (onClose { reset = True, link = "" }) ] []
                ]
            ]
        , div [ class "modal-card-body" ]
            [ p []
                [ text T.please
                , button
                    [ class "button is-small is-primary is-text-aligned mx-2", onClickPD (onClose { reset = True, link = toHref Route.Login }) ]
                    [ text T.signinNow2 ]
                , text T.or_
                , button
                    [ class "button is-small is-success is-text-aligned mx-2", onClickPD (onClose { reset = True, link = toHref Route.Signup }) ]
                    [ text T.signupNow2 ]
                , text T.toPerformThis
                ]
            ]
        ]


viewJoinForTensionNeeded : Bool -> (ModalData -> msg) -> Html msg
viewJoinForTensionNeeded userCanJoin onClose =
    div [ class "modal-card" ]
        [ div [ class "modal-card-head has-background-warning" ]
            [ div [ class "modal-card-title is-size-6 has-text-grey-dark has-text-weight-semibold" ]
                [ text T.authorizationNeeded
                , button [ class "delete is-pulled-right", onClickPD (onClose { reset = True, link = "" }) ] []
                ]
            ]
        , div [ class "modal-card-body" ]
            [ if userCanJoin then
                p []
                    [ button [ class "button is-small mx-2 joinTrigger" ] [ text T.join2 ]
                    , text T.joinForTension
                    , text " "
                    , a [ href "https://doc.fractale.co/tension", target "_blank" ] [ text T.tensions ]
                    , text "."
                    ]

              else
                p []
                    [ text T.onlyMemberCanCreate
                    , text " "
                    , a [ href "https://doc.fractale.co/tension", target "_blank" ] [ text T.tensions ]
                    , text "."
                    ]
            ]
        ]


viewJoinForCommentNeeded : Bool -> Html msg
viewJoinForCommentNeeded userCanJoin =
    div [ class "box has-background-primary has-text-light" ]
        [ if userCanJoin then
            p []
                [ button [ class "button is-small mx-2 joinTrigger" ] [ text T.join2 ]
                , text T.thisOrgaToParticipate
                ]

          else
            p []
                [ text T.onlyMemberCanParticipate
                ]
        ]
