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


port module Components.TensionPicker exposing (Msg(..), State, completeWords, filterTensions, init, isOpen_, subscriptions, update, viewTensionSeeker)

import Assets as A
import Auth exposing (ErrState(..), parseErr)
import Components.Out as O exposing (noOut, out0, out2)
import Fractale.Error exposing (viewGqlErrors)
import Fractale.View exposing (statusColor, tensionIcon)
import Html exposing (Html, div, p, span, text)
import Html.Attributes exposing (attribute, class, classList)
import List.Extra as LE
import Loading exposing (GqlData, RequestResult(..), isLoading, isSuccess, loadingSpinRight, withDefaultData)
import ModelSchema exposing (PinTension)
import Ports
import Query.QueryTension exposing (queryTensionRefs)
import Session exposing (Apis, GlobalCmd(..))
import Text as T
import Utils.Bool exposing (ternary)
import Utils.Cmd exposing (sendSleep)
import Utils.DomEvents exposing (onMousedownPD)


type State
    = State Model


type alias Model =
    { isOpen : Bool
    , pattern : String
    , tensions_result : GqlData (List PinTension)
    , rootnameid : String -- organisation tensions_result was fetched for
    , words : List String -- complete words tensions_result was fetched for
    , activePos : Int
    , isArrowMode : Bool
    , refresh_trial : Int -- use to refresh user token
    }


{-| Size of the pool the client filter runs on: the latest open tensions, or the server matches of the complete words.
ponytail: a search with more than 100 matches is truncated to the newest; page it if that bites.
-}
nTensions : Int
nTensions =
    100


init : State
init =
    State { isOpen = False, pattern = "", tensions_result = NotAsked, rootnameid = "", words = [], activePos = 0, isArrowMode = False, refresh_trial = 0 }


isOpen_ : State -> Bool
isOpen_ (State model) =
    model.isOpen


resetNav : Model -> Model
resetNav m =
    { m | pattern = "", activePos = 0, isArrowMode = False }


{-| Lowercased words of a pattern (the pattern is what follows `0x`).
-}
toWords : String -> List String
toWords =
    String.toLower >> String.words >> List.filter ((/=) "")


{-| Words followed by a space: the ones the server search runs on.
-}
completeWords : String -> List String
completeWords pattern =
    let
        ws =
            toWords pattern
    in
    ternary (String.endsWith " " pattern) ws (List.take (List.length ws - 1) ws)


{-| Tensions matching ANY word of the pattern (title contains it, or id starts with `0x` + word), best matches first.
-}
filterTensions : String -> List PinTension -> List PinTension
filterTensions pattern tensions =
    let
        words =
            toWords pattern

        score t =
            words
                |> List.filter (\w -> String.contains w (String.toLower t.title) || String.startsWith ("0x" ++ w) t.id)
                |> List.length
    in
    ternary (words == []) tensions (tensions |> List.filter (\t -> score t > 0) |> List.sortBy (\t -> negate (score t)))
        |> List.take 15


lookup : Model -> List PinTension
lookup model =
    filterTensions model.pattern (withDefaultData [] model.tensions_result)


type Msg
    = OnOpen
    | OnClose
    | OnChangePattern String
    | DoQuery
    | OnTensionsAck (List String) (GqlData (List PinTension))
    | OnClickTension String
    | OnArrowMove String
    | OnSelectActive


type alias Out =
    O.Out Msg String


{-| `rootnameid` is the organisation whose tensions are listed.
-}
update : Apis -> String -> Msg -> State -> ( State, Out )
update apis rootnameid msg (State model) =
    update_ apis rootnameid msg model |> Tuple.mapFirst State


update_ : Apis -> String -> Msg -> Model -> ( Model, Out )
update_ apis rootnameid msg model =
    case msg of
        OnOpen ->
            let
                m =
                    resetNav { model | isOpen = True }
            in
            if rootnameid /= "" && (rootnameid /= m.rootnameid || m.words /= [] || not (isSuccess m.tensions_result || isLoading m.tensions_result)) then
                update_ apis rootnameid DoQuery m

            else
                ( m, noOut )

        OnClose ->
            ( resetNav { model | isOpen = False }, noOut )

        OnChangePattern pattern ->
            let
                m =
                    { model | pattern = pattern, activePos = 0, isArrowMode = False }
            in
            -- Refresh the pool each time a complete word is added or removed.
            if rootnameid /= "" && completeWords pattern /= m.words then
                update_ apis rootnameid DoQuery m

            else
                ( m, noOut )

        DoQuery ->
            let
                words =
                    completeWords model.pattern
            in
            -- Same organisation: keep the current list on screen while refetching.
            ( { model | rootnameid = rootnameid, words = words, tensions_result = ternary (rootnameid == model.rootnameid && isSuccess model.tensions_result) model.tensions_result LoadingSlowly }
            , out0 [ queryTensionRefs apis rootnameid nTensions words (OnTensionsAck words) ]
            )

        OnTensionsAck words result ->
            case parseErr result model.refresh_trial of
                RefreshToken i ->
                    ( { model | refresh_trial = i }, out2 [ sendSleep DoQuery 500 ] [ DoUpdateToken ] )

                _ ->
                    -- Stale answer, or no title has these words (stop word, unfinished word): keep the current list.
                    if words /= model.words || (result == Success [] && words /= [] && isSuccess model.tensions_result) then
                        ( model, noOut )

                    else
                        ( { model | tensions_result = result }, noOut )

        OnClickTension tid ->
            ( resetNav { model | isOpen = False }, { noOut | result = Just tid } )

        OnArrowMove dir ->
            let
                n =
                    List.length (lookup model)
            in
            if n == 0 then
                ( model, noOut )

            else if not model.isArrowMode then
                ( { model | activePos = 0, isArrowMode = True }, noOut )

            else
                ( { model
                    | activePos =
                        case dir of
                            "up" ->
                                max 0 (model.activePos - 1)

                            "down" ->
                                min (n - 1) (model.activePos + 1)

                            "next" ->
                                modBy n (model.activePos + 1)

                            _ ->
                                model.activePos
                  }
                , noOut
                )

        OnSelectActive ->
            case LE.getAt model.activePos (lookup model) of
                Just t ->
                    update_ apis rootnameid (OnClickTension t.id) model

                Nothing ->
                    ( model, noOut )


subscriptions : State -> List (Sub Msg)
subscriptions (State model) =
    [ openTensionPickerFromJs (always OnOpen) ]
        ++ (if model.isOpen then
                [ closeTensionPickerFromJs (always OnClose)
                , changeTensionPatternFromJs OnChangePattern
                , Ports.arrowFromJs OnArrowMove
                , Ports.selectActiveItemFromJs (always OnSelectActive)
                ]

            else
                []
           )


port openTensionPickerFromJs : (() -> msg) -> Sub msg


port closeTensionPickerFromJs : (() -> msg) -> Sub msg


port changeTensionPatternFromJs : (String -> msg) -> Sub msg



-- ------------------------------
-- V I E W
-- ------------------------------


viewTensionSeeker : State -> Html Msg
viewTensionSeeker (State model) =
    div [ class "panel dropList tensionPicker" ]
        [ div [ class "selectors" ] <|
            case lookup model of
                [] ->
                    [ p [ class "panel-block help-label is-static", attribute "style" "cursor: default !important;" ] <|
                        if isLoading model.tensions_result then
                            [ text T.loading, loadingSpinRight True ]

                        else
                            [ text T.noResultsFound ]
                    ]

                tensions ->
                    List.indexedMap
                        (\j t ->
                            p
                                [ class "panel-block pt-1 pb-1"
                                , classList [ ( "is-active", model.isArrowMode && model.activePos == j ) ]
                                , onMousedownPD (OnClickTension t.id)
                                ]
                                [ span [ class "mr-2" ] [ tensionIcon t.type_ ]
                                , span [ class "is-flex-grow-1" ] [ text t.title ]
                                , A.icon ("icon-alert-circle icon-sm ml-2 has-text-" ++ statusColor t.status)
                                ]
                        )
                        tensions
        , case model.tensions_result of
            Failure err ->
                viewGqlErrors err

            _ ->
                text ""
        ]
