{-
   Fractale - Self-organisation for humans.
   Copyright (C) 2023 Fractale Co

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


module Scroll exposing
    ( Config(..), createConfig, scrollTo
    , scrollToBottom, scrollToElement, scrollToSubBottom, scrollToSubElement, scrollToTop
    )

{-| Scrolling to position that always takes the same amount of time.

@docs Config, createConfig, scrollTo
@docs fork : https://github.com/WhileTruu/elm-smooth-scroll

-}

import Browser.Dom exposing (Error(..))
import Ease exposing (Easing)
import Task exposing (Task)
import Time exposing (Posix)



{-
   Public functions
-}


{-| Scroll to the given position in the body viewport
-}
scrollTo : Float -> Task Browser.Dom.Error ()
scrollTo =
    scrollTo_ Nothing (createConfig Ease.inOutCubic 500)


scrollToTop : msg -> Cmd msg
scrollToTop noop =
    Task.attempt (always noop) (scrollTo 0)


scrollToBottom : msg -> Cmd msg
scrollToBottom noop =
    Task.attempt (always noop)
        (Browser.Dom.getViewport
            |> Task.andThen
                (\{ scene, viewport } -> scrollTo (scene.height - viewport.height))
        )


scrollToElement : String -> msg -> Cmd msg
scrollToElement id noop =
    Task.attempt (always noop)
        (Browser.Dom.getElement id |> Task.andThen (scrollTo << (\x -> x.y - 50) << .element))


scrollToSubElement : String -> String -> msg -> Cmd msg
scrollToSubElement viewport_id id noop =
    Task.attempt (always noop)
        (getElementOf { viewportId = viewport_id, elementId = id }
            |> Task.andThen
                (\e ->
                    if e.element.y < e.viewport.y || e.element.y > e.viewport.y + e.viewport.height then
                        scrollToOf viewport_id e.element.y

                    else
                        Task.fail (NotFound "element visible")
                )
        )


scrollToSubBottom : String -> msg -> Cmd msg
scrollToSubBottom viewport_id noop =
    Task.attempt (always noop)
        (getElementOf { viewportId = viewport_id, elementId = viewport_id }
            |> Task.andThen
                (\e ->
                    scrollToOf viewport_id e.element.height
                )
        )



{-
   Private functions
-}


{-| Scroll to an element to the given viewport
-}
scrollToOf : String -> Float -> Task Browser.Dom.Error ()
scrollToOf vpid =
    scrollTo_ (Just vpid) (createConfig Ease.inOutCubic 500)


getElementOf :
    { viewportId : String, elementId : String }
    -> Task Browser.Dom.Error Browser.Dom.Element
getElementOf { viewportId, elementId } =
    Task.map3
        (\e v ev ->
            { scene = v.scene
            , viewport = v.viewport
            , element =
                e.element
                    |> (\a ->
                            { a
                                | x = a.x - ev.element.x
                                , y = a.y - ev.element.y + v.viewport.y
                            }
                       )
            }
        )
        (Browser.Dom.getElement elementId)
        (Browser.Dom.getViewportOf viewportId)
        (Browser.Dom.getElement viewportId)


{-| Configuration for smooth scrolling.
-}
type Config
    = Config { duration : Int, easing : Easing }


{-| Create a smooth scroll configuration from an easing function and a duration
in milliseconds.

  - easing: [Easing functions](https://package.elm-lang.org/packages/elm-community/easing-functions/latest)
    specify the rate of change of a parameter over time.
  - duration: The total duration of the scroll in milliseconds.

```
createConfig Ease.outCubic 100
```

-}
createConfig : Easing -> Int -> Config
createConfig easing duration =
    Config { duration = duration, easing = easing }


{-| Scroll to the `y` offset of the browser viewport using the easing function
and duration specified in the config.
scrollTo (createConfig Ease.outCubic 100) 500

if nodeId is a Just String, it scrool to to the given viewPort
otherwise it use the body viewport

-}
scrollTo_ : Maybe String -> Config -> Float -> Task Browser.Dom.Error ()
scrollTo_ nodeId (Config config) y =
    let
        viewport_ =
            case nodeId of
                Just nid ->
                    Browser.Dom.getViewportOf nid

                Nothing ->
                    Browser.Dom.getViewport
    in
    Task.map2
        (\{ viewport } startTime ->
            let
                f =
                    case nodeId of
                        Just nid ->
                            Browser.Dom.setViewportOf nid

                        Nothing ->
                            --Browser.Dom.setViewport viewport.x
                            Browser.Dom.setViewport
            in
            Task.andThen
                (step f config viewport.y y startTime)
                Time.now
        )
        viewport_
        Time.now
        |> Task.andThen identity


{-| Change the `y` offset of the browser viewport to the calculated position and
then do that again and again until the duration is larger than the time elapsed.
-}
step :
    (Float -> Float -> Task Browser.Dom.Error ())
    -> { duration : Int, easing : Easing }
    -> Float
    -> Float
    -> Posix
    -> Posix
    -> Task Browser.Dom.Error ()
step f config start end startTime now =
    let
        elapsed : Int
        elapsed =
            Time.posixToMillis now - Time.posixToMillis startTime
    in
    f 0 (position config start end elapsed)
        |> Task.andThen
            (if elapsed < config.duration then
                \_ -> Time.now |> Task.andThen (step f config start end startTime)

             else
                Task.succeed
            )


{-| Calculate the desired scroll position.
position defaultConfig 450 0 225 == 225
-}
position : { duration : Int, easing : Easing } -> Float -> Float -> Int -> Float
position { easing, duration } start end elapsed =
    if elapsed < duration then
        start + (end - start) * easing (toFloat elapsed / toFloat duration)

    else
        end
