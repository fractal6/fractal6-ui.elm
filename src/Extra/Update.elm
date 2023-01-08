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


module Extra.Update exposing
    ( andThen
    , updateModel
    )

{-| Allows update call composition. Can be used with the pipeline operator (|>)
to chain updates.
For example:
update msg model =
( model, Cmd.none )
|> andThen update SomeMessage
|> andThen update SomeOtherMessage
|> andThen update (MessageWithArguments "Hello")
...
-}

-- Inspired from https://github.com/ccapndave/elm-update-extra
-- @future: move to elm-spa for simple message tuple update.
--
--
-- Do NOT WORK ! tried wih SaveData |> andThen (Navigate url)...
--


andThen : global -> (global -> msg -> model -> ( model, Cmd a, Cmd b )) -> msg -> ( model, Cmd a, Cmd b ) -> ( model, Cmd a, Cmd b )
andThen global update msg ( model, cmd, gcmd ) =
    let
        ( model_, cmd_, gcmd_ ) =
            update global msg model
    in
    ( model_, Cmd.batch [ cmd, cmd_ ], Cmd.batch [ gcmd, gcmd_ ] )


{-| Allows you to update the model in an update pipeline.
For example
update msg model = model ! []
|> updateModel \\model -> { model | a = 1 }
|> updateModel \\model -> { model | b = 2 }
-}
updateModel : (model -> model) -> ( model, Cmd msg ) -> ( model, Cmd msg )
updateModel f ( model, cmd ) =
    ( f model, cmd )
