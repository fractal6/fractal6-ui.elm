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


andThen : (msg -> model -> ( model, Cmd a )) -> msg -> ( model, Cmd a ) -> ( model, Cmd a )
andThen update msg ( model, cmd ) =
    let
        ( model_, cmd_ ) =
            update msg model
    in
    ( model_, Cmd.batch [ cmd, cmd_ ] )


{-| Allows you to update the model in an update pipeline.
For example
update msg model = model ! []
|> updateModel \\model -> { model | a = 1 }
|> updateModel \\model -> { model | b = 2 }
-}
updateModel : (model -> model) -> ( model, Cmd msg ) -> ( model, Cmd msg )
updateModel f ( model, cmd ) =
    ( f model, cmd )
