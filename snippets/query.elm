--
-- ADD
--
addReactionInputEncoder : String -> String -> Int -> Mutation.AddReactionRequiredArguments
addReactionInputEncoder username cid type_ =
    let
        inputReq =
            { user =
                Input.buildUserRef (\u -> { u | username = Present username })
            , comment =
                Input.buildCommentRef (\u -> { u | id = cid |> encodeId |> Present })
            , type_ = type_
            , reactionid = "" -- set in the backend
            }

        --inputOpt =
        --    \x -> x
    in
    { input = [ Input.buildAddReactionInput inputReq ] }

--
-- Update
--
addProjectColumn url form msg =
    let
        name_m =
            Dict.get "name" form.post

        column =
            Input.buildProjectColumnRef
                (\i ->
                    { i
                        | name = fromMaybe name_m
                        , description = fromMaybe (Dict.get "description" form.post)
                        , color = fromMaybe (Dict.get "color" form.post)
                        , pos = fromMaybe form.pos
                    }
                )
    in
    makeGQLMutation url
        (Mutation.updateProject
            { input =
                Input.buildUpdateProjectInput { filter = Input.buildProjectFilter (oneId form.projectid) }
                    (\_ ->
                        { set = Input.buildProjectPatch (\i -> { i | columns = Present [ column ] }) |> Present
                        , remove = Absent
                        }
                    )
            }
            (SelectionSet.map (\a -> withDefault [] a |> List.head |> withDefault Nothing |> withDefault Nothing)
                (Fractal.Object.UpdateProjectPayload.project identity
                    (SelectionSet.map (withDefault [] >> List.head)
                        (Fractal.Object.Project.columns
                            (\b ->
                                { b
                                    | filter =
                                        Input.buildProjectColumnFilter (\c -> { c | name = fromMaybe <| Maybe.map (\name -> { eq = Present name, in_ = Absent }) name_m })
                                            |> Present
                                }
                            )
                            columnPayload
                        )
                    )
                )
            )
        )
        (RemoteData.fromResult >> decodeResponse (withDefault Nothing) >> msg)



