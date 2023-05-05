div [ class "dropdown is-right" ]
    [ div [ class "dropdown-trigger is-w is-h" ]
        [ div
            [ class "ellipsis"
            , attribute "aria-controls" ("edit-ellipsis-" ++ project.id)
            , attribute "aria-haspopup" "true"
            ]
            [ A.icon "icon-more-horizontal icon-lg" ]
        ]
    , div [ id ("edit-ellipsis-" ++ project.id), class "dropdown-menu", attribute "role" "menu" ]
        [ div [ class "dropdown-content p-0" ] <|
            [ div [ class "dropdown-item button-light", onClick (EditProject project) ] [ text T.edit ]
            , hr [ class "dropdown-divider" ] []
            , div [ class "dropdown-item button-light", onClick (ChangeStatus status_new project) ] [ text status_txt ]
            ]
        ]
    ]
