module Generated.Pages exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Generated.Route as Route exposing (Route)
import Global
import Page exposing (Bundle, Document)
import Pages.Top
import Pages.NotFound
import Pages.Test.Testa
import Pages.Org.Dynamic
import Pages.Org.Dynamic.Dynamic



-- TYPES


type Model
    = Top_Model Pages.Top.Model
    | NotFound_Model Pages.NotFound.Model
    | Test_Testa_Model Pages.Test.Testa.Model
    | Org_Dynamic_Model Pages.Org.Dynamic.Model
    | Org_Dynamic_Dynamic_Model Pages.Org.Dynamic.Dynamic.Model


type Msg
    = Top_Msg Pages.Top.Msg
    | NotFound_Msg Pages.NotFound.Msg
    | Test_Testa_Msg Pages.Test.Testa.Msg
    | Org_Dynamic_Msg Pages.Org.Dynamic.Msg
    | Org_Dynamic_Dynamic_Msg Pages.Org.Dynamic.Dynamic.Msg



-- PAGES


type alias UpgradedPage flags model msg =
    { init : flags -> Global.Model -> ( Model, Cmd Msg, Cmd Global.Msg )
    , update : msg -> model -> Global.Model -> ( Model, Cmd Msg, Cmd Global.Msg )
    , bundle : model -> Global.Model -> Bundle Msg
    }


type alias UpgradedPages =
    { top : UpgradedPage Pages.Top.Flags Pages.Top.Model Pages.Top.Msg
    , notFound : UpgradedPage Pages.NotFound.Flags Pages.NotFound.Model Pages.NotFound.Msg
    , test_testa : UpgradedPage Pages.Test.Testa.Flags Pages.Test.Testa.Model Pages.Test.Testa.Msg
    , org_dynamic : UpgradedPage Pages.Org.Dynamic.Flags Pages.Org.Dynamic.Model Pages.Org.Dynamic.Msg
    , org_dynamic_dynamic : UpgradedPage Pages.Org.Dynamic.Dynamic.Flags Pages.Org.Dynamic.Dynamic.Model Pages.Org.Dynamic.Dynamic.Msg
    }


pages : UpgradedPages
pages =
    { top = Pages.Top.page |> Page.upgrade Top_Model Top_Msg
    , notFound = Pages.NotFound.page |> Page.upgrade NotFound_Model NotFound_Msg
    , test_testa = Pages.Test.Testa.page |> Page.upgrade Test_Testa_Model Test_Testa_Msg
    , org_dynamic = Pages.Org.Dynamic.page |> Page.upgrade Org_Dynamic_Model Org_Dynamic_Msg
    , org_dynamic_dynamic = Pages.Org.Dynamic.Dynamic.page |> Page.upgrade Org_Dynamic_Dynamic_Model Org_Dynamic_Dynamic_Msg
    }



-- INIT


init : Route -> Global.Model -> ( Model, Cmd Msg, Cmd Global.Msg )
init route =
    case route of
        Route.Top ->
            pages.top.init ()
        
        Route.NotFound ->
            pages.notFound.init ()
        
        Route.Test_Testa ->
            pages.test_testa.init ()
        
        Route.Org_Dynamic params ->
            pages.org_dynamic.init params
        
        Route.Org_Dynamic_Dynamic params ->
            pages.org_dynamic_dynamic.init params



-- UPDATE


update : Msg -> Model -> Global.Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update bigMsg bigModel =
    case ( bigMsg, bigModel ) of
        ( Top_Msg msg, Top_Model model ) ->
            pages.top.update msg model
        
        ( NotFound_Msg msg, NotFound_Model model ) ->
            pages.notFound.update msg model
        
        ( Test_Testa_Msg msg, Test_Testa_Model model ) ->
            pages.test_testa.update msg model
        
        ( Org_Dynamic_Msg msg, Org_Dynamic_Model model ) ->
            pages.org_dynamic.update msg model
        
        ( Org_Dynamic_Dynamic_Msg msg, Org_Dynamic_Dynamic_Model model ) ->
            pages.org_dynamic_dynamic.update msg model
        
        _ ->
            always ( bigModel, Cmd.none, Cmd.none )



-- BUNDLE - (view + subscriptions)


bundle : Model -> Global.Model -> Bundle Msg
bundle bigModel =
    case bigModel of
        Top_Model model ->
            pages.top.bundle model
        
        NotFound_Model model ->
            pages.notFound.bundle model
        
        Test_Testa_Model model ->
            pages.test_testa.bundle model
        
        Org_Dynamic_Model model ->
            pages.org_dynamic.bundle model
        
        Org_Dynamic_Dynamic_Model model ->
            pages.org_dynamic_dynamic.bundle model


view : Model -> Global.Model -> Document Msg
view model =
    bundle model >> .view


subscriptions : Model -> Global.Model -> Sub Msg
subscriptions model =
    bundle model >> .subscriptions