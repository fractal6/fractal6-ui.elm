port module Pages.Org.Dynamic exposing (Model, Msg, page)

import Array
import Dict exposing (Dict)
import Generated.Org.Params as Params
import Generated.Routes exposing (Route)
import Global exposing (NID)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Encode exposing (string)
import Ports
import Spa.Page
import Utils.Spa exposing (Page, PageContext)


page : Page Params.Dynamic Model Msg model msg appMsg
page =
    Spa.Page.component
        { title = \{ model } -> String.join " | " [ model.asked_orga ]
        , init = init
        , update = always update
        , subscriptions = always subscriptions
        , view = always view
        }



-- Model
{-
   type alias Organisation =
       { graph : OrgaGraph }


   type alias OrgaGraph =
       Dict NID Node


   type NodeType
       = Circle
       | Role


   type alias Node =
       -- A node is either a circle or a role
       { display_name : String
       , children : List NID
       , parent : NID
       , ispublic : Bool
       , ntype : NodeType
       , root : Bool
       }


   type alias Model =
       { asked_orga : String
       , orga : Maybe Organisation
       }
-}


type alias CircleFocusState =
    { nidjs : NID
    , name : String
    , nodeType : String
    , path : Array.Array { name : String, nidjs : NID }
    }


type alias Model =
    { asked_orga : String
    , data : String
    , circle_focus : CircleFocusState
    , isLoading : Bool
    , route : Route
    }



-- INIT


init : PageContext -> Params.Dynamic -> ( Model, Cmd Msg, Cmd Global.Msg )
init { route } params =
    let
        focus =
            { nidjs = "545687"
            , name = "Sku Root"
            , nodeType = "Circle"
            , path = Array.fromList [ { name = "root", nidjs = "" } ]
            }

        model =
            { asked_orga = params.param1
            , data = ""
            , circle_focus = focus
            , isLoading = True
            , route = route
            }
    in
    ( model
    , Http.get { url = "/data1.json", expect = Http.expectString GotText }
    , Cmd.none
      --Ports.init_circlePacking data_json
    )



-- UPDATE


type Msg
    = GotText (Result Http.Error String)
    | CircleClick CircleFocusState
    | ChangeNodeFocus Int


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok data ->
                    ( model
                    , Cmd.none
                    , Ports.init_circlePacking data
                    )

                Err _ ->
                    ( model
                    , Cmd.none
                    , Cmd.none
                    )

        CircleClick focus ->
            ( { model | circle_focus = focus }
            , Cmd.none
            , Cmd.none
            )

        ChangeNodeFocus pos ->
            let
                nidjs =
                    case Array.get pos model.circle_focus.path of
                        Just x ->
                            x.nidjs

                        Nothing ->
                            ""
            in
            ( model, sendNodeFocus nidjs, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveData CircleClick


port receiveData : (CircleFocusState -> msg) -> Sub msg


port sendNodeFocus : NID -> Cmd msg



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "columns" ]
        [ div [ class "column is-1 is-fullheight is-hidden-mobile", id "leftPane" ]
            [ viewLeftPane model ]
        , div [ class "column is-10", id "mainPane" ]
            [ div [ class "columns" ]
                [ viewHelperBar model ]
            , div [ class "columns is-variable is-4" ]
                [ div [ class "column is-6" ]
                    [ div [ id "chart" ] []
                    , br [] []
                    , viewMandate model
                    ]
                , div [ class "column is-5", attribute "style" "width: 45%;" ] [ viewTensions model ]
                ]
            ]
        ]


viewHelperBar : Model -> Html Msg
viewHelperBar model =
    nav
        [ class "column is-full breadcrumb has-succeeds-separator"
        , attribute "aria-label" "breadcrumbs"
        ]
        [ i [ class "fas fa-angle-right" ] [ text "\u{00A0} " ]
        , Array.indexedMap
            (\i x ->
                if i == (Array.length model.circle_focus.path - 1) then
                    li [ class "is-active" ] [ a [ attribute "aria-current" "page", href "#" ] [ text x.name ] ]

                else
                    li [] [ a [ href "#", onClick (ChangeNodeFocus i) ] [ text x.name ] ]
            )
            model.circle_focus.path
            |> Array.toList
            |> ul [ attribute "style" "display: inline-flex;" ]
        ]


viewMandate : Model -> Html mgs
viewMandate model =
    div [ class "hero is-small is-light" ]
        [ div [ class "hero-body" ]
            [ h1 [ class "title is-3" ]
                [ i [ class "fas fa-scroll fa-xs" ] []
                , text ("\u{00A0} " ++ "Mandate")
                ]
            , hr [ class "has-background-grey-light" ] []
            , div [ class "content" ]
                [ h2 [ class "title is-4" ] [ text "Purpose" ]
                , div [] [ text "Devellop fractal6 and find a business model." ]
                , h2 [ class "title is-4" ] [ text "Responsabilities" ]
                , div [] [ text "Propose new feature, fix security bugs." ]
                , h2 [ class "title is-4" ] [ text "Domains" ]
                , div [] [ text "the code, the platform." ]
                ]
            ]
        ]


viewTensions : Model -> Html Msg
viewTensions model =
    div [ class "hero is-small is-light" ]
        [ div [ class "hero-body" ]
            [ h1 [ class "title is-3" ]
                [ i [ class "fas fa-exchange-alt fa-xs" ] []
                , text ("\u{00A0} " ++ "Tensions")
                ]
            , hr [ class "has-background-grey-light" ] []
            , div [ class "content" ]
                [ text "Considered an invitation do introduced sufficient understood instrument it. Of decisively friendship in as collecting at. No affixed be husband ye females brother garrets proceed. Least child who seven happy yet balls young. Discovery sweetness principle discourse shameless bed one excellent. Sentiments of surrounded friendship dispatched connection is he. Me or produce besides hastily up as pleased. Bore less when had and john shed hope. \n\nEcstatic advanced and procured civility not absolute put continue. Overcame breeding or my concerns removing desirous so absolute. My melancholy unpleasing imprudence considered in advantages so impression. Almost unable put piqued talked likely houses her met. Met any nor may through resolve entered. An mr cause tried oh do shade happy. \n\nWhole wound wrote at whose to style in. Figure ye innate former do so we. Shutters but sir yourself provided you required his. So neither related he am do believe. Nothing but you hundred had use regular. Fat sportsmen arranging preferred can. Busy paid like is oh. Dinner our ask talent her age hardly. Neglected collected an attention listening do abilities. \n\nWhole every miles as tiled at seven or. Wished he entire esteem mr oh by. Possible bed you pleasure civility boy elegance ham. He prevent request by if in pleased. Picture too and concern has was comfort. Ten difficult resembled eagerness nor. Same park bore on be. Warmth his law design say are person. Pronounce suspected in belonging conveying ye repulsive. \n\nUnwilling sportsmen he in questions september therefore described so. Attacks may set few believe moments was. Reasonably how possession shy way introduced age inquietude. Missed he engage no exeter of. Still tried means we aware order among on. Eldest father can design tastes did joy settle. Roused future he ye an marked. Arose mr rapid in so vexed words. Gay welcome led add lasting chiefly say looking. \n\nTolerably earnestly middleton extremely distrusts she boy now not. Add and offered prepare how cordial two promise. Greatly who affixed suppose but enquire compact prepare all put. Added forth chief trees but rooms think may. Wicket do manner others seemed enable rather in. Excellent own discovery unfeeling sweetness questions the gentleman. Chapter shyness matters mr parlors if mention thought. \n\nOut believe has request not how comfort evident. Up delight cousins we feeling minutes. Genius has looked end piqued spring. Down has rose feel find man. Learning day desirous informed expenses material returned six the. She enabled invited exposed him another. Reasonably conviction solicitude me mr at discretion reasonable. Age out full gate bed day lose. \n\nReceived overcame oh sensible so at an. Formed do change merely to county it. Am separate contempt domestic to to oh. On relation my so addition branched. Put hearing cottage she norland letters equally prepare too. Replied exposed savings he no viewing as up. Soon body add him hill. No father living really people estate if. Mistake do produce beloved demesne if am pursuit. \n\nEat imagine you chiefly few end ferrars compass. Be visitor females am ferrars inquiry. Latter law remark two lively thrown. Spot set they know rest its. Raptures law diverted believed jennings consider children the see. Had invited beloved carried the colonel. Occasional principles discretion it as he unpleasing boisterous. She bed sing dear now son half. \n\nInsipidity the sufficient discretion imprudence resolution sir him decisively. Proceed how any engaged visitor. Explained propriety off out perpetual his you. Feel sold off felt nay rose met you. We so entreaties cultivated astonished is. Was sister for few longer mrs sudden talent become. Done may bore quit evil old mile. If likely am of beauty tastes. \n\n"
                ]
            ]
        ]


viewLeftPane : Model -> Html Msg
viewLeftPane model =
    nav [ class "menu" ]
        [ p [ class "menu-label" ]
            [ div [ class "hero is-small is-primary is-bold" ]
                [ div [ class "hero-body has-text-centered" ] [ text model.asked_orga ] ]
            ]
        , ul [ class "menu-list" ]
            [ li [ class "menu-label" ]
                [ div [ class "hero is-small is-info is-bold" ]
                    [ div [ class "hero-body" ]
                        [ i [ class "far fa-circle fa-lg" ] []
                        , text ("\u{00A0} " ++ model.circle_focus.name)
                        ]
                    ]
                ]
            , li []
                [ ul [ class "menu-list" ]
                    [ li []
                        [ a []
                            [ i [ class "fas fa-scroll fa-xs" ] []
                            , text ("\u{00A0} " ++ "Mandates")
                            ]
                        ]
                    , li []
                        [ a []
                            [ i [ class "fas fa-exchange-alt fa-xs" ] []

                            --[ i [ class "fas fa-exclamation-circle fa-fw" ] []
                            , text ("\u{00A0} " ++ "Tensions")
                            ]
                        ]
                    , li []
                        [ a []
                            [ i [ class "fas fa-history fa-xs" ] []
                            , text ("\u{00A0} " ++ "Journal")
                            ]
                        ]
                    , li []
                        [ a []
                            [ i [ class "fas fa-user fa-xs" ] []
                            , text ("\u{00A0} " ++ "Members")
                            ]
                        ]
                    ]
                ]
            ]
        ]
