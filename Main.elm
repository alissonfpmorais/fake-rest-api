import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, map4, field, string)



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL


type Model
  = Failure
  | Loading
  | Success Profile


init : () -> (Model, Cmd Msg)
init _ =
  (Loading, getProfile)



-- UPDATE


type alias Profile =
    { account : String
    , email : String
    , chat_user_id : String
    , chat_token : String
    }


type Msg
  = MorePlease
  | GotProfile (Result Http.Error Profile)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      (Loading, getProfile)

    GotProfile result ->
      case result of
        Ok profile ->
          (Success profile, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text "Get Profile" ]
    , viewProfile model
    ]


viewProfile : Model -> Html Msg
viewProfile model =
  case model of
    Failure ->
      div []
        [ text "I could not load the profile for some reason. "
        , button [ onClick MorePlease ] [ text "Try Again!" ]
        ]

    Loading ->
      text "Loading..."

    Success profile ->
      div []
        [ button [ onClick MorePlease, style "display" "block" ] [ text "Reload!" ]
        , p [] [ text ("Account: " ++ profile.account) ]
        , p [] [ text ("Email: " ++ profile.email) ]
        , p [] [ text ("User Id: " ++ profile.chat_user_id) ]
        , p [] [ text ("Auth Token: " ++ profile.chat_token) ]
        ]



-- HTTP


getProfile : Cmd Msg
getProfile =
  Http.get
    { url = "https://my-json-server.typicode.com/alissonfpmorais/fake-rest-api/profile"
    , expect = Http.expectJson GotProfile profileDecoder
    }


profileDecoder : Decoder Profile
profileDecoder =
  map4 Profile
      (field "data" (field "user" (field "account" string)))
      (field "data" (field "user" (field "email" string)))
      (field "data" (field "user" (field "chat_user_id" string)))
      (field "data" (field "user" (field "chat_token" string)))
  
  
  
