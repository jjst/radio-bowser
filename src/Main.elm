module Main exposing (main)

import Debug
import Browser
import Html exposing (Html, text, div)
import Http
import Json.Decode exposing (Decoder, field, string)

baseUrl : String 
baseUrl = "https://now-playing-42nq5.ondigitalocean.app/api/stations/fr/radiomeuh/now-playing"


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
  | Success String


init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , Http.get
      { url = baseUrl
      , expect = Http.expectJson GotNowPlayingInfo nowPlayingDecoder
      }
  )

nowPlayingDecoder : Decoder String
nowPlayingDecoder =
  field "title" string


-- UPDATE


type Msg
  = GotNowPlayingInfo (Result Http.Error String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotNowPlayingInfo result ->
      case result of
        Ok fullText ->
          (Success fullText, Cmd.none)

        Err error ->
          Debug.log "error" error |> \_ -> (Failure, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  case model of
    Failure ->
      text "I was unable to load your book."

    Loading ->
      text "Loading..."

    Success fullText ->
      div [] [ text fullText ]
