module Main exposing (main)

import Debug
import Dict exposing (Dict)
import Dict
import Browser
import Html exposing (Html, text, div)
import Http
import Json.Decode exposing (Decoder, list, field, map, map2, string)


import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Badge as Badge

baseUrl : String
baseUrl = "https://now-playing-42nq5.ondigitalocean.app/api"


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
  | Success (StationDict)

type alias StationId = String
type alias StationInfo = {name: String, nowPlaying: Maybe String}
type alias StationDict = Dict StationId StationInfo

init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , Http.get
      { url = baseUrl ++ "/stations"
      , expect = Http.expectJson GotStationList stationListDecoder
      }
  )

nowPlayingDecoder : Decoder String
nowPlayingDecoder =
  field "title" string

stationListDecoder : Decoder StationDict
stationListDecoder =
  field "items" (list stationListItemDecoder) |> map Dict.fromList

stationListItemDecoder : Decoder (StationId, StationInfo)
stationListItemDecoder =
  map2 Tuple.pair
    (field "id" string)
    (field "name" (map (\n -> { name = n, nowPlaying = Nothing }) string))

-- UPDATE


type Msg
  = GotNowPlayingInfo (Result Http.Error String)
  | GotStationList (Result Http.Error StationDict)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotStationList result ->
      case result of
        Ok stations ->
          (Success stations, Cmd.none)

        Err error ->
          Debug.log "error" error |> \_ -> (Failure, Cmd.none)
    GotNowPlayingInfo _ ->
      (model, Cmd.none)




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  case model of
    Failure ->
      text "I was unable to load radio stations."

    Loading ->
      text "Loading..."

    Success stations ->
      let
        stationsDivs = stations
          |> Dict.values
          |> List.map (\station ->
            ListGroup.li
              [ ListGroup.attrs [ Flex.block, Flex.justifyBetween, Flex.alignItemsCenter ] ]
              [ text station.name
              , Badge.badgePrimary [] [ text "14" ]
              ]
          )
      in
        -- div [] stationsDivs
        Grid.container []
            [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
            , Grid.row []
                [ Grid.col [] [ ListGroup.ul stationsDivs ] ]

            ]
