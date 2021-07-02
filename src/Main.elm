module Main exposing (main)

import Debug
import Dict exposing (Dict)
import Dict
import Browser
import Html exposing (Html, text, div)
import Http
import Json.Decode exposing (Decoder, list, field, map, map2, nullable, string)
import Maybe.Extra
import Process
import Task



import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Badge as Badge

baseUrl : String
baseUrl = "https://now-playing-42nq5.ondigitalocean.app/api"

stationRefreshRateSeconds : Float
stationRefreshRateSeconds = 10

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
type alias StationInfo = {name: String, nowPlaying: Maybe NowPlayingInfo}
type alias NowPlayingInfo = {title: String, itemType: String}
type alias StationDict = Dict StationId StationInfo

init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , getStationList
  )

getStationList : Cmd Msg
getStationList =
  Http.get
      { url = baseUrl ++ "/stations"
      , expect = Http.expectJson GotStationList stationListDecoder
      }

getNowPlaying : StationId -> Cmd Msg
getNowPlaying stationId =
  Http.get
      { url = baseUrl ++ "/stations/" ++ stationId ++ "/now-playing"
      , expect = Http.expectJson (GotNowPlayingInfo stationId) nowPlayingDecoder
      }

nowPlayingDecoder : Decoder (Maybe NowPlayingInfo)
nowPlayingDecoder =
  map2 (Maybe.map2 NowPlayingInfo)
    (field "title" (nullable string))
    (field "type" (nullable string))

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
  = GotNowPlayingInfo StationId (Result Http.Error (Maybe NowPlayingInfo))
  | GotStationList (Result Http.Error StationDict)
  | UpdateNowPlaying StationId


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotStationList result ->
      case result of
        Ok stations ->
          (Success stations, Dict.keys stations |> List.map getNowPlaying |> Cmd.batch)

        Err error ->
          Debug.log "error" error |> \_ -> (Failure, Cmd.none)
    GotNowPlayingInfo stationId result ->
      let
        newModel =
          case result of
            Ok maybeTitle ->
              case model of
                Success stations ->
                  let
                      updatedStations = 
                        stations |> Dict.update stationId (Maybe.map (\station -> { station | nowPlaying = maybeTitle }))
                  in
                      Success updatedStations
                _ -> model

            Err error ->
              Debug.log "error" error |> \_ -> model
        nextUpdateDelaySeconds = if newModel == model then 20 else 120
        cmd = Process.sleep (nextUpdateDelaySeconds * 1000) |> Task.andThen (\_ -> Task.succeed (UpdateNowPlaying stationId)) |> Task.perform identity
      in
        (newModel, cmd)
    UpdateNowPlaying stationId ->
      Debug.log "updating station" stationId |> \_ -> (model, getNowPlaying stationId)




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
              ([ text station.name ] ++ Maybe.Extra.toList (Maybe.map viewNowPlayingInfo station.nowPlaying))
          )
      in
        -- div [] stationsDivs
        Grid.container []
            [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
            , Grid.row []
                [ Grid.col [] [ ListGroup.ul stationsDivs ] ]

            ]

viewNowPlayingInfo : NowPlayingInfo -> Html Msg
viewNowPlayingInfo nowPlayingInfo =
  let
      icon = case nowPlayingInfo.itemType of
        "song" -> "🎵"
        "programme" -> "🎤"
        _ -> ""
  in
  Badge.badgePrimary [] [text (icon ++ " " ++ nowPlayingInfo.title)]
