module Main exposing (main)

import DateFormat.Relative exposing (relativeTime)
import Dict exposing (Dict)
import Dict
import Browser
import Html exposing (Html, img, text, div, small, p, h5)
import Html.Attributes exposing (href, class, src, style)
import Http
import Json.Decode exposing (Decoder, list, field, map, map2, map3, map4, nullable, string, succeed)
import Maybe.Extra
import Process
import Random
import Task
import Time



import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Spinner as Spinner
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Size as Size
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Badge as Badge

baseUrl : String
baseUrl = "https://now-playing-42nq5.ondigitalocean.app/api"

stationRefreshRateSeconds : Float
stationRefreshRateSeconds = 10

jitterSeconds : Float
jitterSeconds = 4

-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL


type alias Model =
  { stations : Stations
  , time : Time.Posix
  }

type Stations
  = Failure
  | Loading
  | Success (StationDict)

type alias StationId = String
type alias StationInfo = {name: String, favicon: Maybe String, nowPlaying: Maybe NowPlayingInfo, loadingState: LoadingState}
type LoadingState
  = CurrentlyLoading
  | LoadedAt Time.Posix
type alias NowPlayingInfo = {title: String, itemType: String}
type alias StationDict = Dict StationId StationInfo

init : () -> (Model, Cmd Msg)
init _ =
  ( Model Loading (Time.millisToPosix 0)
  , getStationList
  )

getStationList : Cmd Msg
getStationList =
  Http.get
      { url = baseUrl ++ "/stations"
      , expect = Http.expectJson GotStationList stationListDecoder
      }

getNowPlaying : StationId -> Time.Posix -> Cmd Msg
getNowPlaying stationId time =
  Http.get
      { url = baseUrl ++ "/stations/" ++ stationId ++ "/now-playing"
      , expect = Http.expectJson (GotNowPlayingInfo stationId time) nowPlayingDecoder
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
    stationInfoDecoder

stationInfoDecoder : Decoder StationInfo
stationInfoDecoder =
  map4 StationInfo
    (field "name" string)
    (field "favicon" (nullable string))
    (succeed Nothing)
    (succeed CurrentlyLoading)


-- UPDATE


type Msg
  = GotNowPlayingInfo StationId Time.Posix (Result Http.Error (Maybe NowPlayingInfo))
  | GotStationList (Result Http.Error StationDict)
  | ScheduleNowPlayingUpdate StationId Float
  | UpdateNowPlaying StationId Time.Posix


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotStationList result ->
      case result of
        Ok stations ->
          ({model | stations = Success stations }, Dict.keys stations |> List.map (scheduleNowPlayingUpdateIn 0) |> Cmd.batch)

        Err error ->
          ({ model | stations = Failure }, Cmd.none)
    GotNowPlayingInfo stationId time result ->
      let
        newStations =
          case result of
            Ok np ->
              case model.stations of
                Success stations ->
                  let
                      updatedStations =
                        stations |> Dict.update stationId (Maybe.map (\station -> { station | nowPlaying = np, loadingState = (LoadedAt time) }))
                  in
                      Success updatedStations
                _ -> model.stations

            Err error ->
              model.stations
        isProgramme = case result of
            Ok (Just {itemType}) -> itemType == "programme"
            _ -> False
        nextUpdateDelaySeconds =
          if isProgramme then
            120
          else if newStations /= model.stations then
            120
          else
            20
        cmd = Random.generate (ScheduleNowPlayingUpdate stationId) (Random.float (nextUpdateDelaySeconds - jitterSeconds) (nextUpdateDelaySeconds + jitterSeconds))
      in
        ({ model | stations = newStations }, cmd)
    ScheduleNowPlayingUpdate stationId delaySeconds ->
          (model, scheduleNowPlayingUpdateIn delaySeconds stationId)
    UpdateNowPlaying stationId time ->
      ({ model | time = time }, getNowPlaying stationId time)


scheduleNowPlayingUpdateIn : Float -> StationId -> Cmd Msg
scheduleNowPlayingUpdateIn delaySeconds stationId=
  Process.sleep (delaySeconds * 1000) |> Task.andThen (\_ -> Time.now) |> Task.perform (UpdateNowPlaying stationId)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view {stations, time} =
  case stations of
    Failure ->
      text "I was unable to load radio stations."

    Loading ->
      text "Loading..."

    Success s ->
      let
        items = s
          |> Dict.values
          |> List.map (viewStation time)
      in
        Grid.container []
            [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
            , Grid.row []
                [ Grid.col [] [ ListGroup.custom items ] ]

            ]

viewStation : Time.Posix -> StationInfo -> ListGroup.CustomItem Msg
viewStation currentTime station =
  let
      -- Pretend we're a tiny bit into the future, just so `relativetime` displays times uniformly.
      effectiveTime =
        currentTime
          |> Time.posixToMillis
          |> (+) 1000
          |> Time.millisToPosix
      loadedWhenInfo =
        case station.loadingState of
          CurrentlyLoading -> Spinner.spinner [ Spinner.small ] []
          LoadedAt time -> text (relativeTime effectiveTime time)
  in
  ListGroup.anchor
      [ ListGroup.attrs [ href "#", Flex.col, Flex.alignItemsStart ] ]
      [ div [ Flex.block, Flex.justifyBetween, Size.w100 ]
          [ h5 [ Spacing.m1 ]
              [ text station.name
              ]
          , small [ Spacing.m1, class "ml-auto" ] [ loadedWhenInfo ]
          ]
      , p [ Spacing.mb1 ] [text (Maybe.Extra.unwrap "" viewNowPlayingInfo station.nowPlaying)]
      ]

viewNowPlayingInfo : NowPlayingInfo -> String
viewNowPlayingInfo nowPlayingInfo =
  let
      icon = case nowPlayingInfo.itemType of
        "song" -> "🎵"
        "programme" -> "🎤"
        _ -> ""
  in
  icon ++ " " ++ nowPlayingInfo.title
