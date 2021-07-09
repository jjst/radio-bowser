module Main exposing (main)

import DateFormat.Relative exposing (relativeTime)
import Dict exposing (Dict)
import Dict
import Browser
import Html exposing (Html, img, text, div, small, p, h5, node)
import Html.Attributes exposing (href, class, src, style, rel, href)
import Http
import Json.Decode exposing (Decoder, list, field, map, map2, map3, map5, nullable, string, succeed)
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

timeRefreshRateSeconds : Float
timeRefreshRateSeconds = 30

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
type alias StationInfo = {name: String, favicon: Maybe String, nowPlaying: Maybe NowPlayingInfo, loadingState: LoadingState, fresh: Bool }
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
  map5 StationInfo
    (field "name" string)
    (field "favicon" (nullable string))
    (succeed Nothing)
    (succeed CurrentlyLoading)
    (succeed False)


-- UPDATE


type Msg
  = GotNowPlayingInfo StationId Time.Posix (Result Http.Error (Maybe NowPlayingInfo))
  | GotStationList (Result Http.Error StationDict)
  | ScheduleNowPlayingUpdate StationId Float
  | UpdateNowPlaying StationId Time.Posix
  | UpdateTime Time.Posix


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotStationList result ->
      case result of
        Ok stations ->
          let
              command =
                stations
                  |> Dict.keys
                  |> List.reverse
                  |> List.map (scheduleNowPlayingUpdateIn 0)
                  |> Cmd.batch
          in
          ({model | stations = Success stations }, command)

        Err error ->
          ({ model | stations = Failure }, Cmd.none)
    GotNowPlayingInfo stationId time result ->
      let
        updateStation station =
          case result of
            Ok np ->
              { station | nowPlaying = np, loadingState = (LoadedAt time), fresh = (station.nowPlaying /= np) }
            Err error ->
              { station | nowPlaying = Nothing, loadingState = (LoadedAt time), fresh = False }
        newStations =
          case model.stations of
            Success stations ->
              stations |> Dict.update stationId (Maybe.map updateStation) |> Success
            _ ->
              model.stations
        isProgramme = case result of
            Ok (Just {itemType}) -> itemType == "programme"
            _ -> False
        nextUpdateDelaySeconds =
          if isProgramme then
            120
          else
            20
        cmd = Random.generate (ScheduleNowPlayingUpdate stationId) (Random.float (nextUpdateDelaySeconds - jitterSeconds) (nextUpdateDelaySeconds + jitterSeconds))
      in
        ({ model | stations = newStations }, cmd)
    ScheduleNowPlayingUpdate stationId delaySeconds ->
          (model, scheduleNowPlayingUpdateIn delaySeconds stationId)
    UpdateNowPlaying stationId time ->
      let
          newStations =
            case model.stations of
              Success stations ->
                stations |> Dict.update stationId (Maybe.map (\s -> { s | fresh = False, loadingState = CurrentlyLoading })) |> Success
              _ ->
                model.stations
      in
      ({ stations = newStations, time = time }, getNowPlaying stationId time)
    UpdateTime time ->
      ({ model | time = time }, Cmd.none)


scheduleNowPlayingUpdateIn : Float -> StationId -> Cmd Msg
scheduleNowPlayingUpdateIn delaySeconds stationId=
  Process.sleep (delaySeconds * 1000) |> Task.andThen (\_ -> Time.now) |> Task.perform (UpdateNowPlaying stationId)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every (timeRefreshRateSeconds * 1000) UpdateTime



-- VIEW


view : Model -> Html Msg
view {stations, time} =
  let
      mainView =
        case stations of
          Failure ->
            text "I was unable to load radio stations."

          Loading ->
            text ""

          Success s ->
            s |> Dict.values
              |> List.map (viewStation time)
              |> ListGroup.custom
  in
  Grid.container []
      [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
      , css "style.css"
      , Grid.row []
          [ Grid.col [] [ mainView ] ]

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
      classes = if station.fresh then "now-playing fresh" else "now-playing"
  in
  ListGroup.anchor
      [ ListGroup.attrs [ href "#", Flex.col, Flex.alignItemsStart, class classes ] ]
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

css : String -> Html a
css path =
    node "link" [ rel "stylesheet", href path ] []
