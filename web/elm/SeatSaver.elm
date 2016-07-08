module SeatSaver exposing ( Model, Msg, init, update, view )

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.App as Html
import Task exposing (Task)
import Task.Extra
import Http
import Json.Decode as Json exposing ((:=))

main : Program Never
main =
  Html.program
    { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }

-- port tasks : Signal (Task.Task Never ()) -- FIXME
-- port tasks =
--   app.tasks

-- MODEL

type alias Seat =
  { seatNo : Int
  , occupied : Bool
  }

type alias Model =
  List Seat

init : (Model, Cmd Msg)
init =
  ([], fetchSeats)

-- UPDATE

type Msg = Toggle Seat | SetSeats (Maybe Model)

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    Toggle seatToToggle ->
      let
        updateSeat seatFromModel =
          if seatFromModel.seatNo == seatToToggle.seatNo then
            { seatFromModel | occupied = not seatFromModel.occupied }
          else seatFromModel
      in
        (List.map updateSeat model, Cmd.none)
    SetSeats seats ->
      let
        newModel = Maybe.withDefault model seats
      in
        (newModel, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  ul [ class "seats" ] (List.map seatItem model)

seatItem : Seat -> Html Msg
seatItem seat =
  let
    occupiedClass =
      if seat.occupied then "occupied" else "available"
  in
    li
      [ class ("seat " ++ occupiedClass)
      , onClick (Toggle seat)
      ]
      [ text (toString seat.seatNo) ]

-- EFFECTS

fetchSeats: Cmd Msg
fetchSeats =
  Http.get decodeSeats "http://localhost:4000/api/seats"
    |> Task.toMaybe
    |> Task.map SetSeats
    |> Task.Extra.performFailproof identity -- FIXME

decodeSeats: Json.Decoder Model
decodeSeats =
  let
    seat =
      Json.object2 (\seatNo occupied -> (Seat seatNo occupied))
        ("seatNo" := Json.int)
        ("occupied" := Json.bool)
  in
    Json.at ["data"] (Json.list seat)
