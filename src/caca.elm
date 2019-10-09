
import Browser

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Json.Decode exposing (Decoder, field, string)




-- MAIN

main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view}




-- MODEL

type StatusRequestType
  = Failure
  | Loading
  | Success String


type alias Model =
  { statusRequest : StatusRequestType
  , content : String
  }




init : () -> (Model, Cmd Msg)
init _ =
  ( {statusRequest = Loading, content = ""}
  , Http.get
      { url = "http://localhost:3000/viejas_locas/homero"
      , expect = Http.expectJson GotText tabDecoder
      }
  )


tabDecoder : Decoder String
tabDecoder =
  field "body" string




-- UPDATE


type Msg
  = GotText (Result Http.Error String) | Change String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Change newContent ->
        ({ model | content = newContent }, Cmd.none)
    GotText result ->
      case result of
        Ok fullText ->
          ({ model | statusRequest = Success fullText }, Cmd.none)

        Err _ ->
          ({ model | statusRequest = Failure }, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  case model.statusRequest of
      Failure ->
        text "I was unable to load your book."

      Loading ->
        text "Loading..."

      Success fullText ->
        div [] [
            input [ placeholder "Text to reverse", value model.content, onInput Change ] [],
            pre [ ] [ text fullText ]
        ]

