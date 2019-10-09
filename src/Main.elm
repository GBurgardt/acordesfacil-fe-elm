import Browser

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
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
init _ = ({statusRequest = Success "", content = ""}, Cmd.none)


tabDecoder : Decoder String
tabDecoder =
  field "body" string




-- UPDATE
type Msg
  = GotText (Result Http.Error String) | Change String | OnClickSearch


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Change newContent ->
        ({ model | content = newContent }, Cmd.none)
    GotText result ->
      case result of
        Ok fullText ->
          (
            { 
              model | 
                statusRequest = 
                  if String.contains "<!DOCTYPE HTML PUB" fullText
                  then Failure
                  else Success fullText 
            }, 
            Cmd.none
          )

        Err _ ->
          ({ model | statusRequest = Failure }, Cmd.none)
    OnClickSearch -> 
      (
        { model | statusRequest = Loading }, 
        Http.get 
          { 
            url = "http://localhost:3000/" ++ model.content,
            expect = Http.expectJson GotText tabDecoder
          }
      )
      

-- VIEW
view : Model -> Html Msg
view model =
  
  div [] [
    input [ placeholder "", value model.content, onInput Change ] [],
    button [ onClick OnClickSearch ] [ text "Search" ],
    pre [ ] [
      case model.statusRequest of
        Failure ->
          text "Algo escribiste mal loco"

        Loading ->
          text "Runneando..."

        Success fullText -> text fullText
    ]

  ]


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
