module Main exposing (main)

import Browser
import Html exposing (Html,input, button, div, li, text, ul,h3)
import Html.Events exposing (onClick)
import Http
import HttpBuilder
import Json.Decode exposing (Decoder, int, list)
import Json.Decode.Pipeline
import RemoteData exposing (RemoteData(..), WebData)
import Task exposing (Task)
import Html.Attributes as A



type alias Model =
    { lords : WebData (List Character) }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { lords = NotAsked }, Cmd.none )


type Msg
    = FetchClick
    | LordsFetched (WebData (List Character))


fetchHouses : Task Http.Error (List Int)
fetchHouses =
    HttpBuilder.get "https://hacker-news.firebaseio.com/v0/topstories.json"
        |> HttpBuilder.withExpectJson (Json.Decode.list decodeHouse)
        |> HttpBuilder.toTask


fetchCurrentLord : Int -> Task Http.Error Character
fetchCurrentLord currentLord =
    HttpBuilder.get ("https://hacker-news.firebaseio.com/v0/item/" ++ String.fromInt currentLord ++ ".json")
        |> HttpBuilder.withExpectJson decodeCharacter
        |> HttpBuilder.toTask


fetchLords : Cmd Msg
fetchLords =
    fetchHouses
        |> Task.andThen
            (\houses ->
                houses
                    |> List.map fetchCurrentLord
                    |> Task.sequence
            )
        |> RemoteData.asCmd
        |> Cmd.map LordsFetched


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchClick ->
            ( { model | lords = Loading }
            , fetchLords
            )

        LordsFetched lords ->
            ( { model | lords = lords }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ case model.lords of
            NotAsked ->
                button [ onClick FetchClick ] [ text "Get recent stories" ]
                

            Loading ->
                text "Loading..."

            Success lords ->
                ul [] <|
                    List.map
                        (\char ->
                            li [] [text "(", text char.title, text ")", text " written by ", text char.by, input [ A.placeholder "Type a note about the story"] []]
                        )
                        lords

            Failure err ->
                text <| Debug.toString err
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Character =
    { by : String
    , title : String
    }

decodeCharacter : Decoder Character
decodeCharacter =
    Json.Decode.succeed Character
        |> Json.Decode.Pipeline.required "by" Json.Decode.string
        |> Json.Decode.Pipeline.required "title" Json.Decode.string

decodeHouse : Decoder Int
decodeHouse = int
