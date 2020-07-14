-- This code was adapted from "https://elm.christmas/2018/16"


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
    { stories : WebData (List Story) }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { stories = NotAsked }, Cmd.none )


type Msg
    = FetchClick
    | StoriesFetched (WebData (List Story))


fetchIds : Task Http.Error (List Int)
fetchIds =
    HttpBuilder.get "https://hacker-news.firebaseio.com/v0/topstories.json"
        |> HttpBuilder.withExpectJson (Json.Decode.list decodeId)
        |> HttpBuilder.toTask


fetchCurrentStory : Int -> Task Http.Error Story
fetchCurrentStory currentStory =
    HttpBuilder.get ("https://hacker-news.firebaseio.com/v0/item/" ++ String.fromInt currentStory ++ ".json")
        |> HttpBuilder.withExpectJson decodeStory
        |> HttpBuilder.toTask


fetchStories : Cmd Msg
fetchStories =
    fetchIds
        |> Task.andThen
            (\ids ->
                ids
                    |> List.map fetchCurrentStory
                    |> Task.sequence
            )
        |> RemoteData.asCmd
        |> Cmd.map StoriesFetched


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchClick ->
            ( { model | stories = Loading }
            , fetchStories
            )

        StoriesFetched stories ->
            ( { model | stories = stories }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ case model.stories of
            NotAsked ->
                button [ onClick FetchClick ] [ text "Get recent stories" ]
                

            Loading ->
                text "Loading..."

            Success stories ->
                ul [] <|
                    List.map
                        (\stor ->
                        li[A.class "alert alert-primary"][ li [A.class "alert alert-dark"] [text "Story by: ", text stor.by],
                            li [A.class "alert alert-dark"] [text "Title: ",text stor.title],li[A.class "alert alert-dark"][input[A.class "col-md-6",A.placeholder "Add Your notes"][]]]
                        )
                        stories

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


type alias Story =
    { by : String
    , title : String
    }

decodeStory : Decoder Story
decodeStory =
    Json.Decode.succeed Story
        |> Json.Decode.Pipeline.required "by" Json.Decode.string
        |> Json.Decode.Pipeline.required "title" Json.Decode.string

decodeId : Decoder Int
decodeId = int
