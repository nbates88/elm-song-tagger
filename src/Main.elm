port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import String
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Url
import Url.Parser as Parser
import Url.Parser exposing ((</>))
import Array
import Http
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import Debug
-- MAIN


main =
  Browser.application { init = init 
    , update = update
    , view = view  
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked}



-- MODEL


type alias Model =   
    { key : Nav.Key
    , url : Url.Url
    , access_token: String
    , my_tracks: List Track
    }

type alias Track =
    { name : String
    , album_image: String
    , preview_url: String
    }


init :  Maybe String -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    case flags of
        Just token -> 
            let model = Model key url token []
             in
             (model, fetchPlaylists model)
        Nothing -> 
            setStorageHelper (Model key url (processUrl url) [])

processUrl: Url.Url -> String
processUrl url = 
    let 
        urlFragment =  url.fragment
    in 
    case urlFragment of
        Just fragment -> 
            getFromList "&" 0 fragment
            |> String.replace "access_token=" ""
        Nothing -> ""

getFromList: String -> Int -> String -> String
getFromList delimiter position stringToParse =
    let arr  = Array.fromList (String.split delimiter stringToParse)
        in Maybe.withDefault "" (Array.get position arr)

fetchPlaylists : Model -> Cmd Msg
fetchPlaylists model =
    Http.request
    { method = "GET"
    , expect = Http.expectJson FetchPlaylistsCompleted tracksDecoder
    , headers = [ Http.header "Authorization" ("Bearer " ++ model.access_token) ]
    , url = "https://api.spotify.com/v1/me/top/tracks"
    , body = Http.emptyBody
    , timeout = Nothing
    , tracker = Nothing
    }

trackDecoder: Decoder Track
trackDecoder = 
    Decode.map3 Track
    (field "name" Decode.string)
    (field "album" (field "images" (Decode.index 1 (field "url" Decode.string))))
    (field "preview_url" Decode.string)

tracksDecoder : Decoder (List Track)
tracksDecoder =
    field "items" (Decode.list trackDecoder)
  
fetchPlaylistsCompleted : Model -> Result Http.Error (List Track) -> ( Model, Cmd Msg )
fetchPlaylistsCompleted model result =
    case result of
        Ok tracks ->
            ( { model | my_tracks = tracks }, Cmd.none)
        Err _ ->
            ( model, Cmd.none )

-- Helper to update model and set localStorage with the updated model

setStorageHelper : Model -> ( Model, Cmd Msg )
setStorageHelper model =
    ( model, setStorage model.access_token )

-- Ports


port setStorage : String -> Cmd msg

-- port removeStorage : Cmd msg

-- UPDATE

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | FetchPlaylistsCompleted (Result Http.Error (List Track))
  | GetTracks

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )
    UrlChanged url ->
        ( { model | url = url }
        , Cmd.none
        )
    GetTracks -> 
        (model, fetchPlaylists model)
    FetchPlaylistsCompleted result -> 
        fetchPlaylistsCompleted model result

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none


-- VIEW


view : Model -> Browser.Document Msg
view model =
    let 
    
        loggedIn: Bool
        loggedIn = 
            if String.length model.access_token > 0 then
                True
            else
                False
        hasTracks: Bool
        hasTracks = 
            if List.length model.my_tracks > 0 then 
                True
            else    
                False

        homeView = 
            if hasTracks then
                 div [class "tracks"]
                        (List.map (\l -> div [class "track"] [
                            p [] [text l.name ]
                            , img [src l.album_image ] []
                            , audio
                                [ src l.preview_url
                                , id "audio-player"
                                , controls True
                                ]
                                []
                            
                        ]) model.my_tracks)
            else
                if loggedIn then
                    div [class "get-tracks"] [
                        button [ class "btn btn-success", onClick GetTracks ] [ text "Get Tracks!" ]
                    ]
                else
                    div [id "login"][
                    p [ class "text-center" ] [
                        a [  class "btn btn-success", href "https://accounts.spotify.com/authorize?client_id=356e5c975b12471d9875649901894fbb&redirect_uri=http:%2F%2Flocalhost:8080%2F&scope=user-top-read&response_type=token" ] [ text "Log In" ]]
                    ]
        in 
  { title = "Top Tracks"
  , body =
      [ 
        div [ class "container" ] [
            h2 [ class "text-center" ] [ text "Top Tracks" ]
            , 
            homeView
        ]
      ]
  }
