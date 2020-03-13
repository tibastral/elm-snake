module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Keyboard
import Lamdera exposing (ClientId)
import Set
import Time exposing (Posix)
import Url exposing (Url)


type alias Position =
    ( Int, Int )


type alias FrontendModel =
    { snake : List Position
    , apple : Position
    , direction : Position
    , pressedKeys : List Keyboard.Key
    , size : Position
    , touch : Position
    , url : Url.Url
    , key : Key
    }


type alias BackendModel =
    { snake : List Position
    , apple : Position
    , arrows : Position
    , direction : Position
    , size : Position
    , touch : Position
    , clients : Set.Set ClientId
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | KeyboardMsg Keyboard.Msg
    | Resize Position
    | StartMsg ( Float, Float )
    | EndMsg ( Float, Float )


type ToBackend
    = UpdateArrows ( Int, Int )
    | ClientJoin
    | NoOpToBackend


type BackendMsg
    = Tick Posix
    | NewApple ( Int, Int )
    | NoOpBackendMsg


type ToFrontend
    = NewModel (List Position) Position
    | NoOpToFrontend
