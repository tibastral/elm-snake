module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Keyboard
import Time exposing (Posix)
import Url exposing (Url)


type alias Position =
    ( Int, Int )


type alias FrontendModel =
    { snake : List Position
    , apple : Position
    , arrows : Position
    , direction : Position
    , pressedKeys : List Keyboard.Key

    -- , keyboardModel : Keyboard.Model
    , size : Position
    , touch : Position
    , url : Url.Url
    , key : Key
    }



-- type alias FrontendModel =
--     { key : Key
--     , message : String
--     }


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | Tick Posix
    | TickControl Posix
    | KeyboardMsg Keyboard.Msg
    | NewApple ( Int, Int )
    | Resize Position
    | StartMsg ( Float, Float )
    | EndMsg ( Float, Float )


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
