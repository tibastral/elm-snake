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
    { snake : List Position
    , apple : Position
    , arrows : Position
    , direction : Position
    , pressedKeys : List Keyboard.Key

    -- , keyboardModel : Keyboard.Model
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
    = PressedKeys (List Keyboard.Key) ( Int, Int )
    | ClientJoin
    | NoOpToBackend


type BackendMsg
    = Tick Posix
    | TickControl Posix
    | NewApple ( Int, Int )
    | NoOpBackendMsg


type ToFrontend
    = NewModel (List Position) Position
    | NoOpToFrontend
