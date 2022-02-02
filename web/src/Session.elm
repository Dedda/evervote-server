module Session exposing (..)

import Browser.Navigation as Nav

type Session
    = Guest Nav.Key

navKey : Session -> Nav.Key
navKey session =
    case session of
        Guest key ->
            key