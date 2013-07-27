module Verify.Graphics.Vty.Terminal where

-- A list of terminals that should be supported.
-- This started with a list of terminals ubuntu supported. Then those terminals that really could
-- not be supported were removed.
terminals_of_interest = 
    [ "vt100"
    , "vt220"
    , "vt102"
    , "xterm-r5"
    , "xterm-xfree86"
    , "xterm-r6"
    , "xterm-256color"
    , "xterm-vt220"
    , "xterm-debian"
    , "xterm-mono"
    , "xterm-color"
    , "xterm"
    , "mach"
    , "mach-bold"
    , "mach-color"
    , "linux"
    , "ansi"
    , "hurd"
    , "Eterm"
    , "pcansi"
    , "screen-256color"
    , "screen-bce"
    , "screen-s"
    , "screen-w"
    , "screen"
    , "screen-256color-bce"
    , "sun"
    , "rxvt"
    , "rxvt-unicode"
    , "rxvt-basic"
    , "cygwin"
    ]
