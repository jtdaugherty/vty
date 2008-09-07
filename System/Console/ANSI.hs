-- | Provides ANSI terminal support for Windows and ANSI terminal software running on a Unix-like operating system.
--
-- The ANSI escape codes are described at <http://en.wikipedia.org/wiki/ANSI_escape_code> and provide a rich range of
-- functionality for terminal control, which includes:
--
--  * Colored text output, with control over both foreground and background colors
--
--  * Hiding or showing the cursor
--
--  * Moving the cursor around
--
--  * Clearing parts of the screen
--
-- The most frequently used parts of this ANSI command set are exposed with a platform independent interface by
-- this module.  Every function exported comes in three flavours:
--
--  * Vanilla: has an @IO ()@ type and doesn't take a @Handle@.  This just outputs the ANSI command directly on
--    to the terminal corresponding to stdout.  Commands issued like this should work as you expect on both Windows
--    and Unix.
--
--  * Chocolate: has an @IO ()@ type but takes a @Handle@.  This outputs the ANSI command on the terminal corresponding
--    to the supplied handle.  Commands issued like this should also work as your expect on both Windows and Unix.
--
--  * Strawberry: has a @String@ type and just consists of an escape code which can be added to any other bit of text
--    before being output.  This version of the API is often convenient to use, but due to fundamental limitations in
--    Windows ANSI terminal support will only work on Unix.  On Windows these codes will always be the empty string,
--    so it is possible to use them portably for e.g. coloring console output on the understanding that you will only
--    see colors if you are running on a Unix-like operating system.
#if defined(WINDOWS)
module System.Console.ANSI (
        module System.Console.ANSI.Windows
    ) where

import System.Console.ANSI.Windows

#elif defined(UNIX)

module System.Console.ANSI (
        module System.Console.ANSI.Unix
    ) where

import System.Console.ANSI.Unix

#else

#error Unsupported platform for the ansi-terminal package

#endif