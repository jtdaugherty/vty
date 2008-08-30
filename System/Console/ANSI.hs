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