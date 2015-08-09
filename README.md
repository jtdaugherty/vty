[![Build Status](https://travis-ci.org/coreyoconnor/vty.png)](https://travis-ci.org/coreyoconnor/vty)

vty is a terminal interface library.

Project is hosted on github.com: https://github.com/coreyoconnor/vty

git clone git://github.com/coreyoconnor/vty.git

# Features

* Support for a large number of terminals. vt100, ansi, hurd, linux, screen etc
  etc. Anything with a sufficient terminfo entry.

* Automatic handling of window resizes.

* If the terminal support UTF-8 then vty supports Unicode output.

* Handles multi-column glyphs. (Requires user to properly configure terminal.)

* Efficient output. Output buffering and terminal state changes are minimized.

* Minimizes repaint area. Virtually eliminating the flicker problems that
  plagues ncurses programs.

* A pure, compositional interface for efficiently constructing display images.

* Automatically decodes keyboard keys into (key,[modifier]) tuples.

* Automatically supports refresh on Ctrl-L.

* Automatically supports timeout after for lone ESC. The timeout is
  customizable.

* Interface is designed for easy compatible extension.

* Supports ANSI graphics modes (SGR as defined in console_codes(4)) with a
  type-safe interface. Gracefull fallback for terminals that do not support, or
  partially support the standard ANSI graphics modes.

* Properly handles cleanup, but not due to signals.

* Comprehensive test suite.

# Known Issues

* Terminals have numerous quirks and bugs. vty picks what works best for the
  author in ambigious, or buggy situations.

* Signal handling of STOP, TERM and INT are non existent.

* The character encoding of the terminal is assumed to be UTF-8 if
  unicode is used.

* Terminfo is assumed to be correct unless there is an override configured.
  Some terminals will not have correct special key support (shifted F10 etc).
  See Config for customizing vty's behavior for a particular terminal.

* Uses the TIOCGWINSZ ioctl to find the current window size, which appears to be
  limited to Linux and BSD.

# Platform Support

## Posix Terminals

Uses terminfo to determine terminal protocol. With some special rules to handle
some omissions from terminfo.

## Windows

cygwin only.

# Development Notes

## Under NixOS

After installing ncurses to user env.

~~~
LIBRARY_PATH=$HOME/.nix-profile/lib/ cabal configure --enable-tests --extra-lib-dirs=$HOME/.nix-profile/lib
LIBRARY_PATH=$HOME/.nix-profile/lib/ cabal build
LIBRARY_PATH=$HOME/.nix-profile/lib/ cabal test
~~~

## Coverage

As of last testing, profiling causes issues with coverage when enabled. To
evaluate coverage configure as follows:

~~~
rm -rf dist ; cabal configure --enable-tests --enable-library-coverage \
  --disable-library-profiling \
  --disable-executable-profiling
~~~

## Profiling


~~~
rm -rf dist ; cabal configure --enable-tests --disable-library-coverage \
  --enable-library-profiling \
  --enable-executable-profiling
~~~
