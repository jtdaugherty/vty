vty is a terminal interface library.

Project is hosted on github.com: https://github.com/coreyoconnor/vty

git clone git://github.com/coreyoconnor/vty.git

# Features

* Automatic handling of window resizes.

* Supports Unicode characters on output, automatically setting and
  resetting UTF-8 mode for xterm. Other terminals are assumed to support 

* Efficient output. 

* Minimizes repaint area, thus virtually eliminating the flicker
  problem that plagues ncurses programs.

* A pure, compositional interface for efficiently constructing display
  images.

* Automatically decodes keyboard keys into (key,[modifier]) tuples.

* Automatically supports refresh on Ctrl-L.

* Automatically supports timeout after 50ms for lone ESC (a barely
  noticable delay)

* Interface is designed for relatively easy compatible extension.

* Supports all ANSI SGR-modes (defined in console_codes(4)) with
  a type-safe interface. 

* Properly handles cleanup, but not due to signals.

# Known Issues

* Signal handling of STOP, TERM and INT are non existent.

* The character encoding of the terminal is assumed to be UTF-8.

* Terminfo is assumed to be correct unless the terminal (as declared by TERM) starts with xterm or
  ansi. This means that some terminals will not have correct special key support (shifted F10 etc)

* Uses the TIOCGWINSZ ioctl to find the current window size, which
  appears to be limited to Linux and BSD.

# Platform Support

## Posix Terminals

Uses terminfo to determine terminal protocol. Some special rules for Mac terminal applications. The
special rules might be invalid on newer Mac OS.

## Windows

None!

# Development Notes

## Under NixOS

After installing ncurses to user env.

~~~
LIBRARY_PATH=$HOME/.nix-profile/lib/ cabal configure --enable-tests --extra-lib-dirs=$HOME/.nix-profile/lib
LIBRARY_PATH=$HOME/.nix-profile/lib/ cabal build
LIBRARY_PATH=$HOME/.nix-profile/lib/ cabal test
~~~

## Coverage

Profiling appears to cause issues with coverage when enabled. To evaluate coverage configure as
follows:

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
