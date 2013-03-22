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

* Properly handles cleanup.

# Known Issues

* The character encoding of the output terminal is assumed to be UTF-8.

* Minimal support for special keys on terminals other than the
  linux-console.  (F1-5 and arrow keys should work, but anything
  shifted isn't likely to.)

* Uses the TIOCGWINSZ ioctl to find the current window size, which
  appears to be limited to Linux and *BSD.

# Platform Support

## Posix Terminals

Uses terminfo to determine terminal protocol. Some special rules for Mac terminal applications. The
special rules might be invalid on newer Mac OS.

## Windows

None!
