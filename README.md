[![Build Status](https://travis-ci.org/jtdaugherty/vty.png)](https://travis-ci.org/jtdaugherty/vty)

`vty` is a terminal interface library. It provides a high-level
interface for doing terminal I/O. Vty is supported on GHC versions
7.10.1 and up.

`vty` and its partner packages are published on
[Hackage](https://hackage.haskell.org/). The `vty` package works in
concert with one or more *platform packages* to do terminal I/O. Each
platform package provides support for terminal I/O on a specific
platform. Known platform packages are:

* [vty-unix](https://github.com/jtdaugherty/vty-unix) - the Unix
  terminal backend for Vty
* [vty-windows](https://github.com/chhackett/vty-windows) - the Windows
  terminal backend for Vty
* [vty-crossplatform](https://github.com/jtdaugherty/vty-crossplatform) -
  a package that builds `vty-unix` or `vty-windows` based on the build
  environment

# How to use Vty

1. Add a package dependency on `vty-unix`, `vty-windows,` or
   `vty-crossplatform`, depending on the desired level of platform
   support. For example, if an application only supports Unix systems,
   it should depend on `vty-unix`. But if an application is intended to
   work anywhere Vty works, then `vty-crossplatform` is the best choice.
2. Add a package dependency on `vty`; the core library abstractions,
   types, and functions are obtained from `vty` itself. The platform
   packages do not re-export the core library's modules.
3. Import `mkVty` from the platform package in step (1) and use that to
   construct a `Vty` handle and initialize the terminal.
4. If desired, call `Graphics.Vty.Config.userConfig` to load the Vty
   user configuration since this step is not automatic.

Once you've initialized the terminal and have a `Vty` value, all of the
`vty` package's API is now ready to use to do terminal I/O.

# Implementing support for a new platform

Although this shouldn't be necessary to do very often (if ever!), if
you would like to implement support for a new platform for Vty, see
`PLATFORM-HOWTO.md`.

# Features

* Supports a large number of terminals, i.e., vt100, ansi, hurd, linux,
  `screen`, etc., or anything with a sufficient terminfo entry.

* Automatically handles window resizes.

* Supports Unicode output on terminals with UTF-8 support.

* Provides an efficient output algorithm. Output buffering and terminal
  state changes are minimized.

* Minimizes repaint area, which virtually eliminates the flicker
  problems that plague ncurses programs.

* Provides a pure, compositional interface for efficiently constructing
  display images.

* Automatically decodes keyboard keys into (key,[modifier]) tuples.

* Automatically supports refresh on Ctrl-L.

* Supports a keypress timeout after for lone ESC. The timeout is
  customizable.

* Provides extensible input and output interfaces.

* Supports ANSI graphics modes (SGR as defined in `console_codes(4)`)
  with a type-safe interface and graceful fallback for terminals
  with limited or nonexistent support for such modes.

* Properly handles cleanup (but not due to signals).

* Provides a comprehensive test suite.

* Supports "normal" and "extended" (SGR) mouse modes as described at
  http://invisible-island.net/xterm/ctlseqs/ctlseqs.html#h2-Mouse-Tracking

* Supports bracketed paste mode as described at
  http://cirw.in/blog/bracketed-paste

* Supports multi-column Unicode characters such as emoji characters. In
  cases where Vty and your terminal emulator disagree on character
  widths, Vty provides a tool `vty-build-width-table` and library
  functionality to build a width table that will work for your terminal
  and load it on application startup.

# Development Notes

Vty uses threads internally, so programs made with Vty need to be
compiled with the threaded runtime using the GHC `-threaded` option.

# Platform Support

## Posix Terminals

Unix-like systems are supported by the `vty-unix` platform package. For
the most part, Vty uses `terminfo` to determine terminal protocol with
some special rules to handle some omissions from `terminfo`.

## Windows

Windows systems are supported by the third-party `vty-windows` platform
package.

# Multi-Column Character Support

Vty supports rendering of multi-column characters such as two-column
Asian characters and Emoji characters. This section details how to
take advantage of this feature, since its behavior will depend on the
terminal emulator in use.

Terminal emulators support Unicode to varying degrees, and each terminal
emulator relies on a table of column widths for each supported Unicode
character. Vty also needs to rely on such a table to compute the width
of Vty images to do image layout. Since those tables can disagree if
Vty and the terminal emulator support different versions of Unicode,
and since different terminal emulators will support different versions
of Unicode, it's likely that for some wide characters, Vty applications
will exhibit rendering problems. Those rendering problems arise from Vty
and the terminal emulator coming to different conclusions about how wide
some characters are.

To address this, Vty supports loading custom character width tables
that are based on the terminal's behavior in order to eliminate these
disagreements. By default, though, Vty will use its built-in Unicode
character width table. Since the built-in table is likely to eventually
disagree with your terminal, Vty provides an API and a command-line tool
to generate and install custom tables.

Custom Unicode width tables based on your terminal emulator can be
built by running Vty's built-in tool, `vty-build-width-table`. The tool
works by querying the current terminal emulator to obtain its width
measurements for the entire supported Unicode range. The
results are then saved to a disk file. These custom tables
can also be generated programmatically by using the API in
`Graphics.Vty.UnicodeWidthTable.Query`.

Saved width tables can then be loaded in one of two ways:

* Via the library API in `Graphics.Vty.UnicodeWidthTable.IO`
* By adding a `widthMap` directive to your Vty configuration file and
  then invoking `mkVty` to initialize Vty

The Vty configuration file supports the `widthMap` directive to allow
users to specify which custom width table should be loaded for a given
terminal type. This is done by specifying, e.g.,

```
widthMap "xterm" "/path/to/map.dat"
```

where the first argument is the value that `TERM` must have in order for
the table to be loaded, and the second argument is the path to the table
file itself as generated by the two alternatives listed above. If the
Vty configuration file contains multiple matching `widthMap` directives
for the current value of `TERM`, the last one listed in the file is
used.

The tables declared in the configuration file are only ever
automatically loaded when applications set up Vty by calling
`Graphics.Vty.mkVty`.

Before a custom table has been loaded, calls to the library's character
width functions (e.g. `wcwidth`) will use the default built-in table.
Once a custom table has been loaded, the functions will use the new
custom table. Only one custom table load can be performed in a Vty
program. Once a custom table has been loaded, it cannot be replaced or
removed.

Without using a custom width table, users of Vty-based applications
are likely to eventually experience rendering problems with with wide
characters. We recommend that developers of Vty-based applications either:

* Provide the `vty-build-width-table` tool and documentation for running
  it and updating the Vty configuration, or
* Have the application invoke the Vty library's table-building
  functionality and load the table at startup without using the Vty
  configuration.

The best option will depend on a number of factors: the user audience,
the amount of risk posed by wide character rendering, the terminal
emulators in use, etc.

# Contributing

If you decide to contribute, that's great! Here are some guidelines you
should consider to make submitting patches easier for all concerned:

 - Please ensure that the examples and test suites build along with the
   library by running `build.sh` in the repository.
 - If you want to take on big things, talk to me first; let's have a
   design/vision discussion before you start coding. Create a GitHub
   issue and we can use that as the place to hash things out.
 - If you make changes, make them consistent with the syntactic
   conventions already used in the codebase.
 - Please provide Haddock documentation for any changes you make.

# Known Issues

* Terminals have numerous quirks and bugs, so mileage may vary. Please
  report issues as you encounter them and provide details on your
  terminal emulator, operating system, etc.

* STOP, TERM and INT signals are not handled.

* The character encoding of the terminal is assumed to be UTF-8 if
  unicode is used.

* Terminfo is assumed to be correct unless there is an override
  configured. Some terminals will not have correct special key support
  (shifted F10 etc). See `Config` for customizing vty's behavior for a
  particular terminal.

* Vty uses the `TIOCGWINSZ` ioctl to find the current window size, which
  appears to be limited to Linux and BSD.

# Further Reading

Good sources of documentation for terminal programming are:

* https://github.com/b4winckler/vim/blob/master/src/term.c
* http://invisible-island.net/xterm/ctlseqs/ctlseqs.html
* http://ulisse.elettra.trieste.it/services/doc/serial/config.html
* http://www.leonerd.org.uk/hacks/hints/xterm-8bit.html
* http://www.unixwiz.net/techtips/termios-vmin-vtime.html
* http://vt100.net/docs/vt100-ug/chapter3.html
