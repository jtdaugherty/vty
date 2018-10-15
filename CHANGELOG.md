
5.25
 - The Vty type got a new field, isShutdown, that returns whether the
   Vty handle has had its 'shutdown' function called (thanks Ian
   Jeffries)
 - Vty's shutdown function is now thread-safe.

5.24.1
 - The "shutdown" method of Vty handles is now idempotent (#159)

5.24
 - Add Generic and NFData instances for some types
 - Image: remove custom Show instance, add derived Show and Read instances
 - Updated Travis build settings (thanks Eric Mertens)

5.23.1
  - Fixed a bug where italics did not combine properly with other
    display modes (#155, thanks Eric Mertens)

5.23
  - Added support for italicized output when terminfo supports it. This
    takes the form of a new Style, "italic". Note that most terminfo
    descriptors do not report capabilities for italics, so support for
    this will be very spotty.
  - Updateed text/string function documentation to indicate that escapes
    are not permitted in their inputs.

5.22
 - Added Graphics.Vty.Attributes.Color240.color240CodeToRGB function
   (thanks Brent Carmer)
 - Added nextEventNonblocking function (field) to Vty type (#87)

5.21
 - Picture and Background now provide Eq instances (thanks Jaro Reinders)
 - #145: vty builds with microlens 0.4.9 (thanks Daniel Wagner)
 - #142: note requirement of threaded RTS

5.20
API changes:
 - Split up Monoid instances into Monoid and Semigroup for newer GHCs
   (thanks Ryan Scott)

5.19.1
API changes:
 * Cursor now provides an Eq instance (thanks Jaro Reinders)

5.19
API changes:
 - URL hyperlinking (via 'withURL') is now optional and disabled by
   default due to poor support on some common terminals. A new 'Mode'
   constructor, 'Hyperlink', has been added to enable this feature. To
   change the hyperlinking mode, use 'setMode' on the 'outputIface' of a
   Vty handle.

5.18.1
Bug fixes:
 - Reset the hyperlink state on line endings to avoid run-on hyperlinks

5.18
API changes:
 - Added support for hyperlinking attributes (thanks Getty Ritter). This
   change adds a new Attr field for containing the hyperlink to apply,
   as per https://gist.github.com/egmontkob/eb114294efbcd5adb1944c9f3cb5feda

5.17.1
 - withStyle now ignores zero arguments, leaving attribute styles
   untouched if the input style is the null style

5.17
API changes:
 - Add support for terminal focus events. This change adds a new mode
   usable with setMode, Focus, that requests that the terminal send
   events on focus lose/gain. This change also adds two new Event
   constructors, EvLostFocus and EvGainedFocus.
 - No longer enable UTF8 mouse event encoding. This encoding was not
   working properly with Terminal.app, and using the other modes (SGR,
   etc.) work.
 - Graphics.Vty.Attributes: escape backticks in Haddock comment (fixes
   #131)

5.16
API changes:
  - Added support for mouse wheel events while in mouse mode. The Button
    type got two new constructors as a result: BScrollUp and BScrollDown.
    Thanks to doublescale@tutanota.com for this contribution!

Bug fixes:
  - charFill now clamps negative arguments to zero (thanks Eric
    Mertens!)

5.15.1
Package changes:
  - Documentation files are now marked accordingly (thanks Michal
    Suchánek)

Bug fixes:
  - translateX/Y: fix negative translations

5.15
Package changes:
  - Discontinued support for GHC versions prior to 7.10.1.
  - Removed instructions and configuration for Stack builds since they
    are no longer supported.
  - Clarified README mention of (lack of) Windows support (contributors
    wanted, though!)
  - Removed dependency on data-default (see below).

API changes:
  - Moved color definitions from Attributes to Color module.
  - In lieu of data-default (Default) instances for Attr and Config, use
    'defAttr' and the new 'defaultConfig' (or 'mempty') instead of 'def'.
  - Graphics.Vty.Output no longer re-exports
    Graphics.Vty.Output.Interface.
  - Removed Graphics.Vty.Prelude module and moved DisplayRegion and its
    accessors to Graphics.Vty.Image.
  - Graphics.Vty.Image no longer re-exports Graphics.Vty.Attributes.
  - Graphics.Vty.Picture no longer re-exports Graphics.Vty.Image.

5.14
  - addMaybeClippedJoin: instead of raising an exception when the join
    is totally clipped, just reduce the clip amount and continue
  - addMaybeClipped: skip blit of joins when their primary dimension is
    zero
  - 'string' and related text functions no longer treat an empty string
    as an empty image (thanks Chris Penner). This means that now it is
    possible to use 'str ""' as a non-empty image with height 1.

5.13
  - Reverted changes in 5.12 due to disagreements between terminal
    emulators and utf8proc; for more details, please see the ticket
    discussion at
    https://github.com/coreyoconnor/vty/issues/115

5.12
  - Replaced 'wcwidth' with a call to the utf8proc library's character
    width function, which is much more up to date (by several Unicode
    versions) and returns the right width for a much larger set of
    characters.
  - Added a bundled version of the utf8proc C library.

5.11.3
  - Fix mouse event offsets in mouse-up events

5.11.2
  - Mouse events were modified so that the upper-left corner of the
    window is (0,0) rather than (1,1).

5.11.1
  - Add Generic instance for Image
  - nextEvent: stop trying to refresh on a resize event (fixes segfault
    on refresh with normal cursor positioning mode)
  - Remove redundant clause from clipForCharWidth (thanks Eric Mertens)
  - Update maintainer

5.11
  - Vty now raises a VtyConfigurationError exception when the TERM
    evironment variable is missing (thanks Eric Mertens)
  - Graphics.Vty.Config got an explicit export list to avoid accidentally
    exporting internal types (thanks Eric Mertens)

5.10
  - Add absolute cursor positioning mode AbsoluteCursor to Cursor. This
    mode provides greater control over cursor positioning by bypassing
    the logical positioning provided by default. Rather than positioning
    the cursor by looking at the widths of characters involved, this
    constructor lets you provide a physical row and column instead. This
    is useful in more sophisticated programs. (thanks Eric Mertens)
  - Added a new Generic-derived config parser (thanks Eric Mertens)
  - Fixed the MShift case in the configuration file parser (thanks Eric
    Mertens)
  - Fixed wcwidth import and matched safeWcswidth to its documented
    behavior. Previously vty_mk_wcwidth was being imported with
    the wrong type causing the -1 return value to be mapped to the
    wrong Int value. Additionally safeWcswidth was using the unsafe
    character width function and only ensuring that the final result was
    non-negative. (thanks Eric Mertens)

5.9.1
  - Vty now only emits UTF8 charset sequences in terminals without a
    preexisting UTF8 declaration to avoid emitting garbage sequences
    (fixes #89)

5.9
  - Added new Output methods supportsBell and ringTerminalBell to find
    out whether the output device has an audio bell and to ring it (see
    #102)

5.8.1
  - Fixed "refresh" to work as advertised (see #104)

5.8
  - API change: EvPaste input event now provides paste data as a raw
    ByteString rather than a String to allow the application to decode
    how best to decode it

5.7.1
  - ModeDemo: added an explicit Control.Applicative import for older
    GHCs

5.7
  - Mouse and paste modes are now off by default.
  - The Config type got new fields: mouseMode and bracketedPasteMode.
    These determine whether these modes are enabled initially (for
    terminals that support them).
  - Added a Mode type for modal terminal features (mouse events,
    bracketed paste mode) that is used with new Output interface
    functions:
    * supportsMode :: Mode -> Bool tells whether the device supports a
      mode
    * setMode :: Mode -> Bool -> IO () turns a mode on or off
    * getModeStatus :: Mode -> IO Bool tells you whether a mode is on or
      off
  - Added a new demo program, ModeDemo.hs, to demonstrate usage of modes

5.6
  - Added support for normal and extended mouse modes in Xterm-like
    terminals via the MouseDown and MouseUp Event constructors
  - Added support for bracketed paste mode in Xterm-like terminals via
    the EvPaste event constructor
  - Added derived Show instances for Event and Button (thanks Felix
    Hirn)
  - Now TERM values containing "screen" will automatically use the
    XtermColor driver rather than just TerminfoBased

5.5.0
  - Replaced lens dependency with microlens, microlens-mtl, microlens-th
    dependencies. Issue #90
    - Thanks Jonathan Daugherty
  - Cabal corrections.
    - Thanks Lennart Spitzner

5.4.0
  - Changed eventChannel of Graphics.Vty.Input from Chan to TChan.
    This enables clients to query if there are no pending events. The
    Graphics.Vty interface nextEvent is unchanged. Clients that use
    eventChannel directly will require updating.
    https://github.com/coreyoconnor/vty/issues/60
5.3.1
  - Reverted cabal file to depend on Cabal >= 1.18 instead of 1.20 due
    to possibly breaking this on reasonable GHC versions

5.3
  - Upgraded QuickCheck dependency to 2.7
  - The standard IO Config (standardIOConfig) was overriding any
    provided application config. In addition, the inputFd and outputFd
    could not be changed if mkVty was used. Fixed.
  - Correct handling of display attributes at end of line. The output
    attributes are set to default at the end of content for the line and
    at the start of a new line. Previously the current attribute would
    extend to the next start of content. This was odd to reason about
    and was the cause of https://github.com/coreyoconnor/vty/issues/76
    IIRC Yi requires the old behavior to display the selection region
    correctly.
  - shutdown of the input thread is now performed using killThread and
    synchronization on an MVar. For correct handling of the terminal
    read vmin and vtime the read must be a blocking read on an OS
    thread. This places a threadWaitRead, which will be interrupted by
    the killThread, prior to the uninterruptable read. An alternative
    would be to re-import the read foreign call as interruptable.

5.2.11
  - deepseq bounds increased for tests.
  - Clean up warnings when compiling on 7.10
    - Thanks Eric Mertens
  - Avoid discarding input bytes after multi-byte encoded codepoint
    - Thanks Eric Mertens

5.2.10
  - "str" now returns EmptyImage for empty strings to match behavior of
    other string-like Image constructors (fixes #74)
    - Thanks Jonathan Daugherty

5.2.9
  - dependency version bumps
    - https://github.com/coreyoconnor/vty/pull/71
    - https://github.com/coreyoconnor/vty/pull/70
  - Correct/Simplify the example code
    - Thanks glguy
    - https://github.com/coreyoconnor/vty/pull/69

5.2.8
  - blaze-builder, lens, utf8-string version constraint bump
      - Thanks glguy
      - https://github.com/coreyoconnor/vty/pull/67
  - Do not differentiate based on TERM_PROGRAM
      - https://github.com/coreyoconnor/vty/issues/68

5.2.7
  - lens and deepseq constraint bump + misc
      - Thanks ethercrow
      - https://github.com/coreyoconnor/vty/pull/66

5.2.6
  - lens constraint bump
      - Thanks alexander-b!
      - https://github.com/coreyoconnor/vty/pull/64

5.2.5
  - lens and random version constraint bump.
      - Thanks RyanGlScott!
      - https://github.com/coreyoconnor/vty/pull/62

5.2.4
  - removed -fpic from cc-options. No longer required.
      - https://github.com/coreyoconnor/vty/issues/61
      - https://ghc.haskell.org/trac/ghc/ticket/9657
      - Thanks Fuuzetsu!

5.2.3
  - evaluate/compile the input parsing table once instead of each
    keystroke.
      - https://github.com/coreyoconnor/vty/pull/59
      - Thanks ethercrow!

5.2.2
  - When looking at input for an event, don't look too deep.
      - https://github.com/coreyoconnor/vty/pull/57
      - Thanks ethercrow!

5.2.1
  - Bump upper version bound for lens to 4.5. Thanks markus1189!

5.2.0
  - Config structure now specifies file descriptor to use. The default
    is stdInput and stdOutput file descriptors. Previously Vty used
    stdInput for input and the follow code for output:
      - hDuplicate stdout >>= handleToFd >>= (`hSetBuffering`
        NoBuffering)
      - the difference was required by Vty.Inline. Now, Vty.Inline uses
        the Config structure options to acheive the same effect.
  - removed: derivedVtime, derivedVmin, inputForCurrentTerminal,
    inputForNameAndIO, outputForCurrentTerminal, outputForNameAndIO
  - added: inputForConfig, outputForConfig
  - updates to vty-rogue from jtdaugherty. Thanks!
  - the oldest version of GHC tested to support vty is 7.6.2.
  - the oldest version of GHC that vty compiles under is 7.4.2

5.1.4
  - merged https://github.com/coreyoconnor/vty/pull/51 thanks trofi!

5.1.1
  - merged https://github.com/coreyoconnor/vty/pull/48 thanks sjmielke!
  - jtdaugherty resolved a number of compiler warnings. Thanks!

5.1.0
  - vmin and vtime can be specified however the application requires.
    See Graphics.Vty.Config.
  - fixed the processing of input when vmin is set > 1.

5.0.0
  - The naming convention now matches:
    - http://www.haskell.org/haskellwiki/Programming_guidelines#Naming_Conventions
  - all projects using vty for input must be compiled with -threaded.
    Please notify vty author if this is not acceptable.
  - mkVtyEscDelay has been removed. Use "mkVty def". Which initialized
    vty with the default configuration.
  - input handling changes
    - KASCII is now KChar
    - KPN5 is now KCenter
    - tests exist.
    - Applications can add to the input tables by setting inputMap of
      the Config. See Graphics.Vty.Config
    - Users can define input table extensions that will apply to all vty
      applications. See Graphics.Vty.Config
    - terminal timing is now handled by selecting an appropriate VTIME.
      Previously this was implemented within Vty itself. This reduced
      complexity in vty but provides a different meta key behavior and
      implies a requirement on -threaded.
    - The time vty will wait to verify an ESC byte means a single ESC
      key is the singleEscPeriod of the Input Config structure.
  - removed the typeclass based terminal and display context interface
    in favor of a data structure of properties interface.
  - renamed the Terminal interface to Output
  - The default picture for an image now uses the "clear" background.
    This background fills background spans with spaces or just ends the
    line.
    - Previously the background defaulted to the space character. This
      causes issues copying text from a text editor. The text would end
      up with extra spaces at the end of the line.
  - Layer support
    - Each layer is an image.
    - The layers for a picture are a list of images.
    - The first image is the top-most layer. The images are ordered from
      top to bottom.
    - The transparent areas for a layer are the backgroundFill areas.
      backgroundFill is added to pad images when images of different
      sizes are joined.
    - If the background is clear there is no background layer.
    - If there is a background character then the bottom layer is the
      background layer.
    - emptyPicture is a Picture with no layers and no cursor
    - addToTop and addToBottom add a layer to the top and bottom of the
      given Picture.
  - compatibility improvements:
    - terminfo based terminals with no cursor support are silently
      accepted. The cursor visibility changes in the Picture will have
      no effect.
    - alternate (setf/setb) color maps supported. Though colors beyond
      the first 8 are just a guess.
    - added "rgbColor" for easy support of RGB specified colors.
    - Both applications and users can add to the mapping used to
      translate from input bytes to events.
  - Additional information about input and output process can be
    appended to a debug log
    - Set environment variable VTY_DEBUG_LOG to path of debug log
    - Or use "debugLog <path>" config directive
    - Or set 'debugLog' property of the Config provided to mkVty.
  - examples moved to vty-examples package. See test directory for cabal
    file.
    - vty-interactive-terminal-test
      - interactive test. Useful for building a bug report for vty's
        author.
      - test/interactive_terminal_test.hs
    - vty-event-echo
      - view a input event log for vty. Example of interacting with
        user.
      - test/EventEcho.hs
    - vty-rogue
      - The start of a rogue-like game. Example of layers and image
        build operations.
      - test/Rogue.hs
    - vty-benchmark
      - benchmarks vty. A series of tests that push random pictures to
        the terminal. The random pictures are generated using
        QuickCheck. The same generators used in the automated tests.
      - test/benchmark.hs

4.7.0.0
  - API changes:
    - Added Graphics.Vty.Image.crop: Ensure an image is no larger
      than the specified size.
    - Added Graphics.Vty.Image.pad: Ensure an image is no smaller
      than the specified size.
    - Added Graphics.Vty.Image.translate: Offset an image.
  - Thanks Ben Boeckel <MathStuf@gmail.com> for these features.

4.3.0.0

4.2.1.0
  - API changes:
    - Attr record accessor fore_color changed to attr_fore_color
    - Attr record accessor back_color changed to attr_back_color
    - Attr record accessor style changed to attr_style
    - Added an "inline" display attribute changing DSL:
      - put_attr_change applies a display attribute change
        immediately to a terminal
      - For instance, can be used to change the display attrbiutes
        of text output via putStrLn and putStr. EX:
        "put_attr_change $ back_color red" will set the background
        color to red.
      - Changes do not apply to a Picture output via output_picture.
      - See Graphics.Vty.Inline
    - Moved all IO actions into any monad an instance of MonadIO

4.0.0.1
  - binding for mk_wcswidth was incorrect. Most platforms just
    magically worked due to coincidence.

4.0.0
  - API changes:
    - "getSize" has been removed. Use "terminal vty >>= display_bounds"
      where "vty" is an instance of the Vty data structure.
    - added a "terminal" field to the Vty data structure. Accesses the
      TerminalHandle associated with the Vty instance.
    - Graphics.Vty.Types has undergone a number of changes. Summary:
      - Partitioned into Graphics.Vty.Attributes for display attributes.
        Graphics.Vty.Image for image combinators. Graphics.Vty.Picture
        for final picture construction.
    - Graphics.Vty.Attributes:
      - "setFG" and "setBG" are now "with_fore_color" and
        "with_back_color"
      - All other "set.." equations similarly replaced.
      - "attr" is now "def_attr", short for "default display attributes"
        Also added a "current_attr" for "currently applied display
        attributes"
    - Graphics.Vty.Image:
      - "horzcat" is now "horiz_cat"
      - "vertcat" is now "vert_cat"
      - "renderBS" is now "utf8_bytestring"
      - "renderChar" is now "char"
      - "renderFill" is now "char_fill"
      - added a "utf8_string" and "string" (AKA "iso_10464_string") for
        UTF-8 encoded Strings and ISO-10464 encoded Strings. String
        literals in GHC have an ISO-10464 runtime representation.
    - Graphics.Vty.Picture:
      - exports Graphics.Vty.Image
      - "pic" is now "pic_for_image"
      - added API for setting background fill pattern.
  - Completely rewritten output backend.
    - Efficient, scanline style output span generator. Has not been
      fully optimized, but good enough.
    - The details required to display the desired picture on a
      terminal are well encapsulated.
    - Terminfo based display terminal implementation. With specialized
      derivitives for xterm, Terminal.app, and iTerm.app.
        - Attempts to robustly handle even terminals that don't
          support all display attributes.
        - I've tested the following terminals with success: iTerm.app,
          Terminal.app, xterm, rxvt, mlterm, Eterm, gnome-terminal,
          konsole, screen, linux vty. Hopefully you will be as
          successfull.
    - Improved unicode support. Double wide characters will display as
      expected.
  - 256 color support. See Graphics.Vty.Attributes.Color240. The actual
    output color is adjusted according to the number of colors the
    terminal supports.
  - The Graphics.Vty.Image combinators no longer require matching
    dimensions to arguments. Unspecified areas are filled in with a
    user-customizable background pattern. See Graphics.Vty.Picture.
  - output images are always cropped to display size.
  - Significant code coverage by QuickCheck tests. An interactive test
    for those final properties that couldn't be automatically verified.

Issues resolved:
  - "gnome terminal displays non-basic attributes as strikethrough"
    - http://trac.haskell.org/vty/ticket/14
  - "Multi-byte characters are not displayed correctly on update"
    - http://trac.haskell.org/vty/ticket/10
  - "Redraw does not handle rendering a line that extends beyond screen
    width characters"
    - http://trac.haskell.org/vty/ticket/13
  - "The <|> and <-> combinators should be more forgiving of mismatched
    dimensions"
    - http://trac.haskell.org/vty/ticket/9
  - "256-color support"
    - http://trac.haskell.org/vty/ticket/19
