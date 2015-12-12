{- Windows support requires three modes:

1. Using the Console API
2. Using a minimal internal termcap for cygwin/msys
3. Using terminfo compiled for cygwin.

Only the first mode is implemented. This assumes that even running under cygwin the terminal supports
the Console API. This is definite incomplete but sufficient for initial support.

The last mode is not considered here, because that is equivalent to the non-Windows terminfo support.

The other two modes are selected according to the following:

* If CONIN$ and CONOUT$ can both be opened then use Console API
* Otherwise, select built-in termcap table based on TERM
* Otherwise, fatal

The assumption is that using the Console API is always preferable to the built-in termcap table.

References:

* Windows Console API:
  https://msdn.microsoft.com/en-us/library/ms682073.aspx
* msys/cygwin term code translation:
  https://github.com/msysgit/msys/blob/master/winsup/cygwin/fhandler_console.cc
-}
module Graphics.Vty.Windows where
